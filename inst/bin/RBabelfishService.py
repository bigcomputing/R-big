#
# Copyright (c) 2005-2008, REvolution Computing, Inc.
#
# NetWorkSpaces is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA
#

import sys, os, glob
import win32serviceutil, win32service, win32event, win32process, win32api, win32file, pywintypes
try:
    from nws.util import msc_argv2str, which
except ImportError:
    from nwsutil import msc_argv2str, which

_RVERSIONDIR = 'C:\\Program Files\\R'
_RVERSIONPATTERN = os.path.join(_RVERSIONDIR, 'R-*')
_RNAME = 'Rterm.exe'
_BINPATH = ['library', 'nws', 'bin']
_SCRIPTNAME = 'babelfish.R'
_NULFILE = 'NUL'

# babelfish states
_BABELFISH_RUNNING = 100
_BABELFISH_DIED = 101
_BABELFISH_RESTARTED = 102

# timeouts
_DIED_TIMEOUT = 10 * 1000       # milliseconds to wait before restarting
                                # a dead babelfish
_RESTARTED_TIMEOUT = 10 * 1000  # milliseconds to wait before considering a
                                # restarted babelfish to be running

def _getRProg():
    # try to find it using PATH first
    m = which(_RNAME)
    if m:
        return m[0]

    # try to find it using the latest version of R installed on this machine
    rdirs = glob.glob(_RVERSIONPATTERN)
    rdirs.sort()
    rdirs.reverse() # latest version of R first
    rprogs = [os.path.join(d, 'bin', _RNAME) for d in rdirs]
    for rprog in rprogs:
        if os.path.exists(rprog):
            return rprog
    else:
        return None

class RBabelfishService(win32serviceutil.ServiceFramework):
    _svc_name_ = 'RBabelfishService'
    _svc_display_name_ = 'Babelfish Service for R NetWorkSpaces'
    _svc_description_ = 'Translates R objects in NWS variables into ' \
            'human readable form so that the values of variables in a ' \
            'workspace can be displayed in the NWS server\'s web interface.'

    def __init__(self, args):
        win32serviceutil.ServiceFramework.__init__(self, args)
        self.hWaitStop = win32event.CreateEvent(None, 0, 0, None)
        self.applicationName = _getRProg()
        d = os.path.dirname(self.applicationName)
        rdir, bdir = os.path.split(d)
        if bdir.lower() != 'bin':
            # looks like PATH contained something unexpected
            rdir = d
        nwsbindir = os.path.join(rdir, *_BINPATH)
        self.script = os.path.join(nwsbindir, _SCRIPTNAME)

    def SvcStop(self):
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        win32event.SetEvent(self.hWaitStop)

    def SvcDoRun(self):
        import servicemanager
        self.sm = servicemanager
        self.sm.LogMsg(self.sm.EVENTLOG_INFORMATION_TYPE,
                self.sm.PYS_SERVICE_STARTED, (self._svc_name_, ''))

        # sanity checking
        sane = True
        if not self.applicationName:
            self._error("unable to locate a copy of " + _RNAME)
            sane = False
        elif not os.path.exists(self.applicationName):
            self._error("R program not found: " + self.applicationName)
            sane = False

        if not os.path.exists(self.script):
            self._error("error finding babelfish script: " + self.script)
            sane = False

        if sane:
            self.procHandle = self._startBabelfish()
            handles = (self.hWaitStop, self.procHandle)
            timeout = win32event.INFINITE
            state = _BABELFISH_RUNNING

        while sane:            
            # wait for a stop request, a babelfish death, or possibly a timeout
            s = win32event.WaitForMultipleObjects(handles, 0, timeout)

            if s == win32event.WAIT_TIMEOUT:
                if state == _BABELFISH_RESTARTED:
                    self._info("babelfish restarted successfully")
                    timeout = win32event.INFINITE
                    state = _BABELFISH_RUNNING
                elif state == _BABELFISH_DIED:
                    self.procHandle = self._startBabelfish()
                    handles = (self.hWaitStop, self.procHandle)
                    timeout = _RESTARTED_TIMEOUT
                    state = _BABELFISH_RESTARTED
                else:
                    self._error("got an unexpected timeout while in state %d" % state)
                    break
            elif s == win32event.WAIT_OBJECT_0:
                # a shutdown was requested, so kill the babelfish
                # and break out of the while loop
                if self.procHandle:
                    self._info("shutdown requested: terminating babelfish")
                    try:
                        win32process.TerminateProcess(self.procHandle, 0)
                    except:
                        e = sys.exc_info()[1]
                        self._info("caught exception terminating babelfish: %s" % str(e))
                else:
                    self._info("shutdown requested while no babelfish running")
                break
            elif s == win32event.WAIT_OBJECT_0 + 1:
                # the babelfish exited by itself, which probably means
                # that the NWS server shutdown.  we want to reconnect
                # when it comes back up, so sleep awhile, and then
                # start another babelfish.  this will probably happen
                # over and over again, so don't do it too frequently.
                if state == _BABELFISH_RUNNING:
                    self._info("babelfish died: restarting in a bit")

                win32api.CloseHandle(self.procHandle)
                self.procHandle = None
                handles = (self.hWaitStop,)
                timeout = _DIED_TIMEOUT
                state = _BABELFISH_DIED
            else:
                self._error("illegal status from WaitForMultipleObjects: stopping")
                break

        self.sm.LogMsg(self.sm.EVENTLOG_INFORMATION_TYPE,
                self.sm.PYS_SERVICE_STOPPED, (self._svc_name_, ''))

    def _info(self, msg):
        self.sm.LogMsg(self.sm.EVENTLOG_INFORMATION_TYPE, 1, (msg,))

    def _error(self, msg):
        self.sm.LogMsg(self.sm.EVENTLOG_ERROR_TYPE, 1, (msg,))

    def _startBabelfish(self):
        outfile = _NULFILE
        sa = pywintypes.SECURITY_ATTRIBUTES()
        sa.bInheritHandle = 1
        try:
            outh = win32file.CreateFile(outfile,
                    win32file.GENERIC_WRITE, 0, sa, win32file.CREATE_ALWAYS,
                    win32file.FILE_ATTRIBUTE_NORMAL, None)
        except pywintypes.error:
            self._error("error creating " + outfile)
            raise

        try:
            inh = win32file.CreateFile(self.script,
                    win32file.GENERIC_READ, 0, sa, win32file.OPEN_EXISTING,
                    win32file.FILE_ATTRIBUTE_NORMAL, None)
        except pywintypes.error:
            self._error("error opening " + self.script)
            win32api.CloseHandle(outh)
            raise

        argv = [self.applicationName, '--vanilla', '--slave']
        commandLine = msc_argv2str(argv)
        processSecurityAttributes = None
        threadSecurityAttributes = None
        fInheritHandles = 1
        creationFlags = win32process.CREATE_NO_WINDOW
        environment = None
        currentDirectory = None
        startupInfo = win32process.STARTUPINFO()
        startupInfo.dwFlags = win32process.STARTF_USESTDHANDLES
        startupInfo.hStdInput = inh
        startupInfo.hStdOutput = outh
        startupInfo.hStdError = outh

        try:
            procHandle, threadHandle, procId, threadId = win32process.CreateProcess(
                    self.applicationName, commandLine,
                    processSecurityAttributes, threadSecurityAttributes,
                    fInheritHandles, creationFlags,
                    environment, currentDirectory,
                    startupInfo)

            win32api.CloseHandle(threadHandle)
        except pywintypes.error:
            self._error("error executing " + self.applicationName)
            win32api.CloseHandle(outh)
            win32api.CloseHandle(inh)
            raise

        win32api.CloseHandle(outh)
        win32api.CloseHandle(inh)

        return procHandle

if __name__ == '__main__':
    win32serviceutil.HandleCommandLine(RBabelfishService)
