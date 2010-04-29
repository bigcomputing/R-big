#!/usr/bin/env python
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

import sys, os, tempfile, traceback, site
from os import environ as Env

if sys.platform.startswith('win'):
    _TMPDIR = tempfile.gettempdir() or '\\TEMP'
    _NULFILE = 'NUL'
else:
    # this can be a very weird directory on Mac OS X
    # _TMPDIR = tempfile.gettempdir() or '/tmp'
    _TMPDIR = '/tmp'
    _NULFILE = '/dev/null'

try:
    try:
        from nws.client import NetWorkSpace
    except ImportError:
        from nwsclient import NetWorkSpace
except ImportError:
    NetWorkSpace = None

# this function is called by the worker process via the preexec_fn parameter
def setpg():
    try: os.setpgid(0, 0)
    except: pass

def main():
    # initialize variables from environment
    modulePath = Env.get('PythonSleighModulePath', '')
    logDir = Env.get('RSleighLogDir')
    if not logDir or not os.path.isdir(logDir):
        logDir = _TMPDIR
    verbose = Env.has_key('RSleighWorkerOut') and 'TRUE' or 'FALSE'
    nwsName = Env['RSleighNwsName']
    nwsHost = Env.get('RSleighNwsHost', 'localhost')
    nwsPort = int(Env.get('RSleighNwsPort', '8765'))
    print nwsName, nwsHost, nwsPort

    # create the script file for the worker to execute
    script = '''\
# use RSleighScriptDir to add the directory that contains
# the nws package to the library path if possible
# (and without modifying the worker's global environment)
local({
    scriptDir <- Sys.getenv('RSleighScriptDir')
    if (basename(scriptDir) == 'bin') {
        nwsDir <- dirname(scriptDir)
        nwsPkg <- basename(nwsDir)
        if (nwsPkg == 'Rbig') {
            libDir <- dirname(nwsDir)
            oldPaths <- .libPaths()
            newPaths <- c(libDir, oldPaths)
            cat("setting library paths to", newPaths, "\n")
            .libPaths(newPaths)
        }
    }
})

library(Rbig)
cmdLaunch(%s)
''' % verbose
    fd, tmpname = tempfile.mkstemp(suffix='.R', prefix='__nws',
                                   dir=_TMPDIR, text=True)
    tmpfile = os.fdopen(fd, 'w')
    tmpfile.write(script)
    tmpfile.close()

    print "executing R worker(s)"
    rprog = Env.get('RProg', 'R')
    argv = [rprog, '--vanilla', '--slave']

    if NetWorkSpace:
        nws = NetWorkSpace(nwsName, nwsHost, nwsPort, useUse=True, create=False)
        newProtocol = nws.findTry('version') is not None
    else:
        nws = None
        newProtocol = False

    numProcs = int(Env.get('RSleighNumProcs', '1'))
    workers = []
    sleighID = int(Env['RSleighID'])  # only used for non-sleigh plugin
    for i in xrange(numProcs):
        outfile = Env.get('RSleighWorkerOut')
        if outfile:
            prefix = 'sleigh_ride_%d_' % i
            fd, outfile = tempfile.mkstemp(suffix='.txt', prefix=prefix,
                                           dir=logDir, text=True)
            out = os.fdopen(fd, 'w')
        else:
            outfile = _NULFILE
            out = open(outfile, 'w')

        Env['RSleighLogFile'] = outfile

        if newProtocol:
            worker_id = nws.fetch('worker_ids')
        else:
            worker_id = str(i + sleighID)

        Env['RSleighRank'] = worker_id
        Env['RSleighLocalID'] = str(i)

        try:
            if sys.platform.startswith('win'):
                p = subprocess.Popen(argv, stdin=open(tmpname), stdout=out,
                        stderr=subprocess.STDOUT)
                wpid = int(p._handle)
            else:
                p = subprocess.Popen(argv, stdin=open(tmpname), stdout=out,
                        stderr=subprocess.STDOUT, preexec_fn=setpg)
                wpid = p.pid

                # attempt to make the child process a process group leader
                try: os.setpgid(wpid, 0)
                except: pass
        except OSError, e:
            print >> sys.stderr, 'error executing command:', argv
            if not os.path.exists(rprog):
                print >> sys.stderr, rprog, 'does not exist'
            raise e

        print "started worker %s, pid: %s" % (worker_id, wpid)
        workers.append((p, wpid, worker_id))

    print "done starting R worker(s)"
    sys.path[1:1] = modulePath.split(os.pathsep)

    if nws:
        sentinel(nws, newProtocol, workers)

        print "killing R worker(s)"
        for p, wpid, worker_id in workers:
            kill(wpid)
    else:
        # nws-python isn't installed, so just wait for the worker to exit
        print >> sys.stderr, 'nws-python not available'
        for p, wpid, worker_id in workers:
            p.wait()

    print "all done"

    # XXX might need to wait for child to die on Windows
    try: os.remove(tmpname)
    except: pass

    # just being paranoid
    sys.stdout.flush()
    sys.stderr.flush()

def getenv(args):
    e = {}
    for kv in args:
        try:
            k, v = [x.strip() for x in kv.split('=', 1)]
            if k: e[k] = v
        except:
            print "warning: bad argument:", kv
    return e

def setup(f):
    if os.path.isabs(f):
        log = f
    else:
        log = os.path.join(Env.get('RSleighLogDir', _TMPDIR), f)

    # open the output file and redirect stdout and stderr
    try:
        # create an unbuffered file for writing
        outfile = open(log, 'w', 0)
        sys.stdout = outfile
        sys.stderr = outfile
    except:
        traceback.print_exc()
        print "warning: unable to create file:", log

    # cd to the specified directory
    wd = Env.get('RSleighWorkingDir')
    if not wd or not os.path.isdir(wd):
        wd = _TMPDIR

    try:
        os.chdir(wd)
    except:
        traceback.print_exc()
        print "warning: unable to cd to", wd

    # this information will normally seem rather obvious
    print "current working directory:", os.getcwd()

def sentinel(nws, newProtocol, workers):
    numWorkers = len(workers)
    try:
        if newProtocol:
            while len(workers) > 0:
                print "blocking on 'sentinel_alive' variable"
                r = nws.fetch('sentinel_alive')
                if r == '1':
                    # the workspace says it time to shutdown
                    print "got a poison pill from the sleigh workspace"
                    break
                elif r != '0':
                    print "got unexpected value from 'sentinel_alive' variable:", r

                print "checking 'Sleigh ride over' variable"
                r = nws.findTry('Sleigh ride over')
                if r is not None:
                    # the master says it time to shutdown
                    print "got a poison pill from the sleigh master"
                    break

                # check if any workers have died
                # warning: we may delete dead workers from "workers"
                # as we iterate through it
                n = len(workers)
                j = 0  # index into "workers"
                print "checking for dead workers; remaining count: %d" % n
                for i in xrange(n):
                    p, wpid, worker_id = workers[j]
                    print "testing %s" % worker_id
                    rc = p.poll()
                    if rc is not None:
                        # report the death to the sleigh workspace
                        print "worker %s has died [%s]" % (worker_id, wpid)
                        nws.store('worker_dead', worker_id)
                        workers.pop(j)
                    else:
                        # we only increment j if the worker is still alive
                        j += 1

                # sanity check
                assert len(workers) == j <= n <= numWorkers
        else:
            print "waiting for 'Sleigh ride over' value"
            nws.find('Sleigh ride over')

        bye = 'Sleigh ride over'
    except Exception, e:
        bye = str(e)
        print "caught exception:", bye

    try:
        print "storing 'bye' value(s)"
        for i in xrange(numWorkers):
            nws.store('bye', bye)
    except:
        pass

    print "sentinel returning"

def kill(pid):
    try:
        try:
            import win32api
            # the "pid" is really a handle on Windows
            win32api.TerminateProcess(pid, -1)
            win32api.CloseHandle(pid)  # XXX not sure about this
        except ImportError:
            try:
                import ctypes
                ctypes.windll.kernel32.TerminateProcess(pid, -1)
            except (ImportError, AttributeError):
                try:
                    from signal import SIGTERM as sig
                except ImportError:
                    print "couldn't import signal module"
                    sig = 15
                try: os.kill(-pid, sig)
                except: os.kill(pid, sig)
    except OSError:
        # process is already dead, so ignore it
        pass
    except:
        traceback.print_exc()

if __name__ == '__main__':
    try:
        olen = len(sys.path)
        i = 1
        while i < len(sys.argv):
            if sys.argv[i] == '-m':
                i += 1
                if i == len(sys.argv):
                    usage('the -m option takes a required argument')
                # usually needed on Windows to find win32process module
                site.addsitedir(sys.argv[i])
            elif sys.argv[i] == '--':
                # we're explicitly finished parsing arguments
                i += 1
                break
            elif sys.argv[i].startswith('-'):
                usage('unrecognized argument: %s', sys.argv[i])
            else:
                # we're implicitly finished parsing arguments
                break
            i += 1

        # addsitedir appends, but we want them at position 1
        nlen = len(sys.path)
        if nlen > olen:
            sys.path[1:1] = sys.path[olen:]
            sys.path[nlen:] = []

        argv = sys.argv[i:]

        # treat any remaining arguments as environment settings
        Env.update(getenv(argv))

        # now that the -m option has been processed, it should
        # be safe to import subprocess
        import subprocess

        # user name is used in log file name to avoid permission problems
        # I'm being a bit paranoid here
        try:
            import getpass
            user = getpass.getuser()
        except:
            user = 'nwsuser'
        f = 'RSleighSentinelLog_' + user + '_' + \
                Env.get('RSleighID', 'X') + '.txt'
        setup(f)

        if not Env.has_key('RSleighNwsName'):
            print "RSleighNwsName variable is not set"
            print >> sys.__stderr__, "RSleighNwsName variable is not set"
        else:
            main()
    except:
        traceback.print_exc()
