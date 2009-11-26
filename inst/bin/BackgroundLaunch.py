#!/usr/bin/python
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

import sys, os, site, subprocess

def usage(msg=None):
    if msg:
        sys.stderr.write('error: %s\n' % msg)
    sys.stderr.write('usage: %s [-m <path> ] -- <command> [ <args> ...]\n' % \
            sys.argv[0])
    sys.exit(1)

def main(argv):
    try:
        from signal import signal, SIGHUP, SIGINT, SIG_IGN
        signal(SIGHUP, SIG_IGN)
        signal(SIGINT, SIG_IGN)
    except ImportError:
        # presumably we're on Windows: nothing to do
        pass

    try:
        # this should work on posix systems and Windows with Python >= 2.6
        subprocess.Popen(argv, close_fds=True)
    except ValueError:
        try:
            # Python versions before 2.6 don't allow close_fds=True on Windows
            import pywintypes
            import win32process
            try:
                from nws.util import msc_argv2str, which
            except ImportError:
                # nws-python isn't installed, but we just need nwsutil.py
                from nwsutil import msc_argv2str, which

            if not os.path.isabs(argv[0]):
                argv[0] = which(argv[0])[0]

            commandLine = msc_argv2str(argv)
            processSecurityAttributes = None
            threadSecurityAttributes = None
            fInheritHandles = 0
            creationFlags = win32process.CREATE_NO_WINDOW
            environment = None
            currentDirectory = None
            startupInfo = win32process.STARTUPINFO()

            procHandle, threadHandle, procId, threadId = win32process.CreateProcess(
                    argv[0], commandLine,
                    processSecurityAttributes, threadSecurityAttributes,
                    fInheritHandles, creationFlags,
                    environment, currentDirectory,
                    startupInfo)
        except ImportError:
            # presumably we're on Windows using Python < 2.6
            # we'll inherit handles, but this is better than nothing
            subprocess.Popen(argv)

if __name__ == '__main__':
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

    # there has to be at least the command name
    if len(argv) == 0:
        usage('no command specified')

    main(argv)
