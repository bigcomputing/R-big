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

import sys, os, string

_iswin = sys.platform.startswith('win')
_winexts = os.environ.get('PATHEXT', '').split(os.pathsep)
_need_quoting = string.whitespace + '"'

def _ismatch(p):
    return os.path.isfile(p) and os.access(p, os.X_OK)

def _expand(n):
    s = [n]
    # only expand on Windows
    if _iswin:
        # only expand if not extension is specified
        b, e = os.path.splitext(n)
        if not e:
            for e in _winexts:
                s.append(b + e)
    return s

def which(cmd, path=None):
    if os.path.split(cmd)[0]:
        cmd = os.path.abspath(cmd)
        # print "Checking for", cmd
        matches = [x for x in _expand(cmd) if _ismatch(x)]
    else:
        matches = []
        if not path:
            path = os.environ.get('PATH', os.defpath).split(os.pathsep)
            if _iswin: path.insert(0, os.curdir)
        for pelem in path:
            full = os.path.join(os.path.normpath(pelem), cmd)
            # print "Checking for", full
            matches += [x for x in _expand(full) if _ismatch(x)]

    return matches

# This is useful in conjunction with os.spawnv on Windows.
# For example,
#     os.spawnv(os.P_WAIT, argv[0], [msc_quote(a) for a in argv])
# This is only useful on Windows.
def msc_quote(cmd):
    if not [c for c in cmd if c in _need_quoting]:
        return cmd

    q = '"'
    nbs = 0

    for c in cmd:
        if c == '\\':
            q += c
            nbs += 1
        elif c == '"':
            q += (nbs + 1) * '\\' + c
            nbs = 0
        else:
            q += c
            nbs = 0

    q += nbs * '\\' + '"'

    return q

# This is useful in conjunction with win32process.CreateProcess
# and os.system on Windows.
# This is only useful on Windows.
def msc_argv2str(argv):
    return ' '.join([msc_quote(a) for a in argv])

def _qtest(argv):
    print "quoted command string:", msc_argv2str(argv)

def _wtest(argv):
    for p in argv[1:]:
        plist = which(p)
        if not plist:
            print >> sys.stderr, "error: no matches found for", p
            continue
        if len(plist) > 1:
            print >> sys.stderr, "warning: more than one match found for", p
        print plist[0]

if __name__ == '__main__':
    _qtest(sys.argv)
