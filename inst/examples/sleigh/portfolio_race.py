#!/usr/bin/env python

import os, sys, subprocess
from nws.client import NwsServer

if not os.environ.get('DISPLAY'):
    print >> sys.stderr, 'error: X11 is required.  DISPLAY environment variable not set'
    sys.exit(1)

# create a unique workspace to synchronize with the contestants
# and put its name in the environment
s = NwsServer()
n = s.mktempWs('portfolio_race_%d')
ws = s.openWs(n)
os.environ['PF_WORKSPACE'] = n
os.environ['PF_HOST'] = 'localhost'
os.environ['PF_PORT'] = '8765'

# execute the contestants
argv = ['R', '--vanilla', '--slave']
par = subprocess.Popen(argv, stdin=open('par_contestant.R', 'r'))
seq = subprocess.Popen(argv, stdin=open('seq_contestant.R', 'r'))

# wait for them to be ready, and then start the race
ws.fetch('par')
ws.fetch('seq')
raw_input('hit return to start race: ')
ws.store('go', 'go')

# wait for them to exit
print "click in the windows when complete to close them"
par.wait()
seq.wait()
