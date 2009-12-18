#!/usr/bin/env python

import sys, os, getopt, site

if sys.platform.startswith('win'):
    _NULFILE = 'NUL'
else:
    _NULFILE = '/dev/null'

if __name__ == '__main__':
    interface = ''
    logfile = None
    serverPort = 0
    webPort = 0
    tmpdir = None
    longvaluesize = None
    serversslcert = None
    serversslkey = None
    pluginPath = None
    installSignalHandlers = 1

    olen = len(sys.path)
    origpath = sys.path[:]
    marker = '/abcdefghijklmnopqrstuvwxyz/THIS_MUST_BE_UNIQUE'
    sys.path.append(marker)

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'i:p:w:t:s:l:c:k:m:x:g:')

        for opt, arg in opts:
            if opt == '-i':
                interface = arg
            elif opt == '-p':
                serverPort = int(arg)
            elif opt == '-w':
                webPort = int(arg)
            elif opt == '-t':
                tmpdir = arg
            elif opt == '-s':
                longvaluesize = int(arg)
            elif opt == '-l':
                logfile = arg
            elif opt == '-c':
                serversslcert = arg
            elif opt == '-k':
                serversslkey = arg
            elif opt == '-m':
                # appends arg to sys.path, and handles any .pth files
                if arg != '':
                    site.addsitedir(arg)
            elif opt == '-x':
                if arg != '':
                    pluginPath = [os.path.expanduser(d) for d in arg.split(os.pathsep)]
                    for d in pluginPath:
                        if not os.path.isdir(d):
                            sys.__stderr__.write('warning: %s does not exist\n' % d)
            elif opt == '-g':
                if len(arg) > 0 and arg.lower() != 'false'[:len(arg)]:
                    installSignalHandlers = 1
                else:
                    installSignalHandlers = 0
            else:
                raise 'internal error: out-of-sync with getopt'
    except getopt.GetoptError, e:
        sys.__stderr__.write('%s\n' % e.msg)
        sys.exit(1)
    except ValueError, e:
        sys.__stderr__.write('option %s requires an integer argument\n' % opt)
        sys.exit(1)
    except KeyboardInterrupt:
        pass
    except:
        ex = sys.exc_info()
        sys.__stderr__.write('%s %s\n' % (ex[0], ex[1]))

    modpath = sys.path[:]

    # addsitedir appends and prepends, so search for where the original
    # last element of sys.path is now
    corrupt = None
    try:
        assert sys.path.count(marker) == 1, 'marker is not in sys.path exactly once'
        i = sys.path.index(marker)
        nlen = len(sys.path)
        if i + 1 < nlen:
            j = i - olen + 1  # insert location
            sys.path[j:j] = sys.path[i+1:]
        sys.path[nlen-1:] = []
        assert sys.path.count(marker) == 0, 'marker was not removed from sys.path'
    except (ValueError, AssertionError), exc:
        corrupt = exc

    if len(args) > 0:
        sys.__stderr__.write('warning: ignoring unused argument(s): %s\n' % \
                ' '.join(args))

    # try to import OpenSSL
    try:
        from OpenSSL import SSL
    except ImportError:
        SSL = None

    # import the networkspace server modules
    try:
        import nwss.config
        from nwss.server import NwsService, NwsWeb
    except ImportError:
        sys.__stderr__.write('error: NetWorkSpaces server is not installed\n')
        sys.exit(1)

    # import the twisted web module
    try:
        from twisted.web import server
    except ImportError:
        # we can live without the web interface
        server = None

    # import modules from twisted core
    try:
        from twisted.internet.protocol import Factory
        from twisted.internet import reactor
        if SSL:
            from twisted.internet import ssl
        from twisted.python import log
    except ImportError:
        sys.__stderr__.write('error: Twisted is not installed\n')
        sys.exit(1)

    # setup logging
    if logfile:
        logf = open(logfile, 'w')
    else:
        logf = open(_NULFILE, 'w')
    log.startLogging(logf)

    # This is will go to the log file
    print 'Path of nwss module:', nwss.__path__
    if corrupt is not None:
        print 'WARNING: sys.path may be corrupt:', corrupt
    print 'Original sys.path [%d]: %s' % (len(origpath), origpath)
    print 'Modified sys.path [%d]: %s' % (len(modpath), modpath)
    print 'Final sys.path [%d]: %s' % (len(sys.path), sys.path)

    # set the values in the config module now that we've processed
    # the options and imported nwss.config
    nwss.config.nwsServerPort = serverPort
    nwss.config.nwsWebPort = webPort
    if tmpdir: nwss.config.nwsTmpDir = tmpdir
    if longvaluesize: nwss.config.nwsLongValueSize = longvaluesize
    if serversslcert: nwss.config.nwsServerSslCert = serversslcert
    if serversslkey:
        nwss.config.nwsServerSslKey = serversslkey
    else:
        # set the default value of the server ssl key if we have a certificate
        if nwss.config.nwsServerSslCert is not None and \
                nwss.config.nwsServerSslCert.count('.') > 0:
            i = nwss.config.nwsServerSslCert.rindex('.')
            nwss.config.nwsServerSslKey = \
                    nwss.config.nwsServerSslCert[0:i] + '.key'
    if pluginPath:
        nwss.config.nwsPluginDirs = pluginPath
    else:
        nwss.config.nwsPluginDirs = [os.getcwd()]

    # start the nws service
    factory = NwsService()
    if nwss.config.nwsServerSslCert is not None and \
            nwss.config.nwsServerSslKey is not None:
        lp = reactor.listenSSL(nwss.config.nwsServerPort, factory,
                DefaultOpenSSLContextFactory(nwss.config.nwsServerSslKey,
                nwss.config.nwsServerSslCert),
                interface=interface)
    else:
        lp = reactor.listenTCP(nwss.config.nwsServerPort, factory,
                interface=interface)

    # write the server port to stdout
    sys.__stdout__.write('%d\n' % lp.getHost().port)

    # start the web interface
    if server:
        wp = reactor.listenTCP(nwss.config.nwsWebPort,
                server.Site(NwsWeb(factory)), interface=interface)
        factory.nwsWebPort = lambda: wp.getHost().port

        # write the web port to stdout
        sys.__stdout__.write('%d\n' % wp.getHost().port)
    else:
        # write a "0" since there is no web port
        sys.__stdout__.write('0\n')

    # write the server pid to stdout
    sys.__stdout__.write('%d\n' % os.getpid())

    # write the interface to stdout
    sys.__stdout__.write('%s\n' % lp.getHost().host)

    # flush and close stdout, which seems to be necessary
    # for R to read our stdout successfully
    sys.__stdout__.flush()
    sys.__stdout__.close()

    # start the reactor
    reactor.run(installSignalHandlers=installSignalHandlers)
