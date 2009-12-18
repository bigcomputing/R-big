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

SERVER.INFO.ARGS <- c('host','port')
MANAGED.SERVER.INFO.ARGS <- c('host','port','webPort','quiet',
                              'pluginPath', 'logFile')

defaultServerInfoOptions <- new.env()
defaultManagedServerInfoOptions <- new.env()
defaultServerOptions <- new.env()

.serverGlobals <- new.env()

computeDefaultServerInfoOptions <- function(...)
  list(host=Sys.info()[['nodename']], port=8765)

# The host argument currently specifies the interface to bind to,
# where '' means to bind to all interfaces
computeDefaultManagedServerInfoOptions <- function(...) {
  list(host='', port=0, webPort=0, quiet=FALSE,
       pluginPath=.serverGlobals$pluginPath, logFile='')
}

setClass('serverInfo',
         representation(host='ANY', port='ANY'))
setMethod('initialize', 'serverInfo',
          function(.Object, ...) {
            call <- match.call()
            check.opts(list(...), SERVER.INFO.ARGS, call)
            options <- new.env()
            blendOptions(options,
                         as.list(defaultServerInfoOptions))
            blendOptions(options, list(...))
            .Object@host=options$host
            .Object@port=options$port

            .Object
          })

showServerInfo <- function(object) {
  cat('\n')
  cat('Server host:\t', object@host,'\n')
  cat('Server port:\t', object@port,'\n')
  cat('\n')
}
setMethod('show', 'serverInfo', showServerInfo)

setClass('managedServerInfo',
         contains='serverInfo',
         representation(webPort='numeric', quiet='logical', locServ='list',
                        pluginPath='character', logFile='character',
                        state='environment'))
setMethod('initialize', 'managedServerInfo',
          function(.Object, ...) {
            call <- match.call()
            check.opts(list(...), MANAGED.SERVER.INFO.ARGS, call)
            options <- new.env()
            blendOptions(options,
                         as.list(defaultManagedServerInfoOptions))
            blendOptions(options, list(...))

            # start initializing .Object
            .Object@quiet <- options$quiet
            .Object@pluginPath <- options$pluginPath
            .Object@logFile <- options$logFile

            verbose <- nzchar(Sys.getenv('NWS_VERBOSE'))

            # launch server
            .Object@locServ <- startNwsServer(logfile=.Object@logFile,
                                              verbose=verbose,
                                              host=options$host,
                                              port=options$port,
                                              webPort=options$webPort,
                                              pluginpath=.Object@pluginPath)

            # Write back values
            .Object@port <- .Object@locServ$serverPort
            .Object@webPort <- .Object@locServ$webPort
            .Object@host <-
              if (identical(.Object@locServ$hostName, '0.0.0.0')) {
                # wild card must be handled specially
                if (!identical(options$host, '') && verbose)
                  cat(sprintf(
                      'nws server bound to 0.0.0.0 when interface was %s',
                      options$host))
                Sys.info()[['nodename']]
              } else {
                .Object@locServ$hostName
              }

            # Set state, true = running
            .Object@state <- new.env()
            .Object@state$running <- TRUE

            connCode <- checkServerConnection(.Object)
            if (connCode == 0) {
              if (! options$quiet) {
                msg <- sprintf('Started nws server on port %d with web interface on port %d\n',
                               .Object@port, .Object@webPort)
                cat(msg)
              }
            }
            else if (connCode == 1) {
              mess <- sprintf('Cannot connect to host %s, please define host externally',
                           .Object@host)
              if (options$host=='') {
                stop(mess, call.=FALSE)
              }
              else {
                warning(mess, call.=FALSE)
              }
            }
            else if (connCode == 2) {
              warning(sprintf("Cannot connect to server on host: %s and port: %d, \n%s",
                              .Object@host, .Object@port,
                              "Please check network and firewall settings"),
                      call.=FALSE)
            }
            else {
              warning("Unexpected status returned from checkServerConnection",
                      call.=FALSE)
            }
            
            .Object
          })

showManagedServerInfo <- function(object) {
  cat('\n')
  cat('This is a managed server.\n')
  cat('Server host:\t\t', object@host,'\n')
  cat('Server port:\t\t', object@port,'\n')
  cat('Server web port:\t', object@webPort,'\n')
  if(!nwsIsRunning(object))
    cat('\nThis server has been stopped\n')
  cat('\n')
}
setMethod('show', 'managedServerInfo', showManagedServerInfo)

if (!isGeneric('view'))
  setGeneric('view', function(.Object, ...) standardGeneric('view'))
setMethod('view', 'managedServerInfo', function(.Object, ...) {
  host <- nwsHost(.Object)
  port <- nwsWebPort(.Object)
  vstring <- sprintf('http://%s:%d/',
                     host, port)
  browseURL(vstring)
  cat(sprintf("Viewing server '%s:%d' in your browser\n",
              host, port))
  invisible(vstring)
})

setGeneric('nwsServerInfoStop', function(.Object, ...) standardGeneric('nwsServerInfoStop'))
setMethod('nwsServerInfoStop','managedServerInfo',
          function(.Object, ...) {
            # just closing the deadman connection should shutdown the server,
            # although if the user has started another long running process
            # probably won't work...
            # We should eventually do more thorough sanity checking here
            if (!nwsIsRunning(.Object))
              warning('this server has already been stopped')
            else {
              try(close(.Object@locServ$deadman))
              .Object@state$running = FALSE
            }

            invisible(NULL)
          })


setGeneric('nwsPort', function(.Object,...) standardGeneric('nwsPort'))
setMethod('nwsPort', 'serverInfo',
          function(.Object, ...) .Object@port)

setGeneric('nwsHost', function(.Object,...) standardGeneric('nwsHost'))
setMethod('nwsHost', 'serverInfo',
          function(.Object, ...) .Object@host)

setGeneric('nwsIsRunning', function(.Object,...) standardGeneric('nwsIsRunning'))
setMethod('nwsIsRunning', 'managedServerInfo',
          function(.Object, ...) .Object@state$running)

if (!isGeneric('nwsWebPort'))
  setGeneric('nwsWebPort', function(.Object,...) standardGeneric('nwsWebPort'))
setMethod('nwsWebPort', 'managedServerInfo',
          function(.Object, ...) .Object@webPort)


serverInfo <- function(...)
  new("serverInfo", ...)

managedServerInfo <- function(...)
  new("managedServerInfo", ...)

startServer <- function(pkgpath, logfile, verbose, python, pythonOpts,
                        pythonpath, pluginPath, host, port, webPort) {
  serverScript <- file.path(pkgpath, 'bin', 'localserver.py')
  logging <- if (!is.null(logfile) && nzchar(logfile[[1]]))
      c('-l', logfile[[1]])
    else
      character(0)
  plugins <- if (!is.null(pluginPath) && any(nzchar(pluginPath)))
      c('-x', paste(pluginPath[nzchar(pluginPath)], collapse=.Platform$path.sep))
    else
      character(0)

  quoting <- NULL  # use the default quoting for this platform
  cmd <- argv2str(c(python, pythonOpts, serverScript,
                    logging,
                    '-m', pythonpath,
                    '-p', as.character(port),
                    '-i', as.character(host),
                    '-w', as.character(webPort),
                    '-g', 'False',
                    plugins),
                  quoting)

  if (verbose)
    cat(sprintf('executing command to start nws server: %s\n', cmd))

  serverPipe <- pipe(cmd, 'r')
  d <- readLines(serverPipe, n=4)

  if (length(d) == 0) {
    try(close(serverPipe))
    stop('unable to read any output from local nws server')
  }
  else if (length(d) < 3) {
    try(close(serverPipe))
    stop('unable to read all of the nws server info')
  }
  else {
    # These are doubles for consistency only
    serverPort <- as.double(d[1])
    webPort <- as.double(d[2])
    serverPid <- as.integer(d[3])
    hostName <- as.character(d[4])
    if (is.na(serverPort) || is.na(webPort) || is.na(serverPid)) {
      try(close(serverPipe))
      stop('bad output format from starting nws server')
    }
    else {
      list(serverPort=serverPort, webPort=webPort,
          serverPid=serverPid, serverPipe=serverPipe, hostName=hostName)
    }
  }
}

makeDeadman <- function(host, port, verbose=FALSE) {
  # create a "deadman" connection, shake hands with the server,
  # and never read or write to the deadman connection again
  if (.Platform$OS.type == 'windows') {
    # on windows, socketConnection will wait for the full timeout,
    # even if no one is listening on the specified server port.
    # make.socket doesn't, so we'll use it to throw an exception
    # if no one is listening.
    tmpsock <-
      tryCatch({
        suppressWarnings(make.socket(host, port))
      },
      error=function(e) {
        msg <- sprintf(
            "unable to connect to NetWorkSpaces server on '%s:%d'",
            host, port)
        warning(msg, call.=FALSE)
        stop(e$message, call.=FALSE)
      })

    close.socket(tmpsock)
  }

  # NB: I set the timeout to be 30 days, mostly to avoid making the
  # timeout short for all other socket connections in this R session
  one.month <- 30 * 24 * 60 * 60
  orig.timeout <- options(timeout=one.month)
  on.exit(options(orig.timeout))
  con <- socketConnection(host, port=port, open='a+b', blocking=TRUE)
  negotiate.deadman(con, verbose)
  con
}

startBabelfish <- function(pkgpath, host, port, verbose) {
  rname <- if (.Platform$OS.type == 'windows') 'Rterm' else 'R'
  rprog <- file.path(R.home(), 'bin', rname)
  babelfishScript <- file.path(pkgpath, 'bin', 'babelfish.R')
  quoting <- NULL  # use the default quoting for this platform
  cmd <- argv2str(c(rprog, '--vanilla', '--slave', '-f', babelfishScript,
                    '--args', '-h', host, '-p', port), quoting)

  if (verbose)
    cat(sprintf('starting the R babelFish: %s\n', cmd))
  ignore.stderr <- ! verbose
  system(cmd, intern=FALSE, ignore.stderr=ignore.stderr, wait=FALSE)
}

# This should be called when nws is loaded with the path of
# the nws installation
initServerEnv <- function(pkgpath) {
  .serverGlobals$pkgpath <- pkgpath

  p <- pythonpath()  # lots of magic here
  if (!is.null(p)) {
    python <- p$pypath
    if (!is.null(p$nspath) && !is.na(p$nspath[1]) && nzchar(p$nspath[1])) {
      pythonPath <- p$nspath[1]
      pluginPath <- file.path(dirname(pythonPath), 'plugins')
      if (nzchar(Sys.getenv('NWS_VERBOSE'))) {
        cat(sprintf('pythonpath function returned nspath: %s\n', pythonPath))
        cat(sprintf('pythonpath function returned pluginPath: %s\n', pluginPath))
      }
      if (!file.exists(pluginPath)) {
        warnings('no plugins directory in your nwsserver installation')
        pluginPath <- ''
      }
    }
    else {
      if (nzchar(Sys.getenv('NWS_VERBOSE')))
        cat(sprintf('pythonpath function returned bad nspath: pluginPath will be empty string\n'))
      pythonPath <- NULL
      pluginPath <- ''
    }
  }
  else {
    if (nzchar(Sys.getenv('NWS_VERBOSE')))
      cat(sprintf('pythonpath function returned NULL: pluginPath will be empty string\n'))
    python <- which.cmd('python')
    if (is.null(python)) {
      python <- which.python()
    }
    pythonPath <- NULL
    pluginPath <- ''
  }

  .serverGlobals$python <- if (is.null(python)) 'python' else python
  .serverGlobals$pythonPath <- pythonPath
  .serverGlobals$pluginPath <- pluginPath

  if (is.null(python)) {
    # nothing more to do if we can't find a Python interpreter
    if (!nzchar(Sys.getenv('NWS_QUIET'))) {
      warning('python command is not in your PATH ',
              'which is needed to start the nws server')
      if (.Platform$OS.type == 'windows')
        warning('Python must be installed on your machine to create a sleigh')
    }
  }
  else {
    if (.Platform$OS.type == 'windows') {
      # add directory containing python executable if it exists,
      # which makes this directory a good place to put DLLs
      pydir <- dirname(python)  # returns '.' if unqualified
      addPath <- if (pydir != '.' && file.exists(pydir)) {
        if (nzchar(Sys.getenv('NWS_VERBOSE')))
          cat(sprintf('Adding %s to PATH\n', pydir))
        pydir
      } else {
        character(0)
      }

      if (!is.null(pythonPath)) {
        # add <pythonPath>/pywin32_system32 to PATH on Windows.
        # this is done to try to improve the chances that pywin32
        # will be able to find pywintypesXX.dll.
        pywintypes <- file.path(pythonPath, 'pywin32_system32')
        if (!file.exists(pywintypes)) {
          if (nzchar(Sys.getenv('NWS_VERBOSE')))
            cat(sprintf('no pywin32_system32 directory in nwsserver: %s\n',
                        pywintypes))
        } else {
          if (nzchar(Sys.getenv('NWS_VERBOSE')))
            cat(sprintf('Adding %s to PATH\n', pywintypes))
          addPath <- c(addPath, pywintypes)
        }
      }

      # check if there's anything to add to PATH
      if (length(addPath) > 0) {
        addPath <- paste(addPath, collapse=.Platform$path.sep)
        path <- Sys.getenv('PATH')
        if (nzchar(path)) {
          path <- paste(path, addPath, sep=.Platform$path.sep)
          if (nzchar(Sys.getenv('NWS_VERBOSE')))
            cat(sprintf('Setting new PATH to %s\n', path))
          Sys.setenv(PATH=path)
        } else {
          warning('PATH is empty: not updating', call.=FALSE)
        }
      }
    }
  }
}

startNwsServer <- function(logfile=NULL, verbose=FALSE,
        python=defaultSleighOptions$python, # Should this dependency exist?
        pythonOpts=defaultSleighOptions$pythonOpts,
        pythonpath=defaultSleighOptions$extraPythonModules,
        host='', port=0, webPort=0, pluginpath='') {

  pkgpath <- .serverGlobals$pkgpath
  if (is.null(pythonpath)) pythonpath <- ""
  obj <- startServer(pkgpath, logfile, verbose, python, pythonOpts,
                     pythonpath, pluginpath, host, port, webPort)

  # we think that '127.0.0.1' is the safest value to use for the
  # babelfish and the dead man connection if the server is bound
  # to all network interfaces
  if (identical(host, '')) {
    serverHost <- '127.0.0.1'
    if (verbose && !identical(obj$hostName, '0.0.0.0'))
      # this is unexpected, but we're not sure it's an error
      cat(sprintf('nws server has bound to address %s\n', obj$hostName))
  } else {
    serverHost <- obj$hostName
  }

  # must start the babelfish before creating the deadman connection
  startBabelfish(pkgpath, serverHost, obj$serverPort, verbose)
  con <- makeDeadman(serverHost, obj$serverPort, verbose)
  if (is.null(con))
    warning('unable to create deadman connection')

  obj$deadman <- con

  obj
}

# the two functions below are modeled after those in the sleighMan package
# and should mirror them closely. Update when updating sleighMan!
setServer <- function(serverInfo, ...) {
  if (!is(serverInfo, 'serverInfo'))
    stop('you must pass a serverInfo object to setServer')

  if (is(.nwsGlobals$nwsserver, 'managedServerInfo') &&
      nwsIsRunning(.nwsGlobals$nwsserver))
    nwsServerInfoStop(.nwsGlobals$nwsserver)

  .nwsGlobals$nwsserver <- serverInfo
}

getServer <- function(..., create=FALSE) {
  call <- match.call()
  if (is.null(.nwsGlobals$nwsserver) || create)
    tryCatch({
      .nwsGlobals$nwsserver <- managedServerInfo()
    },
    error = function(e) {
      msg <- sprintf('error creating managedServerInfo object: %s',
                     conditionMessage(e))
      e <- simpleError(msg, call)
      stop(e)
    })

  # We should eventually do more thorough sanity checking here
  if (is(.nwsGlobals$nwsserver, 'managedServerInfo') &&
      !nwsIsRunning(.nwsGlobals$nwsserver)) {
    warning(paste('The stored server has been stopped, creating another.',
                  ' Previously created netWorkSpace, sleigh, and nwsServer objects will no longer work.'))
    .nwsGlobals$nwsserver <- NULL
    return(getServer())
  }
  else
    .nwsGlobals$nwsserver
}

# function has return codes:
#   0 - nwsServer connection established
#   1 - hostname not defined
#   2 - Connection failed, reason unknown
checkServerConnection <- function(serv) {
  verbose <- nzchar(Sys.getenv('NWS_VERBOSE'))
  tryCatch({
    suppressWarnings(tempServ <- nwsServer(serverInfo=serv))
    close(tempServ)
    0
  }, error=function(e) {
    if(exists("nsl")) {
      tryCatch({
        if(is.null(utils::nsl(nwsHost(serv)))) {
          return(1)
        }
      }, error=function(e) {
        if(verbose) {
          cat(paste("Is nsl defined outside utils?",
                    "\nError message: ",e,'\n'))
        }
      })
    }
    return(2)
  })
}
