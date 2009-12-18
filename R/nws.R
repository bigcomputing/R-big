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

defaultNwsServerOptions <- new.env()
defaultNetWorkSpaceOptions <- new.env()

# this is a manifest constant, how to approriately handle?
nwsRFP = 3*2^24

NegotiationHandshakeInit    = charToRaw('X000')
NegotiationHandshakePropose = charToRaw('P000')
NegotiationHandshakeRequest = charToRaw('R000')
NegotiationHandshakeAccept  = charToRaw('A000')
CookieHandshake             = charToRaw('2223')
NoCookieHandshake           = charToRaw('2222')

OPT.DEADMAN = 'KillServerOnClose'
OPT.METADATATOSERVER = 'MetadataToServer'
OPT.METADATAFROMSERVER = 'MetadataFromServer'

# a utility function to read exactly n bytes from the socket connection.
nwsRecvN <- function(s, n, rawflag=FALSE) {
  if (n > 0) {
    start = proc.time()[[3]]
    repeat {
      b = tryCatch({
          readBin(s, what='raw', n=n)
        }, error=function(e) {
          # readBin sometimes generates confusing exceptions
          stop("error calling readBin on nws socket: ", e$message, call.=FALSE)
        })
      m = length(b)
      current = proc.time()[[3]]
      if (m > 0 || current - start < 30) break
      start = current
    }
    n = n - m
    if (n <= 0) return(if (rawflag) b else rawToChar(b))
    if (m == 0) stop("failed to read from nws socket", call.=FALSE)

    # we didn't get all of the data, so save the raw vector in a list
    # that we'll concatenate when we do have it all
    rlen = 50
    r = vector('list', rlen)
    i = 1
    r[i] = list(b)

    repeat {
      b = readBin(s, what='raw', n=n)
      i = i + 1
      if (i > rlen) {
        # we ran out of space in our list, so double its length
        rlen = 2 * rlen
        length(r) = rlen
      }
      r[i] = list(b)
      m = length(b)
      n = n - m
      if (n <= 0) break
      if (m == 0)
        stop("failed to read subsequent data from nws socket", call.=FALSE)
    }

    # truncate the list, concatenate the raw vectors,
    # and convert to a single character string
    length(r) = i
    return(if (rawflag) do.call(c, r) else rawToChar(do.call(c, r)))
  }
  else {
    return(if (rawflag) raw(0) else '')
  }
}

check.logical <- function(val, defVal, argName, call) {
  if (is.null(val) || is.na(val)) {
    val <- defVal
  } else if (!is.logical(val)) {
    msg <- sprintf("'%s' takes a logical argument", argName)
    e <- simpleError(msg, call)
    stop(e)
  }
  val
}

check.list <- function(val, defVal, argName, call) {
  if (is.null(val) || is.na(val)) {
    val <- defVal
  } else if (!is.list(val)) {
    msg <- sprintf("'%s' takes a list argument", argName)
    e <- simpleError(msg, call)
    stop(e)
  }
  val
}

check.opts <- function(opts, nms, call) {
  n <- names(opts)
  if (is.null(n)) {
    # there are no named arguments in opts, but if any unnamed arguments
    # are present, check if they are allowed
    if (length(opts) > 0 && !('' %in% nms)) {
      msg <- 'illegal unnamed argument specified'
      e <- simpleError(msg, call)
      stop(e)
    }
  } else {
    unrecog <- n[!n %in% nms]
    if (length(unrecog) > 0) {
      msg <- paste('unused argument(s): ', paste(unrecog, collapse=', '))
      e <- simpleError(msg, call)
      stop(e)
    }
  }
}

nwsServer <- function(...) {
  new("nwsServer", ...)
}

computeDefaultNwsServerOptions <- function(...)
  list(serverInfo=NULL, connopts=NULL)

# class respresenting connection to a netWorkSpace server.
# there are extra things here that should probably be removed
setClass('nwsServer', representation(nwsSocket='ANY',
         connopts='list', options='environment',
                                     cookieProtocol='logical'))

setMethod('initialize', 'nwsServer',
          function(.Object, serverHost=NULL, port=NULL, ...) {
            argList <- list(...)

            .Object@options = new.env()
            blendOptions(.Object@options, as.list(defaultNwsServerOptions))
            blendOptions(.Object@options, argList)
            # The next two lines are an explicit traversal that will be removed
            # in 3.0
            if (!is.null(serverHost)) .Object@options$serverHost = serverHost
            if (!is.null(port)) .Object@options$port = port

            if (is.null(.Object@options$serverInfo)) {
              if (!is.null(.Object@options$serverHost))
                warning('use of the serverHost parameter is deprecated, use serverInfo=serverInfo',
                        call.=FALSE)
              if (!is.null(.Object@options$port))
                warning('use of the port parameter is deprecated, use serverInfo=serverInfo',
                        call.=FALSE)
              if (!is.null(.Object@options$serverHost)
                  && !is.null(.Object@options$port))
                .Object@options$serverInfo <- serverInfo(host=.Object@options$serverHost,
                                                            port=.Object@options$port)
              else if (!is.null(.Object@options$serverHost))
                .Object@options$serverInfo <- serverInfo(host=.Object@options$serverHost)
              else if (!is.null(.Object@options$port))
                .Object@options$serverInfo <- serverInfo(port=.Object@options$port)
              else 
                .Object@options$serverInfo <- getServer()
            }


            # This looks strange, but we want to use blendOptions to read possible
            # input for connopts, but still represent it the standard way within
            # the object.
            .Object@connopts =
              if (is.null(.Object@options$connopts)) {
                x <- list()
                x[[OPT.METADATATOSERVER]] <- '1'
                x[[OPT.METADATAFROMSERVER]] <- '1'
                x
              } else {
                as.list(.Object@options$connopts)
              }

            if (.Platform$OS.type == 'windows') {
              # on windows, socketConnection will wait for the full timeout,
              # even if no one is listening on the specified server port.
              # make.socket doesn't, so we'll use it to throw an exception
              # if no one is listening.
              tmpsock <- tryCatch({
                  suppressWarnings(make.socket(nwsHost(.Object@options$serverInfo),
                                               nwsPort(.Object@options$serverInfo)))
                }, error=function(e) {
                  msg <- sprintf(
                         "Please verify that the NetWorkSpaces server is running on '%s:%d'",
                                 nwsHost(.Object@options$serverInfo),
                                 nwsPort(.Object@options$serverInfo))
                  warning(msg, call.=FALSE)
                  stop(e$message, call.=FALSE)
                })
              close.socket(tmpsock)
            }

            # temporarily change the timeout while creating the socketConnection.
            # we will block for up to a year for data on this socket.
            old.timeout = options(timeout=60 * 60 * 24 * 365)
            .Object@nwsSocket = tryCatch({
                suppressWarnings(
                    socketConnection(nwsHost(.Object@options$serverInfo),
                                     port = nwsPort(.Object@options$serverInfo),
                                     open ='a+b', blocking=TRUE))
              }, error=function(e) {
                msg <- sprintf(
                    "Please verify that the NetWorkSpaces server is running on '%s:%d'",
                               nwsHost(.Object@options$serverInfo),
                               nwsPort(.Object@options$serverInfo))
                warning(msg, call.=FALSE)
                stop(e$message, call.=FALSE)
              }, finally=options(old.timeout))

            # tell the server that we're a new client
            writeBin(NegotiationHandshakeInit, .Object@nwsSocket)
            handshake <- nwsRecvN(.Object@nwsSocket, 4, rawflag=TRUE)

            # XXX should we allow for future change here?
            if (identical(handshake, NegotiationHandshakePropose)) {
              # use the new protocol
              # XXX the data from the server is currently ignored
              proposal.opts <- receive.list(.Object@nwsSocket)

              if (nzchar(Sys.getenv('NWS_VERYVERBOSE'))) {
                cat('nws server options proposal:\n')
                print(proposal.opts)
              }

              wp <- proposal.opts$NwsWebPort
              .Object@options$webPort <-
                  if (!is.null(wp)) as.numeric(wp) else NULL

              # request the connection options that we want
              d <- c(NegotiationHandshakeRequest, marshal.list(.Object@connopts))
              writeBin(d, .Object@nwsSocket)
              acc <- nwsRecvN(.Object@nwsSocket, 4, rawflag=TRUE)
              if (!identical(acc, NegotiationHandshakeAccept)) {
                stop("server didn't accept our request", call.=FALSE)
              }
              .Object@cookieProtocol <- TRUE
            } else if (identical(handshake, CookieHandshake)) {
              # old protocol
              warning("connected to old server: ", rawToChar(handshake),
                      call.=FALSE)
              .Object@cookieProtocol <- TRUE

              # make sure we don't send or expect any meta data
              .Object@connopts[[OPT.METADATATOSERVER]] <- NULL
              .Object@connopts[[OPT.METADATAFROMSERVER]] <- NULL
            } else if (identical(handshake, NoCookieHandshake)) {
              # really old protocol
              warning("connected to very old server: ", rawToChar(handshake),
                      call.=FALSE)
              .Object@cookieProtocol <- FALSE

              # make sure we don't send or expect any meta data
              .Object@connopts[[OPT.METADATATOSERVER]] <- NULL
              .Object@connopts[[OPT.METADATAFROMSERVER]] <- NULL
            } else {
              stop('unrecognized handshake: ', handshake, call.=FALSE)
            }
            .Object
          })

setGeneric('sendOp', function(.Object, ..., metadata=list()) standardGeneric('sendOp'))
setGeneric('sendOpStreaming', function(.Object, ..., metadata=list()) standardGeneric('sendOpStreaming'))
setGeneric('nwsDeleteWs', function(.Object, wsName) standardGeneric('nwsDeleteWs'))
setGeneric('nwsListWss', function(.Object, showDataFrame=TRUE) standardGeneric('nwsListWss'))
setGeneric('nwsMktempWs', function(.Object, wsNameTemplate='__Rws__%010d', ...) standardGeneric('nwsMktempWs'))
setGeneric('nwsOpenWs', function(.Object, wsName, space=NULL, ...) standardGeneric('nwsOpenWs'))
setGeneric('nwsUseWs', function(.Object, wsName, space=NULL, ...) standardGeneric('nwsUseWs'))

setMethod('sendOp', 'nwsServer',
          function(.Object, ..., metadata=list()) {
            s = .Object@nwsSocket
            if (OPT.METADATATOSERVER %in% names(.Object@connopts)) {
              if (nzchar(Sys.getenv('NWS_VERYVERBOSE'))) {
                cat('sending metadata:\n')
                print(metadata)
              }
              writeBin(c(marshal.list(metadata), marshal.vec(list(...))), s)
            }
            else {
              if (nzchar(Sys.getenv('NWS_VERYVERBOSE')))
                cat('not sending metadata\n')
              writeBin(marshal.vec(list(...)), s)
            }

            if (OPT.METADATAFROMSERVER %in% names(.Object@connopts))
              receive.list(s)
            else
              list()
          })

setMethod('sendOpStreaming', 'nwsServer',
          function(.Object, ..., extra=NULL, extralen=length(extra),
                   metadata=list()) {
            # sanity check that either "extra" is specified
            # or "extralen" is
            stopifnot(missing(extra) != missing(extralen))

            s = .Object@nwsSocket
            extralen = if (is.integer(extralen))
                         charToRaw(sprintf('%020d', extralen))
                       else
                         charToRaw(sprintf('%020.0f', extralen))
            # this should only happen if extralen was > 2^64
            stopifnot(identical(length(extralen), 20L))

            if (OPT.METADATATOSERVER %in% names(.Object@connopts))
              writeBin(c(marshal.list(metadata), marshal.vec(list(...), 1),
                         extralen, extra), s)
            else
              writeBin(c(marshal.vec(list(...), 1), extralen, extra), s)

            if (is.null(extra))
              # we can't return metadata in this case
              NULL
            else if (OPT.METADATAFROMSERVER %in% names(.Object@connopts))
              receive.list(s)
            else
              list()
          })

setMethod('nwsDeleteWs', 'nwsServer',
          function(.Object, wsName) {
            s = .Object@nwsSocket
            metadata = sendOp(.Object, 'delete ws', wsName)

            # status, unused at the moment.
            bb = nwsRecvN(s, 4)
          })

setMethod('nwsListWss', 'nwsServer',
          function(.Object, showDataFrame=TRUE) {
            s = .Object@nwsSocket
            metadata = sendOp(.Object, 'list wss')

            status = as.integer(nwsRecvN(s, 4))
            desc = nwsRecvN(s, 20)
            if (.Object@cookieProtocol)
              cookie <- nwsRecvN(s, 40)

            ret <- nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
            if (!showDataFrame)
              ret
            else {
              ## convert response into an R data frame
              ret <- unlist(strsplit(ret, "\n"))
              retval <- list()
              fields <- list()
              i = 1
              while (i <= length(ret)) {
                line <- unlist(strsplit(ret[i], "\t"))

                # convert each field to correct type
                fields[1] = FALSE
                if (substr(line[1], 1, 1)=='>')
                  fields[1] = TRUE
                fields[2] = substr(line[1], 2, nchar(line[1]))  # workspace name
                fields[3] = line[2]
                fields[4] = as.logical(line[3])
                fields[5] = as.integer(line[4])
                if (is.na(line[5]))
                  fields[6] = ""
                else
                  fields[6] = line[5]

                retval = c(retval, list(fields))
                i = i+1
              }

              if (length(retval) > 0) {
                names(retval) <- seq(along=retval)
                retval <- do.call(rbind, retval)
                colnames(retval) <-
                  c("Owned", "Name", "Owner", "Persistent", "NumVariables", "Variables")
              }
              retval <- data.frame(retval)
              retval
            }

          })

setMethod('nwsMktempWs', 'nwsServer',
          function(.Object, wsNameTemplate, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('wsmetadata'), call)

            if (!is.character(wsNameTemplate))
              stop('workspace name must be a string')

            s = .Object@nwsSocket
            wsmetadata = opts$wsmetadata
            metadata = sendOp(.Object, 'mktemp ws', wsNameTemplate, metadata=wsmetadata)
            status = as.integer(nwsRecvN(s, 4))
            desc = nwsRecvN(s, 20) # unused at the moment.
            if (.Object@cookieProtocol)
              cookie <- nwsRecvN(s, 40) # unused at the moment.
            n <- as.integer(nwsRecvN(s, 20))
            name <- nwsRecvN(s, n)
            if (status != 0) stop('mktempWs failed')
            name
          })

setMethod('nwsOpenWs', 'nwsServer',
          function(.Object, wsName, space=NULL, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('create', 'persistent', 'wsmetadata'), call)

            # if invoked directly by user, we need to create a space
            # instance. if invoked via networkspace constructor, use the
            # space passed in.
            if (is.null(space)) {
              serverWrap = new.env()
              serverWrap$server = .Object
              space = new('netWorkSpace', wsName=wsName, serverWrap=serverWrap)
            }

            op = 'open ws'
            owner = sprintf('%d', Sys.getpid())
            p = 'no'
            if (!is.null(opts$persistent) && opts$persistent) p = 'yes'

            s = .Object@nwsSocket
            wsmetadata = opts$wsmetadata

            if (is.null(opts$create) || opts$create) {
              metadata = sendOp(.Object, op, wsName, owner, p, metadata=wsmetadata)
            }
            else {
              create = 'no'
              metadata = sendOp(.Object, op, wsName, owner, p, create, metadata=wsmetadata)
            }

            status = as.integer(nwsRecvN(s, 4))
            if (status != 0) stop(paste("workspace", wsName, "doesn't exist"), call.=FALSE)
            space
          })

setMethod('nwsUseWs', 'nwsServer',
          function(.Object, wsName, space=NULL, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('create', 'persistent', 'wsmetadata'), call)

            # see nwsOpenWs
            if (is.null(space)) {
              serverWrap = new.env()
              serverWrap$server = .Object
              space = new('netWorkSpace', wsName=wsName, serverWrap=serverWrap)
            }

            op = 'use ws'
            owner = ''
            p = 'no'

            s = .Object@nwsSocket
            wsmetadata = opts$wsmetadata

            if (is.null(opts$create) || opts$create) {
              metadata = sendOp(.Object, op, wsName, owner, p, metadata=wsmetadata)
            }
            else {
              create = 'no'
              metadata = sendOp(.Object, op, wsName, owner, p, create, metadata=wsmetadata)
            }

            status = as.integer(nwsRecvN(s, 4))
            if (status != 0) stop(paste("workspace", wsName, "doesn't exist"), call.=FALSE)
            space
          })

if (!isGeneric('nwsWebPort'))
  setGeneric('nwsWebPort', function(.Object, ...) standardGeneric('nwsWebPort'))
setMethod('nwsWebPort', 'nwsServer', function(.Object, ...) {
            .Object@options$webPort
          })

close.nwsServer <- function(con, ...) close(con@nwsSocket)

if (!isGeneric('view'))
  setGeneric('view', function(.Object, ...) standardGeneric('view'))
setMethod('view', 'nwsServer', function(.Object, ...) {
  host <- nwsHost(.Object@options$serverInfo)
  port <- nwsWebPort(.Object)
  if (is.null(port))
    stop('the nws server does not have a web interface')

  vstring <- sprintf('http://%s:%d/doit?op=listWss', host, port)
  browseURL(vstring)
  cat(sprintf("Viewing NetWorkSpaces server '%s:%d' in your browser\n",
              host, port))
  invisible(vstring)
})

netWorkSpace <- function(...) {
  new("netWorkSpace", ...)
}

computeDefaultNetWorkSpaceOptions <- function(...)
  list(useUse=FALSE, serverInfo=NULL, serverWrap=NULL,
       connopts=NULL, wsmetadata=list())

# class representing a netWorkSpace.
setClass('netWorkSpace', representation(server='nwsServer',
         wsName='character', cookieProtocol='logical',
                                        serverInfo='ANY'),
         prototype(server=NULL))

setMethod('initialize', 'netWorkSpace',
          function(.Object, wsName='__default', ...) {
            call <- match.call()
            allOpts <- list(...)
            .Object@wsName = wsName

            # We fill the opts list with options we will pass along
            opts <- list()
            if (!is.null(allOpts[['create']])) {
              opts$create = allOpts[['create']]
              allOpts$create = NULL
            }
            if (!is.null(allOpts[['persistent']])) {
              opts$create = allOpts[['persistent']]
              allOpts$persistent = NULL
            }
            check.opts(opts, c('create', 'persistent'), call)

            options <- new.env()
            blendOptions(options, as.list(defaultNetWorkSpaceOptions))

            if (length(allOpts) > 0) {
              # Make sure we don't miss any unnamed arguments
              args <- c('serverHost', 'port', 'useUse', 'serverWrap',
                        'connopts', 'wsmetadata')
              warn <- FALSE
              warningMess <- 'Use of unnamed arguments to netWorkSpace is deprecated'
              for (i in seq(along=allOpts)) {
                if (names(allOpts)[i] == '') {
                  warn <- TRUE
                  assign(args[i], allOpts[[i]], options)
                }
                else {
                  check.opts(allOpts[i], c('serverInfo', args), call)
                  assign(names(allOpts)[i], allOpts[[i]], options)
                }
              }
              # Warn that unnamed arguments are deprecated
              if (warn) cat(warningMess, '\n')
            }

            if (is.null(options$serverInfo)) {
              if (!is.null(options$serverHost))
                warning('use of the serverHost parameter is deprecated, use serverInfo=serverInfo',
                        call.=FALSE)
              if (!is.null(options$port))
                warning('use of the port parameter is deprecated, use serverInfo=serverInfo',
                        call.=FALSE)
              if (!is.null(options$serverHost)
                  && !is.null(options$port))
                options$serverInfo <- serverInfo(host=options$serverHost,
                                                            port=options$port)
              else if (!is.null(options$serverWrap))
                options$serverInfo <- options$serverWrap$server@options$serverInfo
              else if (!is.null(options$serverHost))
                options$serverInfo <- serverInfo(host=options$serverHost)
              else if (!is.null(options$port))
                options$serverInfo <- serverInfo(port=options$port)
              else 
                options$serverInfo <- getServer()
            }

            # In order to hold the deadman we need to keep this around
            .Object@serverInfo <- options$serverInfo

            # assign the options back to local variables
            # This will be switched during the 3.0 code overhaul
            serverHost <- nwsHost(options$serverInfo)
            port <- nwsPort(options$serverInfo)
            useUse <- options$useUse
            serverWrap <- options$serverWrap
            connopts <- options$connopts
            wsmetadata <- options$wsmetadata

            # if invoked (indirectly) via a server openWs or useWs
            # method, the server will be passed in and used. if
            # invoked directly, need to create a new server instance.
            if (!is.null(serverWrap)) {
              # recycle existing server instance.
              .Object@server = serverWrap$server
            }
            else {
              # create new server instance.
              .Object@server = new('nwsServer', serverInfo=.Object@serverInfo,
                connopts=connopts)
              # now give the server a chance to do its thing.
              spaceWrap = new.env()
              spaceWrap$space = .Object
              handler = function(e) { close(.Object@server@nwsSocket); stop(e) }
              if (useUse) {
                # don't claim this space.
                tryCatch({
                  callList = list(.Object=.Object@server, wsName=wsName,
                    space=spaceWrap, wsmetadata=wsmetadata)
                  if (length(opts) > 0)
                    for (i in 1:length(opts))
                      callList[names(opts)[i]] = opts[names(opts)[i]]
                  do.call(nwsUseWs, callList)
                }, error=handler)
              }
              else {
                # attempt to claim ownership
                tryCatch({
                  callList = list(.Object=.Object@server, wsName=wsName,
                    space=spaceWrap, wsmetadata=wsmetadata)
                  if (length(opts) > 0)
                    for (i in 1:length(opts))
                      callList[names(opts)[i]] = opts[names(opts)[i]]
                  do.call(nwsOpenWs, callList)
                  }, error=handler)
              }
            }
            .Object@cookieProtocol <- .Object@server@cookieProtocol

            .Object
          })

showNetWorkSpace <- function(object) {
    nws <- object
    server <- nws@server

    cat('\n')
    cat('NWS Host:\t', nwsHost(server@options$serverInfo), ':',
        nwsPort(server@options$serverInfo), '\n', sep='')
    cat('Workspace Name:\t', nws@wsName, '\n', sep='')
    cat('\n')
}

setMethod('show', 'netWorkSpace', showNetWorkSpace)

setGeneric('nwsClose', function(.Object) standardGeneric('nwsClose'))
setGeneric('nwsDeclare', function(.Object, xName, mode) standardGeneric('nwsDeclare'))
setGeneric('nwsDeleteVar', function(.Object, xName) standardGeneric('nwsDeleteVar'))
setGeneric('nwsFetch', function(.Object, xName, ...) standardGeneric('nwsFetch'))
setGeneric('nwsFetchTry', function(.Object, xName, defaultVal=NULL, ...) standardGeneric('nwsFetchTry'))
setGeneric('nwsFind', function(.Object, xName, ...) standardGeneric('nwsFind'))
setGeneric('nwsFindTry', function(.Object, xName, defaultVal=NULL, ...) standardGeneric('nwsFindTry'))
setGeneric('nwsFetchFile', function(.Object, xName, fObj, ...) standardGeneric('nwsFetchFile'))
setGeneric('nwsFetchTryFile', function(.Object, xName, fObj, ...) standardGeneric('nwsFetchTryFile'))
setGeneric('nwsFindFile', function(.Object, xName, fObj, ...) standardGeneric('nwsFindFile'))
setGeneric('nwsFindTryFile', function(.Object, xName, fObj, ...) standardGeneric('nwsFindTryFile'))
setGeneric('nwsIFetch', function(.Object, xName, ...) standardGeneric('nwsIFetch'))
setGeneric('nwsIFetchTry', function(.Object, xName, defaultVal=NULL, ...) standardGeneric('nwsIFetchTry'))
setGeneric('nwsIFind', function(.Object, xName, ...) standardGeneric('nwsIFind'))
setGeneric('nwsIFindTry', function(.Object, xName, defaultVal=NULL, ...) standardGeneric('nwsIFindTry'))
setGeneric('nwsListVars', function(.Object, wsName='', showDataFrame=TRUE) standardGeneric('nwsListVars'))
setGeneric('nwsStore', function(.Object, xName, xVal, ...) standardGeneric('nwsStore'))
setGeneric('nwsStoreFile', function(.Object, xName, fObj, n=0, ...) standardGeneric('nwsStoreFile'))
setGeneric('nwsWsName', function(.Object) standardGeneric('nwsWsName'))
setGeneric('nwsVariable', function(.Object, xName, mode=c('fifo','lifo','multi','single'),
           env=parent.frame(), force=FALSE, quietly=FALSE) standardGeneric('nwsVariable'))
setGeneric('nwsServerObject', function(.Object) standardGeneric('nwsServerObject'))

setMethod('nwsClose', 'netWorkSpace',
          function(.Object) {
            # XXX this seems wrong
            close(.Object@server)
          })

# helper function for nwsDeclare method.
nwsDeclareInternal <- function(server, ws, xName, mode) {
  metadata = sendOp(server, 'declare var', ws, xName, mode)
  as.integer(nwsRecvN(server@nwsSocket, 4))
}

setMethod('nwsDeclare', 'netWorkSpace',
          function(.Object, xName, mode) {
            status = nwsDeclareInternal(.Object@server, .Object@wsName, xName, mode)
            if (status != 0) {
              stop('variable declaration failed', call.=FALSE)
            }
          })

setMethod('nwsDeleteVar', 'netWorkSpace',
          function(.Object, xName) {
            s = .Object@server@nwsSocket
            ws = .Object@wsName
            metadata = sendOp(.Object@server, 'delete var', ws, xName)
            status = as.integer(nwsRecvN(s, 4))
            if (status != 0) {
              stop('deleteVar failed', call.=FALSE)
            }
          })

# helper function for fetch/find methods.
nwsRetrieve <- function(cprot, server, ws, xName, op, defaultVal=NULL,
                        pkgResult=FALSE, metadata=list()) {
  s = server@nwsSocket
  metadata = sendOp(server, op, ws, xName, metadata=metadata)

  status = as.integer(nwsRecvN(s, 4))

  desc = as.integer(nwsRecvN(s, 20))
  envId = desc %/% 16777216 #(2^24)
  # if bit zero is set, then the object is not serialized
  notSerialized = desc %% 2

  if (cprot) cookie <- nwsRecvN(s, 40)

  n = as.numeric(nwsRecvN(s, 20))

  if (status != 0) {
    # make sure we read all data to avoid corrupting the connection
    sVal = nwsRecvN(s, n, rawflag=TRUE)
    stop('retrieval failed', call.=FALSE)
  }

  if (notSerialized) {
    sVal = nwsRecvN(s, n, rawflag=TRUE)

    # if bit zero and one of desc are set, it's binary data
    if (desc %% 4 - 1)
      # Return a raw vector
      if (pkgResult)
        list(data=sVal, status=TRUE, metadata=metadata)
      else
        sVal
    else
      # Return a character string
      if (pkgResult)
        list(data=rawToChar(sVal), status=TRUE, metadata=metadata)
      else
        rawToChar(sVal)
  }
  else if (n > 0) {
    # Return an object
    tryCatch({
        if (pkgResult)
          list(data=unserialize(s), status=TRUE, metadata=metadata)
        else
          unserialize(s)
      }, error=function(e) {
        stop(sprintf("unable to unserialize value from ws variable '%s': %s",
                     xName, e$message), call.=FALSE)
      })
  }
  else {
    # Return the default value
    if (pkgResult)
      list(data=defaultVal, status=FALSE, metadata=metadata)
    else
      defaultVal
  }
}

setMethod('nwsFetch', 'netWorkSpace',
          function(.Object, xName, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('pkgResult', 'metadata'), call)
            pkgResult <- check.logical(opts$pkgResult, FALSE, 'pkgResult', call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            nwsRetrieve(.Object@cookieProtocol, .Object@server,
                        .Object@wsName, xName, 'fetch', pkgResult=pkgResult,
                        metadata=metadata)
          })

setMethod('nwsFetchTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('pkgResult', 'metadata'), call)
            pkgResult <- check.logical(opts$pkgResult, FALSE, 'pkgResult', call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            tryCatch({
                nwsRetrieve(.Object@cookieProtocol, .Object@server,
                            .Object@wsName, xName, 'fetchTry', defaultVal,
                            pkgResult=pkgResult, metadata=metadata)
              }, error=function(e) {
                if (pkgResult)
                  list(data=defaultVal, status=FALSE, metadata=list())
                else
                  defaultVal
              })
          })

setMethod('nwsFind', 'netWorkSpace',
          function(.Object, xName, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('pkgResult', 'metadata'), call)
            pkgResult <- check.logical(opts$pkgResult, FALSE, 'pkgResult', call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            nwsRetrieve(.Object@cookieProtocol, .Object@server,
                        .Object@wsName, xName, 'find', pkgResult=pkgResult,
                        metadata=metadata)
          })

setMethod('nwsFindTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('pkgResult', 'metadata'), call)
            pkgResult <- check.logical(opts$pkgResult, FALSE, 'pkgResult', call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            tryCatch({
                nwsRetrieve(.Object@cookieProtocol, .Object@server,
                            .Object@wsName, xName, 'findTry', defaultVal,
                            pkgResult=pkgResult, metadata=metadata)
              }, error=function(e) {
                if (pkgResult)
                  list(data=defaultVal, status=FALSE, metadata=list())
                else
                  defaultVal
              })
          })

# helper function for fetchFile/findFile methods.
nwsRetrieveFile <- function(cprot, server, ws, xName, op, fObj) {
  s = server@nwsSocket

  if (missing(fObj)) {
    stop('no value specified for fObj argument', call.=FALSE)
  }

  if (is.character(fObj)) {
    f <- file(fObj, 'wb')
    on.exit(close(f))
  } else {
    if (!is(fObj, "file") || !isOpen(fObj, "w") || summary(fObj)$text != "binary")
      stop('fobj must be a binary mode file object opened for writing', call.=FALSE)
    f <- fObj
  }

  metadata = sendOp(server, op, ws, xName)

  status <- as.integer(nwsRecvN(s, 4))

  # even if failure status, read the rest of the bytes
  desc <- as.integer(nwsRecvN(s, 20))
  if (cprot) cookie <- nwsRecvN(s, 40)
  n <- as.numeric(nwsRecvN(s, 20))

  blen <- 16 * 1024
  while (n > 0) {
    d <- nwsRecvN(s, min(n, blen), rawflag=TRUE)
    if (length(d) == 0) stop('NWS server connection dropped', call.=FALSE)
    writeBin(d, f)
    n <- n - length(d)
  }

  if (status != 0) stop('retrieval failed', call.=FALSE)
  TRUE
}

setMethod('nwsFetchFile', 'netWorkSpace',
          function(.Object, xName, fObj) {
            nwsRetrieveFile(.Object@cookieProtocol, .Object@server,
                            .Object@wsName, xName, 'fetch', fObj)
          })

setMethod('nwsFetchTryFile', 'netWorkSpace',
          function(.Object, xName, fObj) {
            tryCatch({
                nwsRetrieveFile(.Object@cookieProtocol, .Object@server,
                                .Object@wsName, xName, 'fetchTry', fObj)
              }, error=function(e) {
                if (e$message == 'retrieval failed') {
                  FALSE
                }
                else {
                  stop(e$message, call.=FALSE)
                }
              })
          })

setMethod('nwsFindFile', 'netWorkSpace',
          function(.Object, xName, fObj) {
            nwsRetrieveFile(.Object@cookieProtocol, .Object@server,
                            .Object@wsName, xName, 'find', fObj)
          })

setMethod('nwsFindTryFile', 'netWorkSpace',
          function(.Object, xName, fObj) {
            tryCatch({
                nwsRetrieveFile(.Object@cookieProtocol, .Object@server,
                                .Object@wsName, xName, 'findTry', fObj)
              }, error=function(e) {
                if (e$message == 'retrieval failed') {
                  FALSE
                }
                else {
                  stop(e$message, call.=FALSE)
                }
              })
          })

setMethod('nwsIFetch', 'netWorkSpace',
          function(.Object, xName) {
            nwsValueIterator(.Object, xName, 'ifetch', NULL)
          })

setMethod('nwsIFetchTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            nwsValueIterator(.Object, xName, 'ifetchTry', defaultVal)
          })

setMethod('nwsIFind', 'netWorkSpace',
          function(.Object, xName) {
            nwsValueIterator(.Object, xName, 'ifind', NULL)
          })

setMethod('nwsIFindTry', 'netWorkSpace',
          function(.Object, xName, defaultVal=NULL) {
            nwsValueIterator(.Object, xName, 'ifindTry', defaultVal)
          })

# to see list output clearly use: write(nwsList...(), stdout())
setMethod('nwsListVars', 'netWorkSpace',
          function(.Object, wsName='', showDataFrame=TRUE) {
            s = .Object@server@nwsSocket
            if (wsName == '') wsName = .Object@wsName

            metadata = sendOp(.Object@server, 'list vars', wsName)

            # status, unused at the moment
            status = as.integer(nwsRecvN(s, 4))
            desc = nwsRecvN(s, 20)
            if (.Object@cookieProtocol)
              cookie <- nwsRecvN(s, 40)

            ret <- nwsRecvN(s, as.integer(nwsRecvN(s, 20)))
            if (!showDataFrame)
              ret
            else {
              ## convert response into an R data frame
              ret <- unlist(strsplit(ret, "\n"))
              retval <- list()
              fields <- list()

              i = 1
              while (i<=length(ret)) {
                line <- unlist(strsplit(ret[i], "\t"))

                # convert each field to correct type
                fields[1] = line[1]
                fields[2] = as.integer(line[2])
                fields[3] = as.integer(line[3])
                fields[4] = as.integer(line[4])
                fields[5] = line[5]
                retval = c(retval, list(fields))
                i = i+1
              }

              if (length(retval)>0) {
                names(retval) <- seq(along=retval)
                retval <- do.call(rbind, retval)
                colnames(retval) <-
                c("Variable", "NumValues", "NumFetchers", "NumFinders", "Mode")
              }

              retval <- data.frame(retval)
              retval
            }

          })

# helper function for store method
nwsStoreInternal <- function(server, ws, xName, xVal, metadata=list()) {
  s = server@nwsSocket
  desc = nwsRFP # R Fingerprint

  if (missing(xVal)) {
    stop('no value specified for xVal argument', call.=FALSE)
  }

  if (is.raw(xVal)) {
    desc = desc + 3
  }
  else if (!is.character(xVal) || (length(xVal) != 1)) {
    xVal = serialize(xVal, ascii=FALSE, connection=NULL)
    # serialize returns a raw vector as of R 2.4
    if (is.character(xVal)) xVal = charToRaw(xVal)
  }
  else {
    xVal = charToRaw(xVal)
    desc = desc + 1 # in other systems, we use a manifest constant and a bit or here... .
  }
  descTxt = sprintf('%020i', desc) # would prefer to use unsigned here.

  metadata = sendOpStreaming(server, 'store', ws, xName, descTxt, extra=xVal,
                             metadata=metadata)

  # status, barely used at the moment.
  status = as.integer(nwsRecvN(s, 4))

  if (status != 0) {
    stop('store failed', call.=FALSE)
  }
}

setMethod('nwsStore', 'netWorkSpace',
          function(.Object, xName, xVal, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('metadata'), call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            nwsStoreInternal(.Object@server, .Object@wsName, xName, xVal,
                             metadata=metadata)
          })

setMethod('nwsStoreFile', 'netWorkSpace',
          function(.Object, xName, fObj, n=0, ...) {
            call <- match.call()
            opts <- list(...)
            check.opts(opts, c('metadata', 'serialized'), call)
            metadata <- check.list(opts$metadata, list(), 'metadata', call)
            serialized <- check.logical(opts$serialized, FALSE,
                                        'serialized', call)

            ws <- .Object@wsName
            s <- .Object@server@nwsSocket

            desc <- if (serialized) nwsRFP else nwsRFP + 3

            if (missing(fObj)) {
              stop('no value specified for fObj argument', call.=FALSE)
            }

            # if fObj is a character string, handle it specially
            if (is.character(fObj)) {
              f <- file(fObj, 'rb')
              on.exit(close(f))
            } else {
              if (!is(fObj, "file") || !isOpen(fObj, "r") || summary(fObj)$text != "binary")
                stop('fobj must be a binary mode file object opened for reading', call.=FALSE)
              f <- fObj
            }

            fsize <- file.info(summary(f)$description)$size
            fpos <- seek(f)
            fbytes <- fsize - fpos
            n <- if (n <= 0) fbytes else min(n, fbytes)
            if (n <= 0) return(FALSE)

            descTxt <- sprintf('%020i', desc) # would prefer to use unsigned here.

            sendOpStreaming(.Object@server, 'store', ws, xName,
                            descTxt, extralen=n, metadata=metadata)

            blen <- 16 * 1024
            while (n > 0) {
              d <- readBin(f, what='raw', n=min(blen, n))
              dlen <- length(d)
              if (dlen <= 0)
                break
              writeBin(d, s)
              n <- n - dlen
            }

            if (n > 0) {
              # I don't thing this should ever happen unless the file
              # size computation is incorrect, but I really don't want
              # to corrupt the server connection
              warning('unable to read all the data in file ',
                      summary(f)$description, ' [size: ', fsize,
                      'bytes]: padding value in workspace variable')
              blen <- 1024
              buffer <- raw(blen)
              while (n > 0) {
                if (blen <= n) {
                  writeBin(buffer, s)
                  n <- n - dlen
                } else {
                  writeBin(raw(n), s)
                  break
                }
              }
            }

            # now we're ready to read the metadata
            # XXX need to return this to the user
            connopts <- .Object@server@connopts
            metadata <- if (OPT.METADATAFROMSERVER %in% names(connopts))
                receive.list(s)
              else
                list()

            # status, barely used at the moment.
            status <- as.integer(nwsRecvN(s, 4))

            if (status != 0) {
              stop('store file failed', call.=FALSE)
            }
            TRUE
          })

setMethod('nwsWsName', 'netWorkSpace', function(.Object) {.Object@wsName})

setMethod('nwsVariable', 'netWorkSpace',
          function(.Object, xName, mode=c('fifo','lifo','multi','single'),
                   env=parent.frame(), force=FALSE, quietly=FALSE) {
            missingMode = missing(mode)
            mode <- match.arg(mode)

            # be careful, because 'exists' will cause an active binding function
            # to be called, which is a side effect that we don't want
            if (force ||
                (!tryCatch(bindingIsActive(xName, env), error=function(...) FALSE) &&
                 !exists(xName, envir=env, inherits=FALSE))) {
              s = .Object@server@nwsSocket
              ws = .Object@wsName
              if (missingMode) {
                mlist = c(mode, 'fifo', 'lifo', 'multi', 'single')
                mode = NA
                for (m in mlist) {
                  if (nwsDeclareInternal(.Object@server, ws, xName, m) == 0) {
                    mode = m
                    break
                  }
                }
                if (is.na(mode))
                  stop('unable to declare variable', call.=FALSE)
              } else {
                if (nwsDeclareInternal(.Object@server, ws, xName, mode) != 0)
                  stop('variable declaration failed', call.=FALSE)
              }

              if (identical(mode, 'single')) {
                mf <- function(val)
                  if (missing(val))
                    nwsRetrieve(.Object@cookieProtocol, .Object@server, ws, xName, 'find')
                  else
                    nwsStoreInternal(.Object@server, ws, xName, val)
              } else {
                mf <- function(val)
                  if (missing(val))
                    nwsRetrieve(.Object@cookieProtocol, .Object@server, ws, xName, 'fetch')
                  else
                    nwsStoreInternal(.Object@server, ws, xName, val)
              }

              t <- makeActiveBinding(xName, mf, env)
            } else {
              if (! quietly)
                warning('not overwriting previous binding for ', xName)
            }
          })

setMethod('nwsServerObject', 'netWorkSpace', function(.Object) .Object@server)

if (!isGeneric('view'))
  setGeneric('view', function(.Object, ...) standardGeneric('view'))
setMethod('view', 'netWorkSpace', function(.Object, ...) {
  host <- nwsHost(.Object@serverInfo)
  port <- nwsWebPort(.Object@server)
  if (is.null(port))
    stop('the nws server does not have a web interface')

  wsName <- .Object@wsName
  vstring <- sprintf('http://%s:%d/doit?op=listVars&wsName=%s',
                     host, port, URLencode(wsName))
  browseURL(vstring)
  cat(sprintf("Viewing workspace '%s' on server '%s:%d' in your browser\n",
              wsName, host, port))
  invisible(vstring)
})

# helper function for ifetch/ifind methods.
nwsIRetrieve <- function(server, ws, xName, op, varId, valIndex) {
  s = server@nwsSocket
  blanks = '                    '
  varId = strtrim(paste(varId, blanks, sep=''), 20)
  valIndex = strtrim(paste(valIndex, blanks, sep=''), 20)
  metadata = sendOp(server, op, ws, xName, varId, valIndex)

  status = as.integer(nwsRecvN(s, 4))

  desc = as.integer(nwsRecvN(s, 20))
  envId = desc %/% 16777216 #(2^24)
  # if bit zero is set, then the object is not serialized
  notSerialized = desc %% 2

  # cookie protocol is assumed at this point
  varId = nwsRecvN(s, 20)
  valIndex = as.integer(nwsRecvN(s, 20))

  n = as.integer(nwsRecvN(s, 20))

  if (notSerialized) {
    sVal = nwsRecvN(s, n, rawflag=TRUE)

    # if bit zero and one of desc are set, it's binary data
    if (desc %% 4 - 1)
      # Return a raw vector
      list(status=status, sVal=sVal, varId=varId, valIndex=valIndex)
    else
      # Return a character string
      list(status=status, sVal=rawToChar(sVal), varId=varId, valIndex=valIndex)
  }
  else if (n > 0) {
    list(status=status, sVal=unserialize(s), varId=varId, valIndex=valIndex)
  }
  else {
    stop('StopIteration')
  }
}

# helper function to return a closure that acts as an iterator
nwsValueIterator <- function(.Object, xName, op, defaultVal) {
  if (!.Object@cookieProtocol)
    stop('NWS server does not support iterated operations', call.=FALSE)
  if (!is.character(xName))
    stop('variable name must be a string', call.=FALSE)

  # initial state of the closure
  varId <- ''
  valIndex <- 0

  function() {
    defval <- list(status=0, varId=varId, valIndex=valIndex, sVal=defaultVal)

    r <- tryCatch({
          nwsIRetrieve(.Object@server, .Object@wsName,
                       xName, op, varId, valIndex)
        }, error=function(e) defval)

    varId <<- r$varId
    valIndex <<- r$valIndex

    if (r$status != 0) stop('retrieval failed', call.=FALSE)
    r$sVal
  }
}
