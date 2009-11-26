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

# We use alternating barriers to synchronize eachWorker
# invocations. Their names are common to workers and sleighs.
barrierNames <- list('barrier0', 'barrier1')
RNG.TYPES.AVAILIBLE <- c('legacy')
# heuristic test for a closure
isClosure <- function(fun) {
  if (is.function(fun)) {
    e <- environment(fun)
    !is.null(e) && exists("environmentName", mode="function") &&
        identical(environmentName(e), "") &&
        length(ls(e, all.names=FALSE)) > 0
  } else {
    FALSE
  }
}

############################################################################
#  Sleigh code
#

sleigh <- function(...) {
  new("sleigh",...)
}

defaultSleighOptions <- new.env()
nodeSleighOptions <- new.env()

computeDefaultSleighOptions <- function(pkgpath) {
  # compute default value for scriptDir
  scriptDir = file.path(pkgpath, 'bin')

  # for some reason, R.home() uses backslashes on Windows,
  # even though .Platform says it should be forward slash.
  # this should fix the problem.
  rhome = gsub('\\\\', .Platform$file.sep, R.home())

  if (.Platform$OS.type == 'windows') {
    rprog = file.path(rhome, 'bin', 'Rterm.exe')
  }
  else {
    rprog = file.path(rhome, 'bin', 'R')
  }

  python = .serverGlobals$python
  extraPythonModules = .serverGlobals$pythonPath

  list(
      nwsHost = NULL,
      nwsHostRemote = NULL,
      nwsPort = NULL,
      nwsPortRemote = NULL,
      outfile = NULL,
      launch = 'local',
      workerCount = NULL,
      nodeList = NULL,
      scriptExec = scriptcmd,
      wrapperDir = scriptDir,
      scriptDir = scriptDir,
      scriptName = 'RNWSSleighWorker.py',
      workerWrapper = 'BackgroundLaunch.py',
      workingDir = getwd(),
      logDir = NULL,
      user = NULL,
      passwd = NULL,
      wsNameTemplate = 'sleigh_ride_%04d',
      userWsNameTemplate = 'sleigh_user_%04d',
      verbose = FALSE,
      rprog = rprog,
      python = python,
      pythonOpts = c('-E', '-W', 'ignore::DeprecationWarning'),
      extraPythonModules = extraPythonModules,
      serverInfo = NULL,
      rngType = 'legacy',
      rngSeed = NULL,
      fixedargSize = 10240,
      workspaceVersion = 0
      )
}

####
# sleigh class
#
# represents a collection R processes running on a simple network of
# workstation pulling in tasks and generating results.


setClass('sleigh',
         representation(nodeList='character', nws='netWorkSpace',
                        userNws='netWorkSpace', nwsName='character',
                        userNwsName='character', nwss='nwsServer',
                        options='environment', state='environment',
                        newSleigh='logical'),
         prototype(nws=NULL, userNws=NULL, nwss=NULL))

setMethod('initialize', 'sleigh',
function(.Object, ...) {

  argList = list(...)

  # backward compatibility
  # check if nodeList is specified in the old way
  if (length(argList) > 0) {
    argName = names(argList[1])
    if (is.null(argName) || nchar(argName) == 0) {
      if (!is.vector(argList[[1]]))
        stop('argument 1 has no name and is not a vector')
      names(argList)[1] = 'nodeList'
      warning('nodeList should be passed using named variable, nodeList')
    }
  }

  # sanity check the optional arguments
  unrecog = names(argList)[!names(argList) %in% ls(defaultSleighOptions)]
  if (length(unrecog) > 0)
    stop('unused argument(s) ', paste(unrecog, collapse=', '))

  .Object@options = new.env()
  blendOptions(.Object@options, as.list(defaultSleighOptions))
  blendOptions(.Object@options, argList)

  if (is.null(.Object@options$serverInfo)) {
    if (!is.null(.Object@options$nwsHost))
      warning('use of the nwsHost parameter is deprecated, use serverInfo=serverInfo',
              call.=FALSE)
    if (!is.null(.Object@options$nwsPort))
      warning('use of the nwsPort parameter is deprecated, use serverInfo=serverInfo',
              call.=FALSE)

    if (!is.null(.Object@options$nwsHost)
        && !is.null(.Object@options$nwsPort))
      .Object@options$serverInfo <- serverInfo(host=.Object@options$nwsHost,
                                               port=.Object@options$nwsPort)
    else if (!is.null(.Object@options$nwsHost))
      .Object@options$serverInfo <- serverInfo(host=.Object@options$nwsHost)
    else if (!is.null(.Object@options$nwsPort))
      .Object@options$serverInfo <- serverInfo(port=.Object@options$nwsPort)
    else
    .Object@options$serverInfo <- getServer()
  }

  .Object@options$nwsHost <- nwsHost(.Object@options$serverInfo)
  .Object@options$nwsPort <- nwsPort(.Object@options$serverInfo)

  # Even though we have just written back the host and port in the above
  # statements, we are going to use the serverInfo again when creating
  # the nwsServer object (we have to do this in order to hold on to the
  # deadman.  Thus it's really important to not change the host and port
  # in the next fifteen lines.  There's not really a reason to anyway.

  opts = .Object@options

  .Object@state = new.env()
  .Object@state$bx = 1
  .Object@state$occupied = FALSE
  .Object@state$stopped = FALSE
  .Object@state$launch = opts$launch
  .Object@state$totalTasks = 0
  .Object@state$rankCount = 0
  .Object@state$job = 0

  if (!is.function(opts$launch) &&  !is.character(opts$launch)) {
    stop('unknown launch protocol')
  }
  else if (is.character(opts$launch) &&
      !opts$launch %in% c('local', 'service')) {
    stop('unsupported launch protocol')
  }

  if(length(which(RNG.TYPES.AVAILIBLE == opts$rngType)) == 0)
    stop('rng type not supported')
  
  # set up the sleigh's netWorkSpace.
  .Object@nwss <- nwsServer(serverInfo=.Object@options$serverInfo)
  meta = list(wstype='sleigh')
  .Object@nwsName = nwsMktempWs(.Object@nwss, opts$wsNameTemplate, wsmetadata=meta)
  .Object@userNwsName = nwsMktempWs(.Object@nwss, opts$userWsNameTemplate)
  # NEED TO ADD ERROR HANDLING CODE HERE
  .Object@nws = nwsOpenWs(.Object@nwss, .Object@nwsName)
  .Object@userNws = nwsOpenWs(.Object@nwss, .Object@userNwsName)
  .Object@state$workspaceVersion <- nwsFindTry(.Object@nws,'version',defaultVal=0)
 
  # check if this uses a new sleigh workspace, or a generic workspace
  version <- nwsFindTry(.Object@nws, 'version')
  .Object@newSleigh <- !is.null(version)
  if (opts$verbose) {
    if (.Object@newSleigh)
      cat(sprintf('using version %s sleigh workspace\n', version))
    else
      cat('using generic (old-style) workspace\n')
  }

  nwsDeclare(.Object@nws, 'exported', 'fifo')

  if (is.function(opts$launch) || opts$launch == 'local') {
    if (is.null(opts$workerCount)) {
      .Object@nodeList = if (is.null(opts$nodeList))
                           rep('localhost', 3) else opts$nodeList
    }
    else {
      .Object@nodeList = if (is.null(opts$nodeList))
                           rep('localhost', opts$workerCount)
                         else
                           rep(opts$nodeList, length=opts$workerCount)
    }
    .Object@state$workerCount = length(.Object@nodeList)

    if (.Object@state$workerCount < 1) {
      close(.Object@nwss)
      stop('must have at least one worker in a sleigh')
    }

    if (.Object@newSleigh) {
      nwsStore(.Object@nws, 'workerCount',
               as.character(.Object@state$workerCount))
    }

    nodes <- sort(.Object@nodeList)
    rnodes <- rle(nodes)
    id <- 0

    # Check to see if the seed is still null.  We want to make sure
    # that each sleigh has a unique seed.
    if (is.null(opts$rngSeed)) opts$rngSeed <- as.numeric(Sys.time())
    
    for (i in seq(along=rnodes$values)) {
      nodeName <- rnodes$values[i]
      nodeOpts <- new.env()
      blendOptions(nodeOpts, as.list(opts))
      if (exists(nodeName, envir=nodeSleighOptions, mode='list',inherits=FALSE))
        blendOptions(nodeOpts,
            get(nodeName, envir=nodeSleighOptions, mode='list', inherits=FALSE))
      if (exists('nodeName', envir=nodeOpts, inherits=FALSE))
        nodeName <- get('nodeName', envir=nodeOpts, inherits=FALSE)
      if (nodeOpts$verbose)
        nodeOpts$outfile = sprintf('%s_%04d.txt', .Object@nwsName, i)

      addWorker(nodeName, .Object@nwsName, .Object@userNwsName,
                id, .Object@state$workerCount, rnodes$lengths[i], nodeOpts)
      id <- id + rnodes$lengths[i]
    }
  }
  else if (opts$launch == 'service') {
    # remote launch using the "R Sleigh Service"
    service = tryCatch({
        nwsUseWs(.Object@nwss, 'RSleighService', create=FALSE)
      }, error=function(e) {
        close(.Object@nwss)
        stop('no sleigh services are running', call.=FALSE)
      })

    wsvars = nwsListVars(service, showDataFrame=TRUE)
    regworkers = unlist(wsvars$Variable, use.names=FALSE)
    user = if (is.null(opts$user)) Sys.info()[['login']] else opts$user

    # Note: we are only allowing execution on non-administrator sleigh services
    myworkers = regworkers[grep(paste('^', user, '@.', sep=''), regworkers)]

    if (!is.null(opts$nodeList)) {
      warning('ignoring user specified nodeList')
    }

    if (length(myworkers) < 1 || (!is.null(opts$workerCount) && opts$workerCount < 1)) {
      close(.Object@nwss)
      stop('must have at least one worker in a sleigh')
    }

    .Object@nodeList = if (!is.null(opts$workerCount))
                         rep(myworkers, length.out=opts$workerCount) 
                       else myworkers
    .Object@state$workerCount = length(.Object@nodeList)

    b = function(x) if (is.null(x)) '' else x
    nodes <- sort(.Object@nodeList)
    id <- 0
    rnodes <- rle(nodes)
    for (i in seq(along=rnodes$values)) {
      if (opts$verbose)
        opts$outfile = sprintf('%s_%04d.txt', .Object@nwsName, i)
      # XXX is '@' the best delimiter?
      request = sprintf('@%s@%s@%d@%d@%s@%s@%s@%s@%d@',
                        .Object@nwsName,
                        .Object@userNwsName,
                        .Object@state$workerCount,
                        id,
                        b(opts$workingDir),
                        b(opts$outfile),
                        b(opts$logDir),
                        user,
                        rnodes$lengths[i])
      id <- id + rnodes$lengths[i]
      if (opts$verbose)
        cat('command:', request, '\n')

      nwsStore(service, rnodes$values[i], request)
    }
  }
  else if (opts$launch == 'web') {
    cat(sprintf("Your ride is %s, don't forget 'DeleteMe...'.\n", .Object@nwsName))
    nwsStore(.Object@nws, 'runMe', sprintf("library(nws); launch('%s', '%s', %d, userNwsName='%s')",
                                           .Object@nwsName, opts$nwsHost, opts$nwsPort,
                                           .Object@userNwsName))

    tryCatch(nwsFetch(.Object@nws, 'deleteMeWhenAllWorkersStarted'), error=function(...) 0)
    nwsDeleteVar(.Object@nws, 'runMe')

    # XXX this is broken
    .Object@state$workerCount = nwsFetch(.Object@nws, 'rankCount')
    nwsStore(.Object@nws, 'workerCount', .Object@state$workerCount)
    nwsStore(.Object@nws, 'rankCount', -1)
    .Object@state$rankCount = -1
  }

  .Object
})

setMethod('show', 'sleigh', function(object) {
  cat('\n')
  cat('NWS Sleigh Object\n')
  show(object@nws)
  cat(object@state$workerCount, ' Worker Nodes:\t',
      paste(object@nodeList, collapse=', '), '\n', sep='')
  cat('\n')
})

# return a list that contains two values: workerCount and status
setGeneric('status', function(.Object, closeGroup=FALSE, timeout=0) standardGeneric('status'))
setMethod('status', 'sleigh',
function(.Object, closeGroup=FALSE, timeout=0) {
  if (!is.logical(closeGroup))
    stop('the type of the closeGroup argument must be logical')

  if (!is.numeric(timeout))
    stop('the type of the timeout argument must be numeric')

  if (!.Object@newSleigh) {
    warning('status method requires nws server 2.0 with sleigh workspace plugin to be fully functional')
    return(list(numWorkers=.Object@state$workerCount, closed=TRUE))
  }

  if (.Object@state$rankCount < 0) {
    # join phase completed before
    list(numWorkers=workerCount(.Object), closed=TRUE)
  }
  else {
    if (closeGroup) {
      # set the timeout, wait for workers, close the group, and get the count
      nwsStore(.Object@nws, 'status', as.character(timeout))
      nwsFind(.Object@nws, 'status')
      numWorkers <- as.integer(nwsFind(.Object@nws, 'workerCount'))
      closed <- TRUE
    }
    else {
      # wait for workers using timeout, and get the join status
      meta <- list(delay=as.character(timeout))
      numWorkers <- as.integer(nwsFetch(.Object@nws, 'waitForWorkers',
                                        metadata=meta))
      stat <- nwsFind(.Object@nws, 'join_status')
      closed <- switch(stat,
                       open=, joining=FALSE,
                       closed=TRUE,
                       stop('illegal join status'))
    }

    if (closed) {
      # reset the sleigh's workerCount and rankCount
      .Object@state$workerCount <- numWorkers
      .Object@state$rankCount <- -1
    }

    list(numWorkers=numWorkers, closed=closed)
  }
})

close.sleigh <- function(con, ...) stopSleigh(con)

setGeneric('stopSleigh', function(.Object) standardGeneric('stopSleigh'))
setMethod('stopSleigh', 'sleigh', function(.Object) {
  if (.Object@state$stopped) return (invisible(NULL))

  if (!is.function(.Object@state$launch) &&
      identical(.Object@state$launch, 'web')) {
    nwsDeleteVar(.Object@nws, 'task')
  }
  else {
    nwsStore(.Object@nws, 'Sleigh ride over', 1)
  }
  Sys.sleep(3)
  exitCount = 0
  while (!is.null(nwsFetchTry(.Object@nws, 'bye'))) {
    exitCount = exitCount + 1
  }
  if (exitCount != .Object@state$workerCount) {
    cat(sprintf('Only %d of %d have exited.\n', exitCount, .Object@state$workerCount))
  }
  nwsDeleteWs(.Object@nwss, .Object@nwsName)
  close(.Object@nwss)
  .Object@state$stopped = TRUE
})


# run fun once on each worker of the sleigh. pass in a val from the
# range 1:#Workers
setGeneric('eachWorker',
           function(.Object, fun, ..., eo=NULL, DEBUG=FALSE) standardGeneric('eachWorker'))
setMethod('eachWorker', 'sleigh',
function(.Object, fun, ..., eo=NULL, DEBUG=FALSE) {
  if (DEBUG) browser()

  if (.Object@state$rankCount == -1 && .Object@state$workerCount < 1) {
    stop(paste('worker group has been closed, and we have', .Object@state$workerCount, 'workers'))
  }

  if (.Object@state$occupied) {
    stop('sleigh is occupied')
  }

  if (.Object@state$stopped) {
    stop('sleigh is stopped')
  }

  fun <- fun # need to force the argument (NJC: why?)
  force(list(...))  # catch errors before we attempt to submit tasks

  nws = .Object@nws
  wc = .Object@state$workerCount

  blocking = TRUE
  accumulator = NULL
  closure = NULL
  if (!is.null(eo)) {
    if (is.environment(eo) || is.list(eo)) {
      if (!is.null(eo$blocking)) blocking = as.logical(eo$blocking)
      accumulator = eo$accumulator
      if (!is.null(eo$closure)) closure = as.logical(eo$closure)

      # check for unknown options
      if (is.list(eo)) {
        eo$blocking <- eo$accumulator <- eo$closure <- NULL
        if (length(eo) > 0)
          warning('ignoring unknown option(s): ',
            paste('"', names(eo), '"', sep='', collapse=', '))
      }
    }
    else {
      stop('options arg must be a list or environment')
    }
  }

  # issue a warning if fun seems like a closure, and they aren't
  # explicitly enabled via the closure option.
  if (is.null(closure)) {
    closure <- TRUE
    if (isClosure(fun))
      warning('"fun" argument looks like a closure without enabling ',
        'via closure option', immediate.=TRUE)
  }

  # remove the enclosing environment of the function if closures are not
  # allowed.
  if (!closure)
    environment(fun) <- globalenv()

  # use alternating barrier to sync eachWorker invocations with the workers.
  bx = .Object@state$bx
  bn = barrierNames[[bx]]
  .Object@state$bx = bx%%2 + 1

  if (!.Object@newSleigh)
    nwsFetchTry(.Object@nws, bn)

  # allocate a new job id
  job = .Object@state$job
  .Object@state$job = job + 1

  # submit the tasks
  tryCatch({
      if (.Object@newSleigh)
        storeTask(tag=99999, nws=nws, fun=fun, args=list(list(...)), barrier=FALSE,
                  job=job, varName='broadcast')
      else
        lapply(1:wc, storeTask, nws=nws, fun=fun, args=list(list(...)), barrier=TRUE,
               job=job)
    },
    error=function(e) {
      if (!.Object@newSleigh) {
        # XXX we can recover from sending no tasks, but some tasks is bad.
        # XXX it would be nice to detect that situation.
        nwsStore(.Object@nws, bn, 1)
        stop('error sending tasks may have corrupted this sleigh', call.=FALSE)
      }
    })

  # update the total number of submitted tasks
  .Object@state$totalTasks <- .Object@state$totalTasks + wc

  if (!blocking) {
    if (!.Object@newSleigh)
      .Object@state$occupied = TRUE
    func = if (is.null(accumulator)) as.function(list(NULL)) else accumulator
    return (new('sleighPending', nws, wc, wc, func, bn, .Object@state,
                .Object@newSleigh, job))
  }

  val <- if (is.null(accumulator)) vector('list', wc) else NULL
  accumargs = try(length(formals(accumulator)))

  for (i in 1:wc) {
    repeat {
      p = nwsFetch(nws, 'result', metadata=list(batchId=as.character(job)),
                   pkgResult=TRUE)
      r = p$data
      metadata = p$metadata

      # check for a real worker result
      if (is.list(r) && r$type == 'VALUE') {
        break
      }

      if (!is.null(metadata) && metadata$nwsNull == '1') {
        r = list(value=list(NULL), rank=as.integer(metadata$nwsWorkerRank))
        warning(sprintf('returning a NULL result since worker %d is dead', r$rank[[1]]))
        break
      }
    }
    if (is.null(accumulator)) {
      val[r$rank + 1] = r$value
    }
    else {
      if (accumargs == 0)
        accumulator()
      else if (accumargs == 1)
        accumulator(r$value)
      else
        accumulator(r$value, r$rank + 1)
    }
  }

  if (!.Object@newSleigh)
    nwsStore(.Object@nws, bn, 1)

  val
})


# run fun once for each element of a vector.
setGeneric('eachElem',
           function(.Object, fun, elementArgs=list(), fixedArgs=list(),
                    eo=NULL, DEBUG=FALSE) standardGeneric('eachElem'))
setMethod('eachElem', 'sleigh',
function(.Object, fun, elementArgs=list(), fixedArgs=list(), eo=NULL, DEBUG=FALSE) {
  if (DEBUG) browser()

  if (.Object@state$rankCount == -1 && .Object@state$workerCount < 1) {
    stop(paste('worker group has been closed, and we have', .Object@state$workerCount, 'workers'))
  }

  if (.Object@state$occupied) {
    stop('sleigh is occupied')
  }

  if (.Object@state$stopped) {
    stop('sleigh is stopped')
  }

  fun <- fun # need to force the argument (NJC: why?)

  nws = .Object@nws
  wc = .Object@state$workerCount

  argPermute = NULL
  blocking = TRUE
  lf = 0
  by = "row"
  chunkSize = 1
  accumulator = NULL
  elementFunc = NULL
  closure = NULL
  if (!is.null(eo)) {
    if (is.environment(eo) || is.list(eo)) {
      argPermute = eo$argPermute
      if (!is.null(eo$blocking)) blocking = as.logical(eo$blocking)
      if (!is.null(eo$loadFactor)) lf = as.numeric(eo$loadFactor)
      if (!is.null(eo$by)) by = match.arg(eo$by, c('row', 'column', 'cell'))
      if (!is.null(eo$chunkSize)) chunkSize = max(eo$chunkSize, 1)
      accumulator = eo$accumulator
      elementFunc = eo$elementFunc
      if (!is.null(eo$closure)) closure = as.logical(eo$closure)

      # check for unknown options
      if (is.list(eo)) {
        eo$argPermute <- eo$blocking <- eo$loadFactor <- eo$by <-
          eo$chunkSize <- eo$accumulator <- eo$elementFunc <- eo$closure <- NULL
        if (length(eo) > 0)
          warning('ignoring unknown option(s): ',
            paste('"', names(eo), '"', sep='', collapse=', '))
      }
    }
    else {
      stop('options arg must be a list or environment')
    }
  }

  # issue a warning if fun seems like a closure, and they aren't
  # explicitly enabled via the closure option.
  if (is.null(closure)) {
    closure <- TRUE
    if (isClosure(fun))
      warning('"fun" argument looks like a closure without enabling ',
        'via closure option', immediate.=TRUE)
  }

  # remove the enclosing environment of the function if closures are not
  # allowed.
  if (!closure)
    environment(fun) <- globalenv()

  if (!is.list(elementArgs)) elementArgs = list(elementArgs)
  if (!is.list(fixedArgs)) fixedArgs = list(fixedArgs)
  # FixedArgs Optimization - open context
  if ((.Object@state$workspaceVersion >= 1) & (length(fixedArgs) > 0)
      & (object.size(fixedArgs) > .Object@options$fixedargSize)) {
    for(i in seq(length(fixedArgs))) {
      fid <- nwsFetch(nws,'fixedargID')
      nwsStore(nws,fid,fixedArgs[[i]])
      fixedArgs[i] <- list(fixedargHolder(fid))
    }
  }

  if (length(elementArgs) > 0) {
    if (!is.null(elementFunc)) stop('elementFunc cannot be used with elementArgs')

    allTasks = unlist(lapply(elementArgs, countElement, by=by))
    # this allows for functions to be included, even though they aren't now
    numTasks = max(-1, allTasks, na.rm=TRUE)
    if (numTasks < 0) {
      numTasks = NA
    }
    else {
      # cat('got', numTasks, 'tasks\n')

      # check the length of the arguments
      for (taskLen in allTasks) {
        if (!is.na(taskLen) && numTasks %% taskLen != 0) {
          warning('elementArgs contains arguments of inconsistent length')
          break
        }
      }

      # update the total number of submitted tasks
      .Object@state$totalTasks <- .Object@state$totalTasks + numTasks
    }
  }
  else if (!is.null(elementFunc)) {
    numTasks = NA
    nargs = length(formals(elementFunc))
    if (nargs > 2) stop('specified elementFunc function takes too many arguments')
    startingTasks = .Object@state$totalTasks
  }
  else {
    stop('either elementArgs or elementFunc must be specified')
  }

  if (blocking && lf > 0) {
    submitLimit = lf * wc
    if (submitLimit < wc) {
      submitLimit = wc
    }
  }
  else {
    submitLimit = Inf
  }

  allSubmitted = FALSE
  numSubmitted = 0
  numReturned = 0

  tag = 1
  currentTasks = 0

  val <- if (is.null(accumulator)) list() else NULL
  accumargs = try(length(formals(accumulator)))

  # allocate a new job id
  job = .Object@state$job
  .Object@state$job = job + 1

  while (!allSubmitted || numReturned < numSubmitted) {
    if (!allSubmitted) {
      while (!allSubmitted && numSubmitted < submitLimit) {
        if (!is.null(elementFunc)) {
          argchunk = list()
          for (j in 1:chunkSize) {
            varArgs <- tryCatch({
                if (nargs == 0)
                  elementFunc()
                else if (nargs == 1)
                  elementFunc(currentTasks + 1)
                else
                  elementFunc(currentTasks + 1, by)
              }, error=function(e) {
                allSubmitted <<- TRUE
                if (any(nzchar(e$message)))
                  warning(e$message)
                NULL
              })
            if (allSubmitted) break
            if (!is.list(varArgs)) varArgs = list(varArgs)
            args = c(varArgs, fixedArgs)
            if (!is.null(argPermute)) args = args[argPermute]
            # cat('[function case] args:', paste(args, collapse=' '), '\n')
            argchunk[[j]] = args
            currentTasks = currentTasks + 1
          }
        }
        else {
          nTasks = min(numTasks - currentTasks, chunkSize)
          if (nTasks <= 0) {
            argchunk = list()
            allSubmitted = TRUE
          }
          else {
            if (nTasks > 1) {
              v = currentTasks:(currentTasks + nTasks - 1)
              varArgsChunk = lapply(1:length(elementArgs), function(j) {
                iv <- if (is.na(allTasks[j])) v + 1 else v %% allTasks[j] + 1
                getChunk(elementArgs[[j]], iv=iv, by=by)
              })
              argchunk = lapply(1:nTasks, function(i) {
                varArgs = lapply(varArgsChunk, getElement, i=i, by=by)
                args = c(varArgs, fixedArgs)
                if (!is.null(argPermute)) args = args[argPermute]
                # cat('[chunk case] args:', paste(args, collapse=' '), '\n')
                args
              })
            }
            else {
              varArgs = lapply(1:length(elementArgs), function(j) {
                i <- if(is.na(allTasks[j])) currentTasks + 1 else currentTasks %% allTasks[j] + 1
                getElement(elementArgs[[j]], i=i, by=by)
              })
              args = c(varArgs, fixedArgs)
              if (!is.null(argPermute)) args = args[argPermute]
              # cat('[standard case] args:', paste(args, collapse=' '), '\n')
              argchunk = list(args)
            }
            currentTasks = currentTasks + nTasks
          }
        }

        if (length(argchunk) > 0) {
          numSubmitted = numSubmitted + 1
          storeTask(nws, fun, argchunk, tag=tag, barrier=FALSE, job=job)
          tag = tag + length(argchunk)
        }
      }

      if (!is.null(elementFunc)) {
        # update the total number of submitted tasks
        .Object@state$totalTasks <- startingTasks + currentTasks
      }

      if (!blocking) {
        if (!.Object@newSleigh)
          .Object@state$occupied = TRUE
        # cat(sprintf('returning sleighPending object for %d tasks\n', nt))

        # This is giving some contextual information that the sleighPending
        # constructor is expecting.  It is possible that at some point we
        # will want to move this kind of computation into the sleigh-internals.
        # PS 7/09
        fixedArgRep <- if(length(fixedArgs) > 0) {
          if (is(fixedArgs[[1]], 'fixedargHolder')) {fixedArgs}
          else {list()}
        }
        else {list()}

        func = if (is.null(accumulator)) as.function(list(NULL)) else accumulator
        return (new('sleighPending', nws, currentTasks, numSubmitted, func, '',
                    .Object@state, .Object@newSleigh, job, fixedArgRep))
      }
    }

    if (numReturned < numSubmitted) {
      repeat {
        p = nwsFetch(nws, 'result', metadata=list(batchId=as.character(job)),
                     pkgResult=TRUE)
        r = p$data
        metadata = p$metadata

        # ignore everything but 'VALUE' messages
        if (is.list(r) && r$type == 'VALUE') break

        if (!is.null(metadata)) {
          warning(sprintf('ignoring unexpected result with metadata: %s',
                          paste('"', names(metadata), '"', sep='', collapse=', ')))
        }
      }
      if (is.null(accumulator)) {
        val[r$tag:(r$tag + length(r$value) - 1)] = r$value
      }
      else {
        if (accumargs == 0)
          accumulator()
        else if (accumargs == 1)
          accumulator(r$value)
        else
          accumulator(r$value, r$tag:(r$tag + length(r$value) - 1))
      }
      numReturned = numReturned + 1
      submitLimit = submitLimit + 1  # this can become > the number of tasks
    }
  }

  # FixedArgs Optimization - close context
  for (i in fixedArgs) {
    if (class(i) == 'fixedargHolder') {
      nwsDeleteVar(nws,i@name)
    }
  }
  
  if (is.null(accumulator)) length(val) = currentTasks

  val
})


setGeneric('rankCount', function(.Object) standardGeneric('rankCount'))
setMethod('rankCount', 'sleigh', function(.Object) .Object@state$rankCount)

setGeneric('workerCount', function(.Object) standardGeneric('workerCount'))
setMethod('workerCount', 'sleigh', function(.Object) .Object@state$workerCount)

setGeneric('netWorkSpaceObject', function(.Object) standardGeneric('netWorkSpaceObject'))
setMethod('netWorkSpaceObject', 'sleigh', function(.Object) .Object@nws)

wsVarName <- function(name, worker) {
  if (is.null(worker)) {
    sprintf('env_%s', name)
  } else {
    sprintf('env_%d_%s', worker, name)
  }
}

setGeneric('export',
           function(.Object, xName, xVal, worker=NULL) standardGeneric('export'))
setMethod('export', 'sleigh',
function(.Object, xName, xVal, worker=NULL) {
  # sleigh error checking
  if (.Object@state$occupied) stop('sleigh is occupied')
  if (.Object@state$stopped) stop('sleigh is stopped')

  # argument error checking
  if (missing(xName)) stop('no value specified for xName argument')
  if (missing(xVal)) stop('no value specified for xVal argument')
  if (! is.character(xName)) stop('xName must be a character variable')
  if (! is.null(worker)) {
    if (! is.numeric(worker)) stop('worker value must be numeric')
    if (length(worker) > 1) stop('only one worker can be specified')
    if (worker < 0) stop('worker value must be positive')
    if (worker >= .Object@state$workerCount)
      stop('worker value is too large for this sleigh')
  }

  wsVar <- wsVarName(xName, worker)
  nwsDeclare(.Object@nws, wsVar, 'single')
  nwsStore(.Object@nws, wsVar, xVal)
  nwsStore(.Object@nws, 'exported', list(worker=worker, name=xName, wsVar=wsVar))
  invisible(NULL)
})

setGeneric('unexport',
           function(.Object, xName, worker=NULL) standardGeneric('unexport'))
setMethod('unexport', 'sleigh',
function(.Object, xName, worker=NULL) {
  # sleigh error checking
  if (.Object@state$occupied) stop('sleigh is occupied')
  if (.Object@state$stopped) stop('sleigh is stopped')

  # argument error checking
  if (missing(xName)) stop('no value specified for xName argument')
  if (! is.character(xName)) stop('xName must be a character variable')
  if (! is.null(worker)) {
    if (! is.numeric(worker)) stop('worker value must be numeric')
    if (length(worker) > 1) stop('only one worker can be specified')
    if (worker < 0) stop('worker value must be positive')
    if (worker >= .Object@state$workerCount) stop('worker value is too large')
  }

  wsVar <- wsVarName(xName, worker)
  tryCatch({
    nwsDeleteVar(.Object@nws, wsVar)
  }, error=function(e) {
    stop('cannot unexport a variable that was not exported: ', wsVar, call.=FALSE)
  })
  nwsStore(.Object@nws, 'exported', list(worker=worker, name=xName, wsVar=NULL))
  invisible(NULL)
})

setGeneric('workerInfo',
           function(.Object) standardGeneric('workerInfo'))
setMethod('workerInfo', 'sleigh',
function(.Object) {
  n <- .Object@state$workerCount
  host <- as.character(rep(NA, n))
  os <- as.character(rep(NA, n))
  pid <- as.integer(rep(NA, n))
  R <- as.character(rep(NA, n))
  nws <- as.character(rep(NA, n))
  rank <- as.integer(rep(NA, n))
  logfile <- as.character(rep(NA, n))

  it <- nwsIFindTry(.Object@nws, 'worker info')
  x <- it()
  while (! is.null(x)) {
    i <- as.integer(x$rank) + 1
    host[i] <- x$host
    os[i] <- x$os
    pid[i] <- as.integer(x$pid)
    R[i] <- x$R
    nws[i] <- x$nws
    rank[i] <- as.integer(x$rank)
    logfile[i] <- x$logfile
    x <- it()
  }
  data.frame(host=host, os=os, pid=pid, R=R, nws=nws,
             rank=rank, logfile=logfile, stringsAsFactors=FALSE)
})

if (!isGeneric('view'))
  setGeneric('view',
             function(.Object, ...) standardGeneric('view'))
setMethod('view', 'sleigh',
function(.Object, ws=c('system', 'user'), ...) {
  ws <- match.arg(ws)
  wsName <- switch(ws,
                   user = .Object@userNwsName,
                   .Object@nwsName)
  host <- nwsHost(.Object@options$serverInfo)
  port <- nwsWebPort(.Object@nwss)
  if (is.null(port))
    stop('the nws server does not have a web interface')

  vstring <- sprintf('http://%s:%d/doit?op=listVars&wsName=%s',
                     host, port, URLencode(wsName))
  browseURL(vstring)
  cat(sprintf("Viewing sleigh %s workspace '%s' on server '%s:%s' in your browser\n",
              ws, wsName, host, port))
  invisible(vstring)
})

setGeneric('userNws', function(.Object) standardGeneric('userNws'))
setMethod('userNws', 'sleigh', function(.Object) .Object@userNws)

setGeneric('connect',
function(.Object, master=Sys.info()[['nodename']], port=8787)
  standardGeneric('connect'))
setMethod('connect', 'sleigh',
function(.Object, master=Sys.info()[['nodename']], port=8787) {
  # XXX check port somehow?
  opts <- list(blocking=FALSE)
  sp <- eachWorker(.Object, connectionWorker, master, port, eo=opts)

  # set the timeout for socket connections to be 30 days
  one.month <- 30 * 24 * 60 * 60
  orig.timeout <- options(timeout=one.month)
  on.exit(options(orig.timeout))

  cons <- vector('list', workerCount(.Object))
  for (i in seq(along=cons)) {
    nwsStore(.Object@nws, sprintf('connect_%d', i - 1), 't')
    cons[i] <- list(socketConnection(port=port, server=TRUE,
                                     blocking=TRUE, open='a+b'))
  }

  # check the results for any errors
  results <- waitSleigh(sp)
  for (r in results) {
    if (! is.null(r)) {
      if ('message' %in% names(r))
        stop('worker error: ', r$message)
      else
        stop('worker error: ', as.character(r))
    }
  }

  # return the list of socket connection objects
  .Object@state$cons <- cons
})

connectionWorker <- function(master, port) {
  nws <- get('SleighNws', envir=globalenv())
  rank <- get('SleighRank', envir=globalenv())
  ackVar <- sprintf('connect_%d', rank)
  cat(sprintf('rank: %d, ackVar: %s\n', rank, ackVar), file=stderr())
  x <- nwsFetch(nws, ackVar)
  cat(sprintf('fetch value: %s\n', x), file=stderr())
  nwsDeleteVar(nws, ackVar)

  # set the timeout for socket connections to be 30 days
  one.month <- 30 * 24 * 60 * 60
  orig.timeout <- options(timeout=one.month)
  on.exit(options(orig.timeout))

  # try to create the socket connection up to four times
  con <- NULL
  for (i in 1:4) {
    tryCatch({
        con <- socketConnection(master, port=port, blocking=TRUE, open='a+b')
        cat("created socketConnection\n", file=stderr())
        break
      }, error=function(e) {
        cat('caught error calling socketConnection: ',
            as.character(e), file=stderr())
        Sys.sleep(2)
      })
  }

  assign('SleighSock', con, envir=globalenv())
  cat("SleighSock assigned\n", file=stderr())
  invisible(NULL)
}
