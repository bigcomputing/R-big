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

# internal functions for Sleigh class

# enquote and docall copied verbatim from snow
enquote <- function(x) as.call(list(as.name('quote'), x))

docall <- function(fun, args) {
  if ((is.character(fun) && length(fun) == 1) || is.name(fun))
    fun <- get(as.character(fun), env = .GlobalEnv, mode = 'function')
  do.call('fun', lapply(args, enquote))
}

# could this be a method --- it is invoked in the constructor?
addWorker <- function(machine, wsName, userWsName, id, workerCount,
                      numProcs, options) {
  # basic idea is (or should be): if we can get the appropriate
  # worker script running on the remote node, we just need to
  # give it enough env info to take care of the rest
  nwsHost = if (is.null(options$nwsHostRemote))
              options$nwsHost
            else if (options$nwsHostRemote == '')
              'localhost'
            else
              options$nwsHostRemote
  nwsPort = if (is.null(options$nwsPortRemote))
              options$nwsPort
            else
              options$nwsPortRemote

  envVars = list(
    paste('RSleighName=', machine, sep=''),
    paste('RSleighNwsName=', wsName, sep=''),
    paste('RSleighUserNwsName=', userWsName, sep=''),
    paste('RSleighID=', id, sep=''),
    paste('RSleighWorkerCount=', workerCount, sep=''),
    paste('RSleighScriptDir=', options$scriptDir, sep=''),
    paste('RSleighNwsHost=', nwsHost, sep=''),
    paste('RSleighNwsPort=', nwsPort, sep=''),
    paste('RSleighWorkingDir=', options$workingDir, sep=''),
    paste('RSleighNumProcs=', as.character(numProcs), sep=''),
    paste('RProg=', options$rprog, sep=''),
    paste('RSleighRNGType=', options$rngType, sep=''),
    paste('RSleighRNGSeed=', options$rngSeed, sep='')
  )

  elen = length(envVars)

  if (!is.null(options$outfile)) {
    elen = elen + 1
    envVars[[elen]] = paste('RSleighWorkerOut=', options$outfile, sep='')
  }

  if (!is.null(options$logDir)) {
    elen = elen + 1
    envVars[[elen]] = paste('RSleighLogDir=', options$logDir, sep='')
  }

  quoting = NULL  # use the default quoting-style

  if (is.character(options$launch) && options$launch == 'local') {
    background <- file.path(options$wrapperDir, 'BackgroundLaunch.py')
    if (!is.null(options$python)) {
      launchcmd = c(options$python, options$pythonOpts, background)
    }
    else {
      launchcmd = c('python', options$pythonOpts, background)
    }

    d = options$extraPythonModules
    args = unlist(lapply(d, function(opt, arg) c(arg, opt), '-m'))
    launchcmd = c(launchcmd, args, '--')
    workerstart = options$scriptExec(machine, envVars, options)
  }
  else if (is.function(options$launch)) {
    launch = options$launch(machine, options)
    if (is.list(launch) && !is.null(launch$cmd)) {
      # a list allows the quoting-style to be specified
      launchcmd = launch$cmd
      quoting = launch$quoting
    }
    else {
      launchcmd = launch
    }

    # XXX do we need to add an extra level of quoting to workerstart?
    # XXX for ssh? or rsh?
    workerstart = options$scriptExec(machine, envVars, options)
  }

  argv = c(launchcmd, workerstart)
  cmd = argv2str(argv, quoting)
  if (options$verbose) cat("Executing command: ", cmd, "\n")
  system(cmd)
}

storeTask <- function(nws, fun, args,
                      tag = 'anon', barrier = FALSE, return = TRUE, job = -1,
                      varName = 'task') {
  metadata = if (job >= 0) list(batchId=as.character(job)) else list()
  nwsStore(nws, varName,
           list(type='EXEC',
                barrier=barrier,
                data=list(fun=fun, args=args, return=return),
                tag=tag,
                job=job),
           metadata=metadata)
}

## This is necessary since x[i] gets the i'th *column* of a data
## frame object rather than the i'th cell
dfGetElement <- function(x, obs) {
  row <- (obs - 1) %% nrow(x) + 1
  col <- floor((obs - 1) / nrow(x)) + 1
  x[cbind(row, col)]  # pair (r,c) to select elements
}

getChunk <- function(x, iv, by) {
  if (is.matrix(x))
    switch(by, "row"=x[iv,,drop=FALSE], "column"=x[,iv,drop=FALSE], "cell"=x[iv])
  else if (is.data.frame(x))
    switch(by, "row"=x[iv,,drop=FALSE], "column"=x[,iv,drop=FALSE],
        "cell"=dfGetElement(x,iv))
  else
    # this case works for vectors and lists
    x[iv]
}

getElement <- function(x, i, by) {
  if (is.matrix(x))
    switch(by, "row"=x[i,,drop=TRUE], "column"=x[,i,drop=TRUE], "cell"=x[i])
  else if (is.data.frame(x))
    switch(by, "row"=x[i,,drop=TRUE], "column"=x[,i,drop=TRUE], "cell"=dfGetElement(x,i))
  else if (is.list(x))
    x[[i]]
  else
    # this case only works for vectors
    x[i]
}

countElement <- function(x, by) {
  if(is.matrix(x))
    switch(by, "row"=nrow(x), "column"=ncol(x), "cell"=length(x))
  else if (is.data.frame(x))
    switch(by, "row"=nrow(x), "column"=ncol(x), "cell"=nrow(x) * ncol(x))
  else
    length(x)
}

msc.quote <- function(arg) {
  # argument needs quoting if it contains whitespace or a double-quote
  if (length(arg) != 1)
    stop('length of arg must be 1')

  if (!nzchar(arg)) {
    # empty strings must be quoted
    '""'
  } else if (length(grep('[[:space:]"]', arg)) == 0) {
    arg
  }
  else {
    q <- '"'
    nbs <- 0
    v <- strsplit(arg, split='')[[1]]
    for (c in v) {
      if (c == '\\') {
        q <- paste(q, c, sep='')
        nbs <- nbs + 1
      }
      else if (c == '"') {
        q <- paste(q, paste(rep('\\', nbs + 1), collapse=''), c, sep='')
        nbs <- 0
      }
      else {
        q <- paste(q, c, sep='')
        nbs <- 0
      }
    }

    paste(q, paste(rep('\\', nbs), collapse=''), '"', sep='')
  }
}

simple.quote <- function(arg) {
  if (length(arg) != 1)
    stop('length of arg must be 1')

  if (!nzchar(arg)) {
    # empty strings must be quoted
    '""'
  } else if (length(grep('"', arg)) > 0) {
    stop('arguments cannot contain double quotes with simple quoting')
  }
  else if (length(grep('[[:space:]]', arg)) == 0) {
    # argument without whitespace and double-quotes don't need quoting
    arg
  }
  else {
    paste('"', arg, '"', sep='')
  }
}

posix.quote <- function(arg) {
  if (length(arg) != 1)
    stop('length of arg must be 1')

  q <- "'"
  v <- strsplit(arg, split='')[[1]]
  for (c in v) {
    if (c == "'") {
      c <- "'\\''"
    }
    q <- paste(q, c, sep='')
  }

  paste(q, "'", sep='')
}

argv2str <- function(argv, quoting) {
  qfun <- if (is.null(quoting)) {
      if (.Platform$OS.type == 'windows')
        msc.quote
      else
        posix.quote
    }
    else if (quoting == 'posix')
      posix.quote
    else if (quoting == 'msc')
      msc.quote
    else if (quoting == 'simple')
      simple.quote
    else
      stop('unrecognized quoting method: ', quoting)

  paste(lapply(argv, qfun), collapse=' ')
}

# This class is used for the fixedarg opt
setClass('fixedargHolder',
         representation(name='character'))

setMethod('initialize', 'fixedargHolder',
          function(.Object, key) {
            .Object@name=key
            .Object
          })

fixedargHolder <- function(...) {
  new("fixedargHolder",...)
}
