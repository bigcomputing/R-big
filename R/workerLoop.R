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

tostr <- function(obj) {
  rval <- NULL
  tc <- textConnection('rval', open='w')
  sink(tc)
  on.exit({sink(); close(tc)})
  print(obj)
  paste(rval, collapse='\n')
}

logMsg <- function(..., var) {
  msg <- sub('[[:space:]]+$', '', paste(..., sep='\n'))
  cat(msg, '\n')
  flush.console()
  logmsg <- try(sprintf('[%s] %s -- %s', date(),
    get('SleighName', globalenv()), msg))
  nwsStore(get('SleighNws', globalenv()), var, logmsg)
  invisible(NULL)
}

logError <- function(...) {
  logMsg(..., var='logError')
}

logDebug <- function(...) {
  logMsg(..., var='logDebug')
}

importVars <- function(iter, envir=globalenv()) {
  wsVars <- list()
  exp <- iter()
  while (! is.null(exp)) {
    if (is.null(exp$worker) ||
        any(get('SleighRank', globalenv()) %in% exp$worker)) {
      if (is.null(exp$wsVar)) {
        tryCatch({
          remove(list=exp$name, envir=envir)
        },
        warning=function(w) {
        },
        error=function(e) {
          logError(tostr(e))
        })
        wsVars[[exp$name]] <- NULL
      } else {
        wsVars[[exp$name]] <- exp$wsVar
      }
    }
    exp <- iter()
  }

  for (i in seq(along.with=wsVars)) {
    assign(names(wsVars[i]),
           nwsFind(get('SleighNws', globalenv()), wsVars[[i]]), envir)
  }
}

workerLoop <- function(nws, displayName, rank, workerCount, verbose, userNws, rngType,
                       rngSeed) {
  bx <- 1
  lastJob <- -1
  expiter <- tryCatch(nwsIFindTry(nws, 'exported'), error=function(e) NULL)
  
  # put these into global environment so both worker loop and worker
  # code have access
  assign('SleighName', displayName, globalenv())
  assign('SleighNws', nws, globalenv())
  assign('SleighUserNws', userNws, globalenv())
  assign('SleighRank', rank, globalenv())

  loadedRNG <- T
  # this needs to be delayed so that we don't force an immediate join phase
  delayedAssign('SleighWorkerCount', as.integer(nwsFind(nws, 'workerCount')),
                assign.env=globalenv())
  # initialize for random number generation
  setRNGSeed <- function(rngType, rngSeed) {
    if(rngType == 'legacy') {
      logDebug('using legacy random number generation')
      seedval <- (as.integer(rank) + as.numeric(rngSeed))
      set.seed(seedval)
    }
    else if(substr(rngType,1,5) == 'sprng') {
      if (require(rsprng, quietly=TRUE)) {
        logDebug('using rsprng for random number generation')
        if(rngType == 'sprngLFG') gtype <- 0
        else if(rngType == 'sprngLCG') gtype <- 1
        else if(rngType == 'sprngLCG64') gtype <- 2
        else if(rngType == 'sprngCMRG') gtype <- 3
        else if(rngType == 'sprngMLFG') gtype <- 4
        else {
          logError(sprintf('ERROR: This sprng generator type is not supported - shutting down'))
          loadedRNG <- F
        }
        streamno <- rank
        nstream <- workerCount
        seed <- as.numeric(rngSeed)   # XXX should be parameterizable
        param <- 0  # XXX (probably) should be parameterizable
        tryCatch({
          init.sprng(gtype, streamno, nstream, seed, param)
        },
        error=function(e) {
          logError(sprintf('Error calling init.sprng: %s - shutting down',
                           as.character(e)))
          loadedRNG <- F
        })
      }
      else {
        logError(sprintf('ERROR: init.sprng not availible - shutting down'))
        loadedRNG <- F
      }
    }
    else {
      logError(sprintf('ERROR: this rngType is not supported'))
      loadedRNG <- F
    }
  }

  setRNGSeed(rngType, rngSeed)
  if (!loadedRNG)
    break

  repeat {
    # wait for a task to execute
    t <- tryCatch({
           nwsFetch(nws, 'task')
         }, error=function(e) {
           if (e$message != 'retrieval failed')
             logError(sprintf("Error getting task: %s - shutting down",
                              as.character(e)))
           NULL
         })

    if (is.null(t))
      break

    # sanity check
    if (!is.list(t) || t$type != 'EXEC') {
      logError("Bad task: ignoring", tostr(t))
      next
    }

    if (verbose)
      logDebug(sprintf("Got task %s", t$tag))

    if (t$job != lastJob) {
      fixedArgCache <- new.env()
      if (!is.null(expiter)) {
        importVars(expiter)
        lastJob <- t$job
      }
    }

    # Insert any fixedargs as needed
    arg <- t$data$args
    inum <- 0
    for (i in arg) { # Per task in chunk
      inum <- inum + 1
      jnum <- 0
      for (j in i) { # Per arg in task
        jnum <- jnum + 1
        if (is(j,'fixedargHolder')) {
          if (!is.null(fixedArgCache[[j@name]])) {
            fixedArgCache[[j@name]]
          }
          else {
            fixedArgCache[[j@name]] <- nwsFindTry(nws,j@name)
          }
          arg[[inum]][jnum] <- list(fixedArgCache[[j@name]])
        }
      }
    }

    
    # execute the task        
    dotask <- function(i) {
      tryCatch({
        docall(t$data$fun, arg[[i]])
      },
      error = function(e) {
        logError(as.character(e))
        ## would like to figure out a way to log a useful traceback
        ## but that information seems to be lost at this point
        # calls <- sys.calls()
        # logError(paste(limitedLabels(calls), collapse='\n'))
        # rm(calls)
        e
      })
    }

    value <- lapply(seq(arg), dotask)

    if (verbose)
      logDebug(sprintf("Task %s completed", t$tag))

    # send back the task results
    tryCatch({
      nwsStore(nws, 'result', list(type='VALUE', value=value, tag=t$tag,
               job=t$job, resubmitted=t$resubmitted, rank=rank))
    },
    error=function(e) {
      # try to store the error object in case the failure was due to
      # a serialization error.  this will fail also if the workspace
      # has been deleted, or the server has crashed.
      logError(sprintf('Error returning result: %s', as.character(e)))
      nwsStore(nws, 'result', list(type='VALUE', value=e, tag=t$tag,
               job=t$job, resubmitted=t$resubmitted, rank=rank))
    })

    if (t$barrier) {
      nwsFind(nws, barrierNames[[bx]])
      bx <- bx%%2 + 1
    }
  }
}
