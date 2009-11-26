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

cmdLaunch <- function(verbose=FALSE) {
  if (is.na(verbose))
    verbose <- FALSE

  nwsName <- Sys.getenv('RSleighNwsName')
  userNwsName <- Sys.getenv('RSleighUserNwsName')
  nwsHost <- Sys.getenv('RSleighNwsHost')
  nwsPort <- as.integer(Sys.getenv('RSleighNwsPort'))
  rank <- as.integer(Sys.getenv('RSleighRank', '-1'))
  maxWorkerCount <- as.integer(Sys.getenv('RSleighWorkerCount'))
  name <- Sys.getenv('RSleighName')
  rngType <- Sys.getenv('RSleighRNGType')
  rngSeed <- Sys.getenv('RSleighRNGSeed')
  numProcs <- as.integer(Sys.getenv('RSleighNumProcs'))
  localID <- as.integer(Sys.getenv('RSleighLocalID'))
  launch(nwsName, nwsHost, nwsPort, rank, maxWorkerCount, name,
         verbose, userNwsName, rngType, rngSeed, numProcs, localID)
}


launch <- function(nwsName, nwsHost, nwsPort, rank=-1, maxWorkerCount=-1,
      name=Sys.info()['nodename'], verbose=FALSE, userNwsName='__default',
                   rngType='legacy', rngSeed=0, numProcs=1, localID=-1) {
  si <- serverInfo(host=nwsHost, port=nwsPort)
  nws <- netWorkSpace(nwsName, serverInfo=si, useUse=TRUE, create=FALSE)    
  userNws <- nwsUseWs(nws@server, userNwsName, create=FALSE)

  # initialize for monitoring
  displayName = sprintf('%s@%d', name, rank)

  # post some info about this worker
  logfile <- Sys.getenv('RSleighLogFile')
  names(logfile) <- NULL
  info <- Sys.info()
  nwsVersion <- paste(nwsPkgInfo(), collapse=' ')
  nwsStore(nws, 'worker info',
      list(host=info[['nodename']],
           os=info[['sysname']],
           pid=Sys.getpid(),
           R=R.version.string,
           nws=nwsVersion,
           rank=rank,
           logfile=logfile))

  # XXX only do this with a sleigh workspace?
  nwsStore(nws, 'worker_ids', as.character(rank))

  # Figure out if this Revo R, if so numThreads
  if(require(Revobase,quiet=TRUE)) {
    tryCatch({
      numCores <- getMKLthreads()
      numThreads <- floor(numCores/numProcs) +
        if((numCores %% numProcs) > rank) 1 else 0
      setMKLthreads(max(numThreads,1))
    }, error=function(e) {
      warning('Revobase appears to exist, but cannot set MKL thread count.',
              call.=FALSE)
      print(e)
    })
  }
  
  # enter the main worker loop
  workerLoop(nws, displayName, rank, maxWorkerCount, verbose, userNws, rngType,
             rngSeed)

  # indicate exit.
  # XXX not sure if this makes any sense
  nwsStore(nws, 'bye', 1)
}
