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

Sys.setenv(NWS_QUIET='TRUE')
Sys.setenv(NWS_VERBOSE='')

library(nws)

maxlen <- 10000
serverHost <- Sys.getenv('NWS_SERVERHOST', 'localhost')
port <- suppressWarnings(as.integer(Sys.getenv('NWS_PORT', '8765')))
if (is.na(port))
  stop('the NWS_PORT variable must be a valid integer', call.=FALSE)

args <- commandArgs(TRUE)
i <- 1
while (i <= length(args)) {
  if (args[i] == '-h') {
    i <- i + 1
    if (i > length(args))
      stop('the -h option takes a required argument', call.=FALSE)
    serverHost <- args[i]
  }
  else if (args[i] == '-p') {
    i <- i + 1
    if (i > length(args))
      stop('the -p option takes a required argument', call.=FALSE)
    port <- suppressWarnings(as.integer(args[i]))
    if (is.na(port))
      stop('the -p option takes an integer argument', call.=FALSE)
  }
  else {
    stop('unrecognized argument: ', args[i], call.=FALSE)
  }
  i <- i + 1
}

si <- serverInfo(host=serverHost, port=port)
bws <- netWorkSpace('R babelfish', serverInfo=si)

status <- 0
while (status == 0) {
  # watch out for errors unserializing values
  s <- tryCatch({
      paste(capture.output(nwsFetch(bws, 'food')), collapse='\n')
    }, error=function(e) {
      sprintf('babelfish error: %s', try(e$message, silent=TRUE))
    })

  # truncate really long output values
  if (nchar(s) > maxlen) {
    s <- paste(substr(s, 1, maxlen), '[WARNING: output truncated]')
  }

  # send the string representation back to the web interface.
  # an error normally means that the workspace is gone, perhaps
  # because the nws server shutdown.
  status <- tryCatch({
      nwsStore(bws, 'doof', s)
      0
    }, error=function(e) {
      -1
    })
}
