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

.sleighEnv <- new.env()

getSleigh <- function(name='default') {
  k <- paste(name, 'Sleigh', sep='')
  o <- paste(name, 'Own', sep='')

  # create a sleigh and register it if there is no registered
  # sleigh, or if it has been stopped
  if (is.null(.sleighEnv[[k]]) || .sleighEnv[[k]]@state$stopped) {
    tryCatch({
      .sleighEnv[[k]] <- sleigh()
      .sleighEnv[[o]] <- TRUE
    },
    error=function(e) {
      warning('unable to create sleigh object, ',
              'possibly because NWS server not available',
              call.=FALSE)
    })
  }
  .sleighEnv[[k]]
}

setSleigh <- function(s, name='default') {
  k <- paste(name, 'Sleigh', sep='')
  o <- paste(name, 'Own', sep='')
  if (!is.null(.sleighEnv[[o]]) && .sleighEnv[[o]]) {
    stopifnot(!is.null(.sleighEnv[[k]]))
    stopSleigh(.sleighEnv[[k]])
  }
  .sleighEnv[[k]] <- s
  .sleighEnv[[o]] <- FALSE
  invisible(NULL)
}
