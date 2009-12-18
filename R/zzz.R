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

.nwsGlobals <- new.env()

nwsPkgInfo <- function() {
  c(name=.nwsGlobals$pkgName,
      version=packageDescription(.nwsGlobals$pkgName, fields='Version'))
}

.onLoad <- function(lib, pkg) {
  require(methods)
  require(utils)
  pkgpath <- file.path(lib, pkg)

  initServerEnv(pkgpath)

  blendOptions(defaultSleighOptions, computeDefaultSleighOptions(pkgpath))
  blendOptions(defaultServerInfoOptions,
               computeDefaultServerInfoOptions())
  blendOptions(defaultManagedServerInfoOptions,
               computeDefaultManagedServerInfoOptions())
  blendOptions(defaultNwsServerOptions, computeDefaultNwsServerOptions())
  blendOptions(defaultNetWorkSpaceOptions, computeDefaultNetWorkSpaceOptions())

  .nwsGlobals$pkgName <- pkg
}

