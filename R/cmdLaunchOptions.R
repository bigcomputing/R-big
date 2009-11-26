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

sshcmd <- function(host, options) {
  if (is.null(suppressWarnings(nsl(host))))
    warning(sprintf("ssh may not be able to resolve node name '%s'", host),
            call.=FALSE)

  basicArgs <- if (!is.null(options$user))
                 c('ssh', '-f', '-x', '-l', options$user, host)
               else
                 c('ssh', '-f', '-x', host)

  wrapper <- file.path(options$wrapperDir, options$workerWrapper)
  if (file.access(wrapper) == 0) {
    if (length(grep('\\.py$', wrapper, ignore.case=TRUE)) == 1) {
      # hack: stick in extra arguments needed by the BackgroundLaunch.py script
      if (length(grep('BackgroundLaunch', wrapper)) == 1) {
        d <- options$extraPythonModules
        args <- unlist(lapply(d, function(arg, opt) c(opt, arg), '-m'))
        wrapper <- c(wrapper, args, '--')
      }
      if (!is.null(options$python))
        c(options$python, options$pythonOpts, wrapper, basicArgs)
      else
        c('python', options$pythonOpts, wrapper, basicArgs)
    }
    else {
      c(wrapper, basicArgs)
    }
  }
  else {
    basicArgs
  }
}

sshforwardcmd <- function(host, options) {
  if (is.null(suppressWarnings(nsl(host))))
    warning(sprintf("ssh may not be able to resolve node name '%s'", host),
            call.=FALSE)

  if (is.null(options$nwsHostRemote))
    stop('must use the nwsHostRemote option with sshforwardcmd')

  r <- if (nchar(options$nwsHostRemote) > 0)
         sprintf('%s:%d:%s:%d', options$nwsHostRemote, options$nwsPortRemote,
                 options$nwsHost, options$nwsPort)
       else
         sprintf('%d:%s:%d', options$nwsPortRemote,
                 options$nwsHost, options$nwsPort)

  basicArgs <- if (!is.null(options$user))
                 c('ssh', '-f', '-x', '-R', r, '-l', options$user, host)
               else
                 c('ssh', '-f', '-x', '-R', r, host)

  wrapper <- file.path(options$wrapperDir, options$workerWrapper)
  if (file.access(wrapper) == 0) {
    if (length(grep('\\.py$', wrapper, ignore.case=TRUE)) == 1) {
      # hack: stick in extra arguments needed by the BackgroundLaunch.py script
      if (length(grep('BackgroundLaunch', wrapper)) == 1) {
        d <- options$extraPythonModules
        args <- unlist(lapply(d, function(arg, opt) c(opt, arg), '-m'))
        wrapper <- c(wrapper, args, '--')
      }

      if (!is.null(options$python))
        c(options$python, options$pythonOpts, wrapper, basicArgs)
      else
        c('python', options$pythonOpts, wrapper, basicArgs)
    }
    else {
      c(wrapper, basicArgs)
    }
  }
  else {
    basicArgs
  }
}

rshcmd <- function(host, options) {
  if (is.null(suppressWarnings(nsl(host))))
    warning(sprintf("rsh may not be able to resolve node name '%s'", host),
            call.=FALSE)

  basicArgs <- if (!is.null(options$user))
                 c('rsh', host, '-l', options$user, '-n')
               else
                 c('rsh', host, '-n')

  wrapper <- file.path(options$wrapperDir, 'BackgroundLaunch.py')

  d <- options$extraPythonModules
  args <- unlist(lapply(d, function(arg, opt) c(opt, arg), '-m'))
  wrapper <- c(wrapper, args, '--')

  if (!is.null(options$python))
    c(options$python, options$pythonOpts, wrapper, basicArgs)
  else
    c('python', options$pythonOpts, wrapper, basicArgs)
}

lsfcmd <- function(host, options) {
  'bsub'
}

ccscmd <- function(host, options) {
  c("job", "submit", "/exclusive:false")
}

rwincmd <- function(host, options) {
  # Note: Execution of cscript (done locally) must use simple quoting.
  # However, the remote command (the one executed via rwin.vbs) must
  # be done with MSC quoting, since the remote command is presumed to
  # be the Python interpreter.  Therefore, we set quoting to 'simple',
  # but we don't use the rwin.vbs "-s" option.
  wrapper <- file.path(options$wrapperDir, 'rwin.vbs')
  cmd <- if (is.null(options$passwd)) {
      c('cscript', '//nologo', wrapper, host, '--')
    } else {
      user <- if (is.null(options$user))
          Sys.info()[['login']]
        else
          options$user
      c('cscript', '//nologo', wrapper, host, '-l', user,
        '-p', options$passwd, '--')
    }
  list(cmd=cmd, quoting='simple')
}

envcmd <- function(host, envVars, options) {
  args <- if (options$scriptName == 'RNWSSleighWorker.py') {
      d <- options$extraPythonModules
      unlist(lapply(d, function(arg, opt) c(opt, arg), '-m'))
    } else NULL

  c('env', envVars, file.path(options$scriptDir, options$scriptName), args)
}

scriptcmd <- function(host, envVars, options) {
  args <- if (options$scriptName == 'RNWSSleighWorker.py') {
      d <- options$extraPythonModules
      a <- unlist(lapply(d, function(arg, opt) c(opt, arg), '-m'))
      c(a, '--')
    } else NULL

  if (!is.null(options$python))
    c(options$python, options$pythonOpts,
      file.path(options$scriptDir, options$scriptName), args, envVars)
  else
    c('python', options$pythonOpts,
      file.path(options$scriptDir, options$scriptName), args, envVars)
}
