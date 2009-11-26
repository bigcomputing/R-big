path.split <- function(path) {
  v <- strsplit(path, .Platform$path.sep, fixed=TRUE)[[1]]

  # handle a trailing path.sep specially because of the way
  # that strsplit works
  pathc <- strsplit(path, '')[[1]]
  if (length(pathc) > 0) {
    if (pathc[length(pathc)] == .Platform$path.sep)
      v[length(v) + 1] <- ''
  }
  else {
    v[1] <- ''
  }
  v
}

which.cmd <- function(cmd, path, verbose, pathext, allResults=FALSE) {
  # set default values of path and verbose, dealing with undefined
  # environment variables
  if (missing(path)) {
    path <- Sys.getenv('PATH')
    if (!nzchar(path)) path <- character(0)
  }

  if (missing(verbose))
    verbose <- nzchar(Sys.getenv('NWS_VERBOSE'))

  if (missing(pathext)) {
    ext <- paste(.Platform$path.sep, Sys.getenv('PATHEXT'), sep='')
    pathext <- strsplit(ext, .Platform$path.sep, fixed=TRUE)[[1]]
  }

  # split PATH into a vector (if necessary)
  pathv <- if (length(path) == 1) path.split(path) else path

  # change empty strings into dots
  pathv <- ifelse(pathv == '', '.', pathv)

  if (.Platform$OS.type == 'windows') {
    # trying to be consistent with the file.sep character
    pathv <- gsub('\\', .Platform$file.sep, pathv, fixed=TRUE)
  }

  # construct the appropriate file paths, and see if they exist
  cmdv <- file.path(pathv, rep(paste(cmd, pathext, sep=''),
                               each=length(pathv)))
  execv <- file.access(cmdv, 1)  # test for execute permission
  cmdv <- cmdv[execv == 0]

  if (allResults) {
    cmdv
  } else if (length(cmdv) > 0) {
    ## this results in spurious warnings on Windows if Python is
    ## in the registry and your PATH
    # if (length(cmdv) > 1) {
    #   msg <- sprintf('found multiple %s commands in your PATH', cmd)
    #   warning(msg, call.=FALSE)
    # }
    cmdv[1]
  } else {
    NULL
  }
}

pythonInstallations <- function(path, verbose) {
  # set default values of path and verbose, dealing with undefined
  # environment variables
  if (missing(path)) {
    path <- Sys.getenv('PATH')
    if (!nzchar(path)) path <- character(0)
  }

  if (missing(verbose))
    verbose <- nzchar(Sys.getenv('NWS_VERBOSE'))

  # split PATH into a vector (if necessary)
  pathv <- if (length(path) == 1) path.split(path) else path

  # prefer Python install directories to PATH directories
  if (.Platform$OS.type == 'windows') {
    # try to look up install paths of Python in the registry
    tmp <- pythonInstallPaths()

    if (verbose) {
      if (is.null(tmp)) {
        cat('Unable to read from registry in this version of R\n')
      }
      else if (length(tmp) > 0) {
        cat('Found the following Python installations in the registry:\n')
        for (p in tmp)
          cat(sprintf('  %s\n', p))
      }
      else {
        cat('Found no Python installation in the registry\n')
      }
    }
    pathv <- c(tmp, pathv)
  }

  tmp <- Sys.glob(file.path(dirname(R.home()), 'python*'))
  if (length(tmp) > 0) {
    if (verbose) {
      cat('Found the following Python installations next to R_HOME:\n')
      for (p in tmp)
        cat(sprintf('  %s\n', p))
    }
    pathv <- c(tmp, pathv)
  }

  # find all python executables in the path and the corresponding version
  pypath <- which.cmd('python', path=pathv, verbose=verbose, allResults=TRUE)
  nwssversion <- nwssVersion(pypath, verbose)
  pyversion <- pythonVersion(pypath, verbose)
  ord <- tryCatch(order(pyversion), error=function(e) integer(0))

  if (verbose) {
    if (length(pypath) > 0) {
      cat('Found the following Python executables:\n')
      for (i in ord)
        cat(sprintf('  %s [%s]\n', pypath[i], pyversion[i]))
    }
    else {
      cat('Did not find any Python executables\n')
    }
  }

  data.frame(pyversion=pyversion[ord], pypath=pypath[ord],
             nwssversion=nwssversion, stringsAsFactors=FALSE)
}

which.python <- function(path, verbose) {
  p <- pythonInstallations(path, verbose)
  if (nrow(p) > 0) {
    # pick the latest version of Python available
    i <- latestVersion(substring(p$pyversion, 7))
    p$pypath[i[1]]
  } else {
    NULL
  }
}

pythonVersion <- function(pypath, verbose) {
  tfname <- tempfile()
  tryCatch({
    # write a little Python script that prints its version number
    tf <- file(tfname, 'w')
    script <- c(
      'try:',
      '    import platform',
      '    major, minor, patchlevel = platform.python_version_tuple()',
      '    print "python%s.%s" % (major, minor)',
      'except:',
      '    pass'
    )
    writeLines(script, tf)
    close(tf)

    getVersion <- function(pythonexec) {
      # XXX double quoting the arguments seems to cause errors on Windows
      # cmd <- sprintf('%s %s', pythonexec, tfname)
      # XXX I'm giving it another try using argv2str
      cmd <- argv2str(c(pythonexec, '-E', tfname), NULL)
      if (verbose)
        cat(sprintf('executing command: %s\n', cmd))
      p <- pipe(cmd, 'r')
      d <- readLines(p)
      try(close(p))

      i <- grep('^python', d)
      if (length(i) > 0) {
        d[i[1]]
      } else {
        warning(sprintf('unable to determine python version of %s', pythonexec),
                call.=FALSE)
        'UNKNOWN'
      }
    }
    unlist(lapply(pypath, getVersion))
  },
  finally={
    # delete the little Python script
    unlink(tfname)
  })
}

nwssVersion <- function(pypath, verbose) {
  tfname <- tempfile()
  tryCatch({
    # write a little Python script that prints its version number
    tf <- file(tfname, 'w')
    script <- c(
      'try:',
      '    import twisted',
      '    import nwss',
      '    print "nwss%s" % nwss.__version__',
      'except:',
      '    print "nwss0"'
    )
    writeLines(script, tf)
    close(tf)

    getVersion <- function(pythonexec) {
      # XXX double quoting the arguments seems to cause errors on Windows
      # cmd <- sprintf('%s %s', pythonexec, tfname)
      # XXX I'm giving it another try using argv2str
      cmd <- argv2str(c(pythonexec, '-E', tfname), NULL)
      if (verbose)
        cat(sprintf('executing command: %s\n', cmd))
      p <- pipe(cmd, 'r')
      d <- readLines(p)
      try(close(p))

      i <- grep('^nwss', d)
      if (length(i) > 0) substring(d[i[1]], 5) else ''
    }
    unlist(lapply(pypath, getVersion))
  },
  finally={
    # delete the little Python script
    unlink(tfname)
  })
}

if (.Platform$OS.type == 'windows' && exists('readRegistry', mode='function')) {
  pythonInstallPaths <- function() {
    hivefun <- function(hive) {
      p <- tryCatch({
        readRegistry('Software\\REvolution\\ParallelR\\Python\\InstallPath',
                     hive)
      },
      error=function(e) {
        character(0)
      })
      revoReg <- if (length(p) > 0 && nzchar(p[[1]])) p[[1]] else NULL

      versions <- tryCatch({
        readRegistry('Software\\Python\\PythonCore', hive)
      },
      error=function(e) {
        list()
      })
      vnames <- names(versions)
      pathfun <- function(v) {
        key <- sprintf('Software\\Python\\PythonCore\\%s\\InstallPath', v)
        p <- tryCatch({
          readRegistry(key, hive)
        },
        error=function(e) {
          character(0)
        })
        if (length(p) > 0 && nzchar(p[[1]])) p[[1]] else NULL
      }
      pyReg <- unlist(lapply(vnames, pathfun))
      c(revoReg, pyReg)
    }
    unlist(lapply(c('HCU', 'HLM'), hivefun))
  }
} else {
  pythonInstallPaths <- function() NULL
}

# converts a vector of version strings into a list of integer vectors
versionSplit <- function(v) {
  lapply(strsplit(v, '[.-]'), as.integer)
}

# compares two version vectors, represented as character vectors
versionCmp <- function(a, b) {
  if (is.null(a) || is.null(b))
    return(integer(0))

  if (!is.character(a) || !is.character(b))
    stop('versionCmp takes character arguments')

  cmpfun <- function(v1, v2) {
    cmp <- 0L
    n <- max(length(v1), length(v2))
    for (i in seq(length=n)) {
      if (i > length(v1) || i > length(v2)) {
        cmp <- if (i > length(v1)) -1L else 1L
        break
      } else if (v1[i] > v2[i]) {
        cmp <- 1L
        break
      } else if (v1[i] < v2[i]) {
        cmp <- -1L
        break
      }
    }
    cmp
  }
  # mapply goes back to at least R 2.4.0
  r <- mapply(cmpfun, versionSplit(a), versionSplit(b), SIMPLIFY=TRUE)
  if (length(r) > 0) r else integer(0)
}

# returns the indices of the latest version in a character vector
latestVersion <- function(v) {
  if (is.null(v))
    return(integer(0))

  if (!is.character(v))
    stop('latestVersion takes a character argument')

  if (length(v) == 0) {
    integer(0)
  } else {
    latest <- 1L
    for (i in seq(along=v)[-1]) {
      cmp <- versionCmp(v[i], v[latest[1]])
      if (cmp > 0)
        latest <- i
      else if (cmp == 0)
        latest <- c(latest, i)
    }
    latest
  }
}

nwsserverPkgs <- function(verbose) {
  packages <- installed.packages()
  libPath <- unique(packages[packages[,1] == 'nwsserver', 'LibPath'])
  nwssDir <- Sys.glob(file.path(libPath, 'nwsserver*'))

  if (length(nwssDir) > 0) {
    getRecords <- function(pkgDir) {
      descr <- file.path(pkgDir, 'DESCRIPTION')
      pkgInfo <- read.dcf(descr, fields=c('Package', 'Version'))
      nsversion <- pkgInfo[1, 'Version']
      if (pkgInfo[1, 'Package'] == 'nwsserver' && !is.na(nsversion)) {
        getDirs <- function(nsDir) {
          pyversion <- basename(nsDir)
          nspath <- file.path(nsDir, 'Lib')
          data.frame(nsversion=nsversion, pyversion=pyversion, nspath=nspath,
                     stringsAsFactors=FALSE)
        }
        lapply(Sys.glob(file.path(pkgDir, 'python*')), getDirs)
      } else {
        list()
      }
    }
    p <- do.call('rbind', unlist(lapply(nwssDir, getRecords), recursive=FALSE))
    if (verbose) {
      cat('Found the following nwsserver installations:\n')
      print(p)
    }
    p
  } else {
    if (verbose) {
      cat('Found no nwsserver installations\n')
    }
    data.frame(nsversion=character(0), pyversion=character(0), nspath=character(0),
               stringsAsFactors=FALSE)
  }
}

# returns the nwsserver subdirectory and python interpreter path
# needed to start the nws server
pythonpath <- function() {
  verbose <- nzchar(Sys.getenv('NWS_VERBOSE'))
  quiet <- nzchar(Sys.getenv('NWS_QUIET'))
  result <- NULL

  tryCatch({
    # create data frames with information about the Python and
    # nwsserver package installations
    pytable <- pythonInstallations(verbose=verbose)
    nstable <- nwsserverPkgs(verbose=verbose)

    # join the two data frames on the "pyversion" column
    table <- merge(pytable, nstable)

    if (verbose && nrow(table) > 0) {
        cat('Result of merging Python and nwsserver installations:\n')
        print(table)
    }

    # reduce the table to only include the most up to date version of
    # the nwsserver package
    table <- table[latestVersion(table$nsversion),]

    # reduce the table to only include the most up to date version of Python
    table <- table[latestVersion(substring(table$pyversion, 7)),]

    if (nrow(table) > 0 && any(versionCmp(table$nsversion, '2') > 0)) {
      # extract the information we need into a 2-element list
      result <- table[1, c('nspath', 'pypath'), drop=TRUE]

      # mention that there are multiple choices
      if (nrow(table) > 1 && verbose)
        cat(sprintf('Found %d choices of (python, nwsserver) for Python %s\n',
                    nrow(table), substring(table[1, 'pyversion'], 7)))

      if (verbose)
        cat(sprintf('PYTHONPATH="%s" "%s"\n', result$nspath, result$pypath))
    } else if (any(versionCmp(pytable$nwssversion, '2') > 0)) {
      i <- latestVersion(pytable$nwssversion)
      pypath <- pytable$pypath[i[1]]
      result <- list(pypath=pypath, nspath='')

      # warn the user that there are multiple versions of the same package?
      if (length(i) > 1 && !quiet)
        cat(sprintf('Found %d Python installations with nwsserver package version %s\n',
                    length(i), pytable$nwssversion[i[1]]))

      if (verbose)
        cat(sprintf('PYTHONPATH="%s" "%s"\n', result$nspath, result$pypath))
    } else {
      if (!quiet) {
        if (nrow(pytable) == 0)
          cat('Unable to find any Python installations\n')
        else if (nrow(nstable) == 0)
          cat('Unable to find any nwsserver package installations\n')
        else
          cat('Unable to find any nwsserver installations with support',
              'for your Python interpreter\n')
      }
    }
  },
  error=function(e) {
    if (!quiet)
      cat(sprintf('caught exception: %s\n', conditionMessage(e)))
  })

  result
}
