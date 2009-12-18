marshal.list <- function(d, raw=TRUE) {
  x <- if (is.null(d)) {
      '0000'
    } else {
      nm <- names(d)
      ind <- which(nzchar(nm))
      if (any(duplicated(nm[ind])))
        stop('duplicate list names')
      nmlen <- nchar(nm)
      dlen <- nchar(d)
      x <- lapply(ind, function(i) sprintf('%04d%s%04d%s',
          nmlen[i], nm[i], dlen[i], d[[i]]))
      n <- length(d)
      paste(sprintf('%04d', n), paste(x, collapse=''), sep='')
    }

  if (raw) charToRaw(x) else x
}

marshal.vec <- function(d, extra.args=0, raw=TRUE) {
  x <- if (is.null(d)) {
      sprintf('%04d', extra.args)
    } else {
      dlen <- nchar(d)
      x <- lapply(seq(along=d), function(i) sprintf('%020d%s', dlen[i], d[[i]]))
      n <- length(d) + extra.args
      paste(sprintf('%04d', n), paste(x, collapse=''), sep='')
    }

  if (raw) charToRaw(x) else x
}

receive.name <- function(s) {
  len <- as.integer(nwsRecvN(s, 4))
  nwsRecvN(s, len)
}

receive.list <- function(s) {
  num.opts <- as.integer(nwsRecvN(s, 4))
  propose.opts <- list()
  for (i in seq(length=num.opts)) {
    name <- receive.name(s)
    value <- receive.name(s)
    propose.opts[[name]] <- value
  }
  propose.opts
}

negotiate.deadman <- function(s, verbose=FALSE) {
  writeBin(NegotiationHandshakeInit, s)
  handshake <- nwsRecvN(s, 4, rawflag=TRUE)
  if (identical(handshake, NegotiationHandshakePropose)) {
    # it's the new protocol:
    # get the server's proposal, and ignore it
    proposal.opts <- receive.list(s)

    # ask for a deadman connection
    opts <- list()
    opts[[OPT.DEADMAN]] <- '1'
    d <- c(NegotiationHandshakeRequest, marshal.list(opts))
    if (nzchar(Sys.getenv('NWS_VERYVERBOSE')))
      cat(sprintf('sending options to server: %s\n', rawToChar(d)))
    writeBin(d, s)

    # get the final handshake
    acc <- nwsRecvN(s, 4, rawflag=TRUE)
    if (!identical(acc, NegotiationHandshakeAccept)) {
      # probably configured not to shutdown at our whim
      cat("server didn't accept our request\n")
    }
  } else {
    cat("server doesn't support the new protocol\n")
  }
}
