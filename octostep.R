# octostep

# convenience helper
any.null <- function(...) {
  if (missing(...)) stop('no input')
  any(sapply(list(...), function(arg) is.null(arg)))
}

# main
octostep <- function(x, func) {
  stopifnot(is.list(x), length(x) > 3L, 
            is.function(func), length(formals(func)) == 3L)
  y <- vector('list', length(x))
  names(y) <- names(x)
  i <- 1L
  repeat {
    if (i > length(x)) break
    rtn <- func(if (i > 1L) x[[i - 1L]] else NULL, 
                x[[i]], 
                if (i < length(x)) x[[i + 1L]] else NULL)
    if (!is.null(rtn)) y[[i]] <- rtn
    i <- i + 1L
  }
  # exit
  return(y)
}

# xample
octo <- octostep(as.list(1L:100L), function(pre, cur, nxt) {
  if (any.null(pre, nxt)) NULL else pre + cur + nxt
})
