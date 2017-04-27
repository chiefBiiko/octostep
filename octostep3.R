# octostep

# main
octostep <- function(x, func, pad=1L) {
  stopifnot(length(x) >= 2L * pad + 1L, is.function(func), 
            length(formals(func)) == 2L * pad + 1L)
  y <- vector('list', length(x))  # preallocate return
  names(y) <- names(x)
  # iterate
  i <- 1L
  repeat {
    if (i > length(x)) break  # trapdoor
    # build argument list for current callback
    z <- lapply(1L:(2L * pad + 1L), function(a) {
      fixi <- -(pad + 1L - a) + i
      if (a < pad + 1L) {
        if (fixi %in% 1L:length(x)) x[[fixi]]
      } else if (a == pad + 1L) {
        x[[i]]
      } else if (a > pad + 1L) {
        if (fixi %in% 1L:length(x)) x[[fixi]]
      }
    })
    rtn <- do.call(func, z)           # do the call
    if (!is.null(rtn)) y[[i]] <- rtn  # dabbing around
    i <- i + 1L                       # increment
  }
  # exit
  return(y)
}

# xample
octo <- octostep(as.list(1L:5L), function(pre, cur, nxt) {
  c(pre=if (is.null(pre)) NA else pre, 
    cur=cur, 
    nxt=if (is.null(nxt)) NA else nxt)
})

# convenience helper
any.null <- function(...) {
  any(sapply(list(...), function(arg) is.null(arg)))
}
