# reduceList

#' Reduces multiple lists to a single list.
#' 
#' @param x List of lists \strong{required}.
#' @param func Function with arity \code{length(x)} \strong{required}.
#' @param which.names Integer index of the list in \code{x} from which to copy 
#' names, default \code{NULL} indicates not to copy names \strong{optional}.
#' @param from Whether to reduce from left or right, defaults to \code{'left'} 
#' \strong{optional}.
#' @param allow.ragged Whether to allow input lists of unequal length, 
#' defaults to \code{FALSE} \strong{optional}.
#' @param warn Logical indicating whether to signal a warning if the name 
#' vector specified by \code{which.names} does not have the same length as 
#' the longest list of \code{x} \strong{optional}.
#' @return List.
#' 
#' @seealso \code{\link{octostep}}
#' 
#' @export
reduceList <- function(x, func, 
                       which.names=NULL, 
                       from=c('left', 'right')[1],
                       allow.ragged=FALSE, warn=TRUE) {
  stopifnot(is.list(x), length(x) > 1L, 
            all(sapply(x, function(xx) length(xx) > 0L)), 
            is.function(func), length(formals(func)) == length(x),
            is.null(which.names) || which.names %in% 1L:length(x), 
            from %in% c('left', 'right'), 
            is.logical(allow.ragged))
  if (!allow.ragged && 
      !all(sapply(x, function(xx) length(xx) == length(x[[1]])))) {
    stop('Set allow.ragged to TRUE to iterate lists of unequal length')
  }
  # preallocate return
  y <- vector('list', maxl <- length(x[[which.max(sapply(x, length))]]))
  # naming
  if (is.numeric(which.names)) {
    names(y) <- names(x[[which.names]]) 
    if (warn && anyNA(names(y))) warning('Return value has missing names')
  }
  # iterate
  if (from == 'left') {
    i <- 1L
    crem <- +1L
  } else {
    i <- maxl
    crem <- -1L
  }
  repeat {
    # build argument list for current iteration/callback
    z <- lapply(x, function(xx) if (i %in% 1L:length(xx)) xx[[i]])
    rtn <- do.call(func, z)                # do the call
    if (!is.null(rtn)) y[[i]] <- rtn       # dabbing around
    i <- i + crem                          # crement
    if (i > maxl || i < 1L) break          # trapdoor
  }
  # exit
  return(y)
}