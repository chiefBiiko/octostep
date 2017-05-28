# reduceList

#' Reduces multiple lists to a single list.
#' 
#' @param x List of lists \strong{required}.
#' @param func Function with arity \code{length(x)} \strong{required}.
#' @param which.names Integer index of the list in \code{x} from which to copy 
#' names, default \code{NULL} indicates not to copy any names \strong{optional}.
#' @param from Whether to reduce from left or right, defaults to \code{'left'},
#' alternative \code{'right'} \strong{optional}.
#' @param allow.ragged Whether to allow input lists of unequal length, 
#' defaults to \code{FALSE} \strong{optional}.
#' @param warn Logical indicating whether to signal a warning if the name 
#' vector specified by \code{which.names} does not have the same length as 
#' the longest list of \code{x} \strong{optional}.
#' @return List.
#' 
#' @details If the input lists are of unequal length an error is thrown unless
#' \code{allow.ragged} is set to \code{TRUE}. The warn parameter is only of 
#' relevance when \code{x} is ragged (input lists are of unequal length). In 
#' such a case just make sure the name vector referenced via \code{which.names}
#' has the same length as the longest list of \code{x}. Each iteration 
#' \code{func} is called with the current items of \eqn{x[1...n]} as arguments. 
#' If \code{x} is ragged missing values for current items are represented by 
#' \code{NULL}.
#' 
#' @seealso \code{\link{octostep}}
#' 
#' @export
reduceList <- function(x, func, 
                       which.names=NULL, 
                       from=c('left', 'right')[1],
                       allow.ragged=FALSE, 
                       warn=if (allow.ragged) TRUE else FALSE) {
  stopifnot(is.list(x),  # too strict: length(x) > 1L, 
            all(sapply(x, function(xx) is.list(xx))), 
            is.function(func), length(formals(func)) == length(x),
            is.null(which.names) || which.names %in% 1L:length(x), 
            from %in% c('left', 'right'), 
            is.logical(allow.ragged), 
            is.logical(warn))
  if (!allow.ragged && 
      !all(sapply(x, function(xx) length(xx) == length(x[[1]])))) {
    stop('Set allow.ragged to TRUE to iterate lists of unequal length')
  }
  # prep
  y <- vector('list', maxl <- max(sapply(x, length)))  # preallocate return
  if (length(y) == 0L) return(y)                       # early exit
  if (is.numeric(which.names)) {                       # names
    names(y) <- names(x[[which.names]]) 
    if (warn && anyNA(names(y))) warning('Return value has missing names')
  }
  # setup iteration
  if (from == 'left') {
    i <- 1L
    crem <- +1L
  } else {
    i <- maxl
    crem <- -1L
  }
  # iterate
  repeat {
    # build argument list for current iteration/callback
    z <- lapply(x, function(xx) {          # catch ragged indexing
      tryCatch(if (i %in% 1L:length(xx)) xx[[i]], error=function(e) NULL)
    })
    rtn <- do.call(func, z)                # do the call
    if (!is.null(rtn)) {                   # dabbing around
      if (crem == 1L) {                    # from left
        y[[i]] <- rtn
      } else {                             # from right
        y[[length(y) + 1L - i]] <- rtn
      }
    }
    i <- i + crem                          # crement
    if (i > maxl || i < 1L) break          # trapdoor
  }
  # exit
  return(y)
}