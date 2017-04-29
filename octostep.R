# octostep

#' Iterate lists getting a \emph{window} argument list to your callback.
#' 
#' Besides a list \code{octostep} takes a function as input, 
#' applies it to each element of the list and returns the resulting list.
#' 
#' @param x List \strong{required}
#' @param func Function with \code{length(formals(func))} == 
#' \code{2L * pad + 1L} \strong{required}.
#' @param pad Integer controlling how many items should be padded \emph{around} 
#' the current item, must be within \code{1L:((length(x) - 1L) / 2L)} 
#' \strong{optional}.
#' @param use.names If \code{x} has a \code{names} attribute, should it be 
#' used for the return value? \strong{optional}.
#' @param transform.previous Should the \code{previous} arguments of the 
#' callback take the values of previous callbacks rather than the plain
#' values of the initial input list \strong{optional}.
#' @return List.
#'
#' @export
octostep <- function(x, func, 
                     pad=1L, use.names=TRUE, transform.previous=FALSE) {
  stopifnot(pad >= 1L, length(x) >= 2L * pad + 1L, 
            is.function(func), length(formals(func)) == 2L * pad + 1L)
  y <- vector('list', length(x))  # preallocate return
  if (use.names) names(y) <- names(x)
  # iterate
  i <- 1L
  repeat {
    # build argument list for current callback
    z <- lapply(1L:(2L * pad + 1L), function(a) {
      fixi <- -(pad + 1L - a) + i
      if (a < pad + 1L) {
        if (!transform.previous && fixi %in% 1L:length(x)) {
          x[[fixi]]
        } else if (fixi %in% 1L:length(x)) {
          y[[fixi]]
        }
      } else if (a == pad + 1L) {
        x[[i]]
      } else if (a > pad + 1L) {
        if (fixi %in% 1L:length(x)) x[[fixi]]
      }
    })
    rtn <- do.call(func, z)           # do the call
    if (!is.null(rtn)) y[[i]] <- rtn  # dabbing around
    i <- i + 1L                       # increment
    if (i > length(x)) break          # trapdoor
  }
  # exit
  return(y)
}

#' Check multiple arguments for \code{NULL}
#' 
#' @param ... Any R object.
#' @return Logical.
#' 
#' @export
any.null <- function(...) {
  return(any(sapply(list(...), function(arg) is.null(arg))))
}

#' @rdname any.null
#' @export
all.null <- function(...) {
  return(all(sapply(list(...), function(arg) is.null(arg))))
}
