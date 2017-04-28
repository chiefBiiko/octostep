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
#' callback take the values of the callback rather than the plain
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
all.null <- function(...) {
  return(all(sapply(list(...), function(arg) is.null(arg))))
}

##############################################################################

# examples

# input
cable <- list(a=1, b=2, z=3, d=4, u=5)

# see arguments evolve
octo <- octostep(as.list(1L:5L), function(pre, cur, nxt) {
  c(pre=if (is.null(pre)) NA else pre, 
    cur=cur, 
    nxt=if (is.null(nxt)) NA else nxt)
})

# increase padding
paddle <- octostep(as.list(1L:9L), function(pre1, pre2, cur, nxt1, nxt2) {
  c(pre1=if (is.null(pre1)) NA else pre1, 
    pre2=if (is.null(pre2)) NA else pre2, 
    cur=cur, 
    nxt1=if (is.null(nxt1)) NA else nxt1,
    nxt2=if (is.null(nxt2)) NA else nxt2)
}, pad=2L)

# iterate with default options
mule <- octostep(cable, function(pre, cur, nxt) {
  if (!any.null(pre, nxt)) sum(pre, cur, nxt) else cur
}, pad=1L, use.names=TRUE, transform.previous=FALSE)  # all defaults

# transform previous items while iterating
mutant <- octostep(cable, function(pre, cur, nxt) {
  if (!any.null(pre, nxt)) sum(pre, cur, nxt) else cur
}, pad=1L, use.names=TRUE, transform.previous=TRUE)

