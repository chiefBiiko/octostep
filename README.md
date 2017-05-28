octostep
================

Iterate or reduce lists getting a *window* argument list to your callback.

*[octostep](#octostep)*

*[reduceList](#reducelist)*

octostep
--------

``` r
octostep::octostep(x, func,  # x and func are required
                   pad=1L, use.names=TRUE, transform.previous=FALSE)
```

-   `x` List **required**
-   `func` Function with arity `2L * pad + 1L` **required**
-   `pad` Integer controlling the number of items to be padded *around* (on each side of) the current item, must be within `1L:((length(x) - 1L) / 2L)` for the ordinary use case with `length(x) >= 2L * pad + 1L`, otherwise 1L **optional**
-   `use.names` Copy names? **optional**
-   `transform.previous` Should the previous arguments to `func` hold the values of previous callbacks rather than the plain values of the initial input list? **optional**

### Return

List

Examples
--------

``` r
# see arguments evolve
octo <- octostep::octostep(as.list(1L:3L), function(pre, cur, nxt) {
  c(pre=if (is.null(pre)) NA else pre, 
    cur=cur, 
    nxt=if (is.null(nxt)) NA else nxt)
})

print(octo)
```

    [[1]]
    pre cur nxt 
     NA   1   2 

    [[2]]
    pre cur nxt 
      1   2   3 

    [[3]]
    pre cur nxt 
      2   3  NA 

``` r
# increased padding
paddle <- octostep::octostep(as.list(1L:5L), 
                             function(pre1, pre2, cur, nxt1, nxt2) {
                               c(pre1=if (is.null(pre1)) NA else pre1, 
                                 pre2=if (is.null(pre2)) NA else pre2, 
                                 cur=cur, 
                                 nxt1=if (is.null(nxt1)) NA else nxt1,
                                 nxt2=if (is.null(nxt2)) NA else nxt2)
                             }, pad=2L)

print(paddle)
```

    [[1]]
    pre1 pre2  cur nxt1 nxt2 
      NA   NA    1    2    3 

    [[2]]
    pre1 pre2  cur nxt1 nxt2 
      NA    1    2    3    4 

    [[3]]
    pre1 pre2  cur nxt1 nxt2 
       1    2    3    4    5 

    [[4]]
    pre1 pre2  cur nxt1 nxt2 
       2    3    4    5   NA 

    [[5]]
    pre1 pre2  cur nxt1 nxt2 
       3    4    5   NA   NA 

``` r
# cool input
fable <- list(x=4L, y=1L, z=9L, a=0L, b=0L)

# transform previous items while iterating
transformer <- octostep::octostep(fable, function(pre, cur, nxt) {
  if (!octostep::any.null(pre, nxt)) sum(pre, cur, nxt) else cur
}, pad=1L, use.names=TRUE, transform.previous=TRUE)  # transformers

print(transformer)
```

    $x
    [1] 4

    $y
    [1] 14

    $z
    [1] 23

    $a
    [1] 23

    $b
    [1] 0

------------------------------------------------------------------------

reduceList
----------

``` r
octostep::reduceList(x, func,  # x and func are required
                     which.names=NULL, 
                     from=c('left', 'right')[1],
                     allow.ragged=FALSE, 
                     warn=if (allow.ragged) TRUE else FALSE)
```

-   `x` List of lists **required**
-   `func` Function with arity `length(x)` **required**
-   `which.names` Integer index of the list in `x` from which to copy names, default `NULL` indicates not to copy any names **optional**
-   `from` Whether to reduce from left or right, defaults to `left`, alternative `right` **optional**
-   `allow.ragged` Whether to allow input lists of unequal length, defaults to `FALSE` **optional**
-   `warn` Logical indicating whether to signal a warning if the name vector specified by `which.names` does not have the same length as the longest list of `x` **optional**

### Return

List

Examples
--------

``` r
# new input
listoflists <- list(list('A', 'B', 'C'), list(1L, 2L, 3L))

# allows reducing from left ...
octostep::reduceList(listoflists, function(a, b) {
  paste0(a, as.character(b))
}, from='left')
```

    [[1]]
    [1] "A1"

    [[2]]
    [1] "B2"

    [[3]]
    [1] "C3"

``` r
# or from right ...
octostep::reduceList(listoflists, function(a, b) {
  paste0(a, as.character(b))
}, from='right')
```

    [[1]]
    [1] "C3"

    [[2]]
    [1] "B2"

    [[3]]
    [1] "A1"

``` r
# and even lists of unequal length
octostep::reduceList(list(list(), list(1L)), function(a, b) {
  if (is.null(a)) b else a
}, allow.ragged=TRUE)
```

    [[1]]
    [1] 1
