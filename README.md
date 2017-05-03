octostep
================

Iterate lists getting a *window* argument list to your callback.

------------------------------------------------------------------------

API
---

``` r
octostep(x, func,  # x and func are required
         pad=1L, use.names=TRUE, transform.previous=FALSE)
```

-   `x` List. **required**
-   `func` Function with arity `2L * pad + 1L`. **required**
-   `pad` Integer controlling the number of items to be padded *around* (on each side of) the current item, must be within `1L:((length(x) - 1L) / 2L)`. **optional**
-   `use.names` Copy names? **optional**
-   `transform.previous` Should the previous arguments of the callback take the values of previous callbacks rather than the plain values of the initial input list? **optional**

**Return**

List.

------------------------------------------------------------------------

Examples
--------

``` r
# see arguments evolve
octo <- octostep(as.list(1L:3L), function(pre, cur, nxt) {
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
paddle <- octostep(as.list(1L:5L), function(pre1, pre2, cur, nxt1, nxt2) {
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
# new input
cable <- list(a=1, b=2, z=3, d=4, u=5)

# iterate and map with default options
mule <- octostep(cable, function(pre, cur, nxt) {
  if (!any.null(pre, nxt)) sum(pre, cur, nxt) else cur
}, pad=1L, use.names=TRUE, transform.previous=FALSE)  # all defaults

print(mule)
```

    $a
    [1] 1

    $b
    [1] 6

    $z
    [1] 9

    $d
    [1] 12

    $u
    [1] 5

``` r
# transform previous items while iterating
mutant <- octostep(cable, function(pre, cur, nxt) {
  if (!any.null(pre, nxt)) sum(pre, cur, nxt) else cur
}, pad=1L, use.names=TRUE, transform.previous=TRUE)

print(mutant)
```

    $a
    [1] 1

    $b
    [1] 6

    $z
    [1] 13

    $d
    [1] 22

    $u
    [1] 5
