# test reduceList

testthat::context('mergeLists')

testthat::test_that('mergeLists allows alot', {
  
  # handles empty inputs
  testthat::expect_identical(mergeLists(list(list(), list()), 
                                        function(a, b) 1L), 
                             list())
  
  # allows inputs of unequal lengths
  testthat::expect_identical(mergeLists(list(list(), list(1L)), 
                                        function(a, b) 11L, allow.ragged=TRUE),
                             list(11L))
  
  # new input
  listoflists <- list(list('A', 'B', 'C'), list(1L, 2L, 3L))
  
  # allows reducing from left ...
  testthat::expect_identical(mergeLists(listoflists, function(a, b) {
    paste0(a, as.character(b))
  }, from='left'),  # default
  list('A1', 'B2', 'C3'))
  
  # ... or from right
  testthat::expect_identical(mergeLists(listoflists, function(a, b) {
    paste0(a, as.character(b))
  }, from='right'), 
  list('C3', 'B2', 'A1'))
  
  # not recursive: slow above 10000
  testthat::expect_identical(mergeLists(list(as.list(rep(0L, 5e4L)), as.list(rep(1L, 5e4L))), 
                                        function(a, b) a + b),
                             as.list(rep(1L, 5e4L)))
  
})