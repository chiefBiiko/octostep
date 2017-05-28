# test octostep

testthat::context('octostep')

testthat::test_that('callback is fed appropriately', {
  
  # empty list
  testthat::expect_identical(octostep(list(), 
                                      function(pre, cur, nxt) {}), 
                             list())
  
  # omitted return value in callback
  testthat::expect_identical(octostep(list(1L, 2L, 3L), 
                                      function(pre, cur, nxt) {}), 
                             list(NULL, NULL, NULL))
  
  # mini xmple
  testthat::expect_identical(octostep(list(1L, 2L, 3L),
                                      function(pre, cur, nxt) any.null(pre, cur, nxt)),
                             list(TRUE, FALSE, TRUE))
  
  # more xmple
  testthat::expect_identical(octostep(list(1L, 2L, 3L, 4L, 5L), 
                                      function(pre, cur, nxt) {
                                        if (any.null(pre, cur, nxt)) {
                                          cur
                                        } else {
                                          sum(pre, cur, nxt)
                                        }
                                      }, transform.previous=FALSE),  # default
                             list(1L, 6L, 9L, 12L, 5L))
  
  # transformers
  testthat::expect_identical(octostep(list(1L, 2L, 3L, 4L, 5L), 
                                      function(pre, cur, nxt) {
                                        if (any.null(pre, cur, nxt)) {
                                          cur
                                        } else {
                                          sum(pre, cur, nxt)
                                        }
                                      }, transform.previous=TRUE),
                             list(1L, 6L, 13L, 22L, 5L))
    
})