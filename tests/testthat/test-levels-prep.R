

test_that("make sure levelsPrepF is working correctly", {
  
  skip_on_cran()
  
  newData      <- data.frame(a = c(1,2,3))
  originalData <- data.frame(a = c('a', 'b', 'c'))

  expect_error(levelsPrepF(newData, newData, originalData), regexp="You supplied numeric values for a character/factor variable!")

})
