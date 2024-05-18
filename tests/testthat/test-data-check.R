

test_that("make sure dataCheckF detects errors", {
  
  newData        <- data.frame(x = factor(c('A', 'B')))
  modelFrameData <- data.frame(x = factor(c('A', 'A')))
  
  newData1        <- data.frame(x = factor(c('A', 'B')))
  modelFrameData1 <- data.frame(x = c('A', 'A'))
  
  newData2        <- data.frame(x = c('A', 'B'))
  modelFrameData2 <- data.frame(x = factor(c('A', 'A')))
  
  newData3        <- data.frame(x = c('A', 'B'))
  modelFrameData3 <- data.frame(x = c('A', 'A'))
  
  newData4        <- data.frame(x = c('a', 'b'))
  modelFrameData4 <- data.frame(x = c(1,2))
  
  expect_error(dataCheckF(newData, modelFrameData), regexp="There's a factor level mismatch in or or more of the model frame columns!")
  expect_error(dataCheckF(newData1, modelFrameData1), regexp="You specified the values wrong for some character/factor variable!")
  expect_error(dataCheckF(newData2, modelFrameData2), regexp="You specified the values wrong for some character/factor variable!")
  expect_error(dataCheckF(newData3, modelFrameData3), regexp="You specified the values wrong for some character/factor variable!")
  expect_error(dataCheckF(newData4, modelFrameData4), regexp='You supplied character values for some numeric variable!')
})

