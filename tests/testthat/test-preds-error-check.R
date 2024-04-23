
testData <- data.frame(
  y = rnorm(10),
  a = rnorm(10)
)

freqModel <- lm(y ~ a, data=testData)

test_that("make sure predsErrorCheckF is working correctly", {

  expect_error(predsErrorCheckF(freqModel, at=list(a=c(1,2,3)), centrality='mean'), regexp="The model must be a 'stanreg' object!")
  expect_error(predsErrorCheckF(logitModel, at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the at values don't match up with the names in the model data!")

})
