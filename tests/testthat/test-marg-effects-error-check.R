

test_that("make sure margErrorCheckF is catching errors", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  testData <- data.frame(
    y = rnorm(10),
    a = rnorm(10)
  )
  
  freqModel <- lm(y ~ a, data=testData)

  expect_error(margErrorCheckF(logitModel, marginal_effect='educ', at=NULL, start_value=c(0, 5), end_value=c(10)), regexp="The arguments for 'marginal_effect,' 'start_value,' and 'end_value' must all have the same length!")
  expect_error(margErrorCheckF(freqModel, marginal_effect='a', at=NULL, start_value=1, end_value=2), regexp="The model must be a 'stanreg' object!")
  expect_error(margErrorCheckF(logitModel, marginal_effect='f', at=NULL, start_value=c(0), end_value=c(5)), regexp="The names of the marginal effects don't match up with the names in the model data!")
  expect_error(margErrorCheckF(logitModel, marginal_effect='educ', at=list(d=c(1,2)), start_value=c(0), end_value=c(5)), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(margErrorCheckF(logitModel, marginal_effect=c('educ', 'educ'), at=NULL, start_value=c(0, 5), end_value=c(5, 10)), regexp="When specifying multiple start values, they must be contained in a list to preserve data type!")

})
