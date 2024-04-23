

test_that("make sure countMargErrorCheckF is working correctly", {
  
  expect_error(countMargErrorCheckF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=25, end_value=20, at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(countMargErrorCheckF(poissonModel, counts=c(-1,0), marginal_effect='width', start_value=25, end_value=20, at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be non-negative!")
  expect_error(countMargErrorCheckF(poissonModel, counts=c("a"), marginal_effect='width', start_value=25, end_value=20, at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countMargErrorCheckF(poissonModel, counts=c(1, 1.2), marginal_effect='width', start_value=25, end_value=20, at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countMargErrorCheckF(logitModel, counts=c(0,1), marginal_effect='width', start_value=25, end_value=20, at=list(f=c(1,2,3)), centrality='mean'), regexp="The model must be poisson or negative binomial!")
  
})