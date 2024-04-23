

test_that("make sure countPredsErrorCheckF is working correctly", {
  
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(0,1), at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(-1,0), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be non-negative!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c("a"), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(1, 1.2), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countPredsErrorCheckF(logitModel, counts=c(0,1), at=list(f=c(1,2,3)), centrality='mean'), regexp="The model must be poisson or negative binomial!")
  
})
