
set.seed(500)

test_that("make sure predsOrdinalErrorCheckF is working correctly", {

  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  ordinalModel <- suppressWarnings(rstanarm::stan_polr(Sat ~ Infl + Type, data=MASS::housing, prior=rstanarm::R2(0.2, 'mean'), refresh=0, iter=200))
  
  testData <- data.frame(
    y = rnorm(10),
    a = rnorm(10)
  )
  
  freqModel <- lm(y ~ a, data=testData)
  
  expect_error(predsOrdinalErrorCheckF(freqModel, at=list(a=c(1,2,3)), centrality='mean'), regexp="Only models fit using 'stan_polr' are supported!")
  expect_error(predsOrdinalErrorCheckF(ordinalModel, at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the 'at' values don't match up with the names in the model data!")
  expect_error(predsOrdinalErrorCheckF(ordinalModel, centrality='mean'), regexp="You must supply at least 1 value for the 'at' argument!")
  expect_error(predsOrdinalErrorCheckF(ordinalModel, at=list(Type=c("Tower")), centrality='sd'), regexp="Centrality options are 'mean' or 'median'!")
})
