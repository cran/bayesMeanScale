
test_that("make sure countPredsErrorCheckF is working correctly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  set.seed(500)
  
  modelData       <- rstanarm::wells
  modelData$assoc <- ifelse(modelData$assoc==1, 'Y', 'N')
  
  rowMiss <- sample(1:nrow(modelData), size=10, replace=F)
  colMiss <- sample(1:ncol(modelData), size=10, replace=T)
  
  for(i in 1:10){
    
    modelData[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  logitModel  <- suppressWarnings(rstanarm::stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  
  crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)
  
  poissonModel  <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=poisson, refresh=0, chains=2, iter=500))
  
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(0,1), at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(-1,0), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be non-negative!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c("a"), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countPredsErrorCheckF(poissonModel, counts=c(1, 1.2), at=list(weight=c(1,2,3)), centrality='mean'), regexp="Counts values must be integers!")
  expect_error(countPredsErrorCheckF(logitModel, counts=c(0,1), at=list(f=c(1,2,3)), centrality='mean'), regexp="The model must be poisson or negative binomial!")
  
})
