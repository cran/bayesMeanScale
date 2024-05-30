

test_that("make sure predsErrorCheckF is working correctly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  set.seed(500)
  
  testData <- data.frame(
    y = rnorm(10),
    a = rnorm(10)
  )
  
  freqModel <- lm(y ~ a, data=testData)
  
  modelData       <- rstanarm::wells
  modelData$assoc <- ifelse(modelData$assoc==1, 'Y', 'N')
  
  rowMiss <- sample(1:nrow(modelData), size=10, replace=F)
  colMiss <- sample(1:ncol(modelData), size=10, replace=T)
  
  for(i in 1:10){
    
    modelData[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  logitModel  <- suppressWarnings(rstanarm::stan_glm(switch ~ dist*educ + arsenic + I(arsenic^2) + assoc, data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  logitModel2 <- suppressWarnings(rstanarm::stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  
  expect_error(predsErrorCheckF(freqModel, at=list(a=c(1,2,3)), centrality='mean'), regexp="The model must be a 'stanreg' object!")
  expect_error(predsErrorCheckF(logitModel, at=list(f=c(1,2,3)), centrality='mean'), regexp="The names for the at values don't match up with the names in the model data!")

})
