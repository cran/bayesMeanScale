

test_that("make sure margErrorCheckF is catching errors", {
  
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

  expect_error(margErrorCheckF(logitModel, marginal_effect='educ', at=NULL, start_value=c(0, 5), end_value=c(10)), regexp="The arguments for 'marginal_effect,' 'start_value,' and 'end_value' must all have the same length!")
  expect_error(margErrorCheckF(freqModel, marginal_effect='a', at=NULL, start_value=1, end_value=2), regexp="The model must be a 'stanreg' object!")
  expect_error(margErrorCheckF(logitModel, marginal_effect='f', at=NULL, start_value=c(0), end_value=c(5)), regexp="The names of the marginal effects don't match up with the names in the model data!")
  expect_error(margErrorCheckF(logitModel, marginal_effect='educ', at=list(d=c(1,2)), start_value=c(0), end_value=c(5)), regexp="The names for the at values don't match up with the names in the model data!")
  expect_error(margErrorCheckF(logitModel, marginal_effect=c('educ', 'educ'), at=NULL, start_value=c(0, 5), end_value=c(5, 10)), regexp="When specifying multiple start values, they must be contained in a list to preserve data type!")

})
