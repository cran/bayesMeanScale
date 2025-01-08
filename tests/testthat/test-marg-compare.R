

test_that("test that bayesMargCompareF runs without error and without warning", {
  
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
  
  m1AMEInteraction <- bayesMargEffF(logitModel,
                                    marginal_effect = 'dist',
                                    start_value     = 64.041,
                                    end_value       = 21.117,
                                    at              = list(educ=c(0, 5, 8)),
                                    n_draws         = 500)
  
  crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)
  
  poissonModel  <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width + color + spine, data=crabs, family=poisson, refresh=0, chains=2, iter=500))
  
  poissonMarg <- bayesCountMargEffF(poissonModel, 
                                    counts          = c(0,1), 
                                    marginal_effect = c('width', 'weight'), 
                                    start_value     = list(20, 2), 
                                    end_value       = list(25, 3), 
                                    at              = list(color=c(1,2,3,4), spine=c(1,2,3)), 
                                    n_draws         = 500)
  
  expect_no_error(bayesMargCompareF(m1AMEInteraction))
  expect_no_error(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
  expect_no_warning(bayesMargCompareF(m1AMEInteraction))
  expect_no_warning(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
  expect_no_error(bayesMargCompareF(poissonMarg))
  expect_no_error(bayesMargCompareF(poissonMarg, centrality='median'))
  
  expect_no_warning(bayesMargCompareF(poissonMarg))
  expect_no_warning(bayesMargCompareF(poissonMarg, centrality='median'))
  
})