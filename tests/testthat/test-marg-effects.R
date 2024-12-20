
test_that("make sure bayesMargEffF is working properly", {
  
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
  logitModel2 <- suppressWarnings(rstanarm::stan_glm(switch ~ log(dist) + educ + arsenic + I(arsenic^2) + as.factor(assoc), data=modelData, family=binomial, refresh=0, chains=2, iter=500))
  
  expect_no_error(bayesMargEffF(logitModel2, marginal_effect='log(dist)', start_value=4, end_value=3, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value="instantaneous", end_value="instantaneous", at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value="instantaneous", end_value=0, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  
  
  expect_no_warning(bayesMargEffF(logitModel2, marginal_effect='log(dist)', start_value=4, end_value=3, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value="instantaneous", end_value="instantaneous", at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value="instantaneous", end_value=0, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  
  
  ame <- bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), digits=6)$diffTable %>%
    subset(., select=c(mean))
  
  mem <- bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), digits=6, at_means=T)$diffTable %>%
    subset(., select=c(mean))
  
  diffs <- abs(ame-mem)
  
  expect_gt(diffs[1,], 0)
  expect_gt(diffs[2,], 0)
  expect_lt(diffs[1,], abs(ame[1,]*1.1 - ame[1,]))
  expect_lt(diffs[2,], abs(ame[2,]*1.1 - ame[2,]))
  
})
