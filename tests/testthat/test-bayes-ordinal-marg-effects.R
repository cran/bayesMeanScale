
set.seed(500)

test_that("make sure bayesOrdinalMargEffF is working properly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('MASS')
  
  housingData <- MASS::housing
  
  rowMiss <- sample(1:nrow(housingData), size=10, replace=F)
  colMiss <- sample(1:ncol(housingData), size=10, replace=T)
  
  for(i in 1:10){
    
    housingData[rowMiss[[i]], colMiss[[i]]] <- NA
    
  }
  
  ordinalModel        <- suppressWarnings(rstanarm::stan_polr(Sat ~ Infl + Type, data=housingData, prior=rstanarm::R2(0.2, 'mean'), refresh=0, iter=200))
  ordinalModelCloglog <- suppressWarnings(rstanarm::stan_polr(Sat ~ Infl + Type, data=housingData, prior=rstanarm::R2(0.2, 'mean'), refresh=0, method='cloglog', iter=200))
  
  expect_no_error(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), n_draws=500))
  expect_no_error(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), hdi_interval=F, n_draws=500))
  expect_no_error(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), at_means=T, n_draws=500))
  expect_no_error(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), hdi_interval=F, at_means=T, n_draws=500))

  expect_no_warning(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), n_draws=500))
  expect_no_warning(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), at_means=T, n_draws=500))
  expect_no_warning(bayesOrdinalMargEffF(ordinalModel, marginal_effect="Infl", start_value="Low", end_value="High",  at=list(Type=c("Tower", "Apartment")), hdi_interval=F, at_means=T, n_draws=500))

})

