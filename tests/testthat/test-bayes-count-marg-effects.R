

test_that("make sure bayesCountMargEffF is working properly", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  set.seed(500)
  
  crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=T)
  
  poissonModel  <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=poisson, refresh=0, chains=2, iter=500))
  negBinomModel <- suppressWarnings(rstanarm::stan_glm(sat ~ weight + width, data=crabs, family=rstanarm::neg_binomial_2, refresh=0, chains=2, iter=500))
  
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(poissonModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountMargEffF(negBinomModel, counts=c(0,1), marginal_effect='width', start_value=20, end_value=25, at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))
  
})
