

test_that("make sure all configurations of bayesCountMargEffF run without error", {
  
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
  
})

test_that("make sure all configurations of bayesCountMargEffF run without warning", {
  
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
