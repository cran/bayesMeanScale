

test_that("make sure all configurations of bayesPredsF run without error", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('tibble')
  
  expect_no_error(bayesPredsF(logitModel2, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_error(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500))
  
})

test_that("make sure all configurations of bayesPredsF run without warning", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('tibble')
  
  expect_no_warning(bayesPredsF(logitModel2, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_warning(bayesPredsF(logitModel, at=list(educ=c(0, 12)), hdi_interval=F, at_means=T, n_draws=500))
  
})

