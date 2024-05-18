

test_that("make sure all configurations of bayesMargEffF run without error", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('tibble')
  
  expect_no_error(bayesMargEffF(logitModel2, marginal_effect='log(dist)', start_value=4, end_value=3, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_error(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  
})

test_that("make sure all configurations of bayesMargEffF run without warning", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('tibble')
  
  expect_no_warning(bayesMargEffF(logitModel2, marginal_effect='log(dist)', start_value=4, end_value=3, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, n_draws=500))
  expect_no_warning(bayesMargEffF(logitModel, marginal_effect='dist', start_value=50, end_value=20, at=list(educ=c(0, 12)), at_means=T, hdi_interval=F, n_draws=500))
  
})

test_that('make sure ame and mem are close but not exactly the same', {
      
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  skip_if_not_installed('tibble')
  
  set.seed(500)
  
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

