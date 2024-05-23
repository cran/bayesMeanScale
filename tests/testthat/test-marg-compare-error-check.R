

test_that("make sure margCompareErrorCheckF is catching errors", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  margTestError   <- bayesMargEffF(logitModel, marginal_effect='educ', start_value=5, end_value=0, digits=4)
  margTestNoError <- bayesMargEffF(logitModel, marginal_effect='educ', start_value=5, end_value=0, digits=4, at=list(dist=c(20, 30)))
  
  expect_error(margCompareErrorCheckF(marg_list=1, ci=.95, hdi_interval=T, centrality='mean'), regexp="The 'marg_list' argument must have class 'bayes_mean_scale_marg'!")
  expect_error(margCompareErrorCheckF(margTestNoError, ci=1.1, hdi_interval=T, centrality='mean'), regexp="The credible interval level must be between 0 and 1!")
  expect_error(margCompareErrorCheckF(margTestNoError, ci=.95, hdi_interval="W", centrality='mean'), regexp="This is a logical argument!")
  expect_no_error(margCompareErrorCheckF(margTestNoError, ci=.95, hdi_interval=T, centrality='mean'))
  expect_error(margCompareErrorCheckF(margTestError, ci=.95, hdi_interval=T, centrality='mean'), regexp="There is only 1 marginal effect, so nothing to compare to!")
  
})
