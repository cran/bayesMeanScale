

test_that("test that bayesMargCompareF runs without error and without warning", {
  
  skip_on_cran()
  skip_if_not_installed('rstanarm')
  
  m1AMEInteraction <- bayesMargEffF(logitModel,
                                    marginal_effect = 'dist',
                                    start_value     = 64.041,
                                    end_value       = 21.117,
                                    at              = list(educ=c(0, 5, 8)),
                                    n_draws         = 500)
  
  expect_no_error(bayesMargCompareF(m1AMEInteraction))
  expect_no_error(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
  expect_no_warning(bayesMargCompareF(m1AMEInteraction))
  expect_no_warning(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
})