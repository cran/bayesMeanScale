
m1AMEInteraction <- bayesMargEffF(logitModel,
                                  marginal_effect = 'dist',
                                  start_value     = 64.041,
                                  end_value       = 21.117,
                                  at              = list(educ=c(0, 5, 8)),
                                  n_draws         = 500)

test_that("test that bayesMargCompareF runs without error", {
  
  expect_no_error(bayesMargCompareF(m1AMEInteraction))
  expect_no_error(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
})

test_that("test that bayesMargCompareF runs without warning", {
  
  expect_no_warning(bayesMargCompareF(m1AMEInteraction))
  expect_no_warning(bayesMargCompareF(m1AMEInteraction, centrality='median'))
  
})