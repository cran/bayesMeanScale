

test_that("make sure all configurations of bayesCountPredsF run without error", {

  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_error(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

})

test_that("make sure all configurations of bayesCountPredsF run without warning", {

  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountPredsF(poissonModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), at_means=T, n_draws=500))
  expect_no_warning(bayesCountPredsF(negBinomModel, counts=c(0,1), at=list(weight=c(2,3)), hdi_interval=F, at_means=T, n_draws=500))

})

set.seed(500)

poissonPreds <- bayesCountPredsF(poissonModel2, counts=c(0,1), at=list(w=c(2,3)))$predTable %>%
  subset(., select=c(mean))

nbPreds <- bayesCountPredsF(negBinomModel2, counts=c(0,1), at=list(w=c(2,3)))$predTable %>%
  subset(., select=c(mean))

diffs <- abs(poissonPreds - nbPreds)

test_that('make sure poisson and negative binomial preds are somewhat close', {
      
      expect_gt(diffs[1,], 0)
      expect_gt(diffs[2,], 0)
      expect_gt(diffs[3,], 0)
      expect_gt(diffs[4,], 0)
      
      expect_lt(diffs[1,], poissonPreds[1,]*1.15 - poissonPreds[1,])
      expect_lt(diffs[2,], poissonPreds[2,]*1.15 - poissonPreds[2,])
      expect_lt(diffs[3,], poissonPreds[3,]*1.15 - poissonPreds[3,])
      expect_lt(diffs[4,], poissonPreds[4,]*1.15 - poissonPreds[4,])
  
})

