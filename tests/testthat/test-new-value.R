

test_that('make sure that factor levels line up with original data', {
   expect_identical(levels(newValueF(logitModel$model, marg_effect='assoc', new_value='Y')$assoc), sort(unique(logitModel$model$assoc)))
})
