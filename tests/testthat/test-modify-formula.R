

testData <- data.frame(
  a = rnorm(10),
  b = rnorm(10, mean=100),
  c = rnorm(10),
  d = rnorm(10),
  y = rnorm(10)
)

m1           <- lm(y ~ a - offset(log(b)) + offset(exp(c)), data=testData, offset=d/4)
rightFormula <- formula(y ~ a)
testFormula  <- modifyFormulaF(m1)

m1Right <- lm(rightFormula, data=testData)
m1Test  <- lm(testFormula, data=testData)

test_that("make sure modifyFormula is handling offsets correctly", {
  expect_equal(names(m1Test$coefficients), names(m1Right$coefficients))
})
