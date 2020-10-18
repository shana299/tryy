context("ridgereg")
library(mlbench)
library(MASS)
data("BostonHousing")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(mdev~indys+afe, data=BostonHousing, lambda = 1))
  expect_error(ridgereg(Pmdev~indys+afe, data=BostonHousingss, lambda = 1))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("output compares with lm.ridge", {
  ridgereg_mod <- ridgereg(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  lm.ridge_mod <- lm.ridge(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  
  expect_equal(ridgereg_mod$coefficients[-1], lm.ridge_mod$coef, tolerance = .01)
})

test_that("print() works", {
  ridgereg_mod <- ridgereg(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  
  expect_output(print(ridgereg_mod),
    "ridgereg\\(formula = medv ~ indus \\+ crim \\+ lstat, data = BostonHousing, \n( )*lambda = 1\\)")
  expect_output(print(ridgereg_mod),"( )*\\(Intercept\\)( )*indus( )*crim( )*lstat")
})

test_that("predict() works", {
  ridgereg_mod <- ridgereg(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  
  expect_equal(predict(ridgereg_mod, BostonHousing[1:5, ]), ridgereg_mod$fitted_values[1:5])    
})

test_that("coef() works", {
  ridgereg_mod <- ridgereg(medv~indus+crim+lstat, data=BostonHousing, lambda = 1)
  
  expect_true(all(round(unname(coef(ridgereg_mod)),2) %in% c(22.49, -0.45, -0.54, -6.25)))
})
