context("check arugment")

test_that("check_prob with invalid prob", {
  
  
  expect_error(check_prob(1.1))
  expect_error(check_prob(-0.5))

})

test_that("check_prob with valid prob", {
  
  x <- seq(0,1,0.01)
  
  expect_true(check_prob(x))
  
})

test_that("check_prob has a length of 1", {
  
  expect_length(check_prob(1), 1)
})


test_that("check_trials are invalid", {
  expect_error(check_trials(-5))
})


test_that("check_trails are valid", {
  expect_true(check_trials(5L))
  expect_true(check_trials(9))
})


test_that("check_trials has a length of 1", {
  expect_length(check_trials(8), 1)
})


test_that("check_success has success between trials", {
  expect_error(check_success(8,7))
})




