test_that("RRnorm scales values between 0 and 1 by default", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(RRnorm(x), (x - min(x)) / (max(x) - min(x)))
})

test_that("RRnorm applies Scalar and Offset correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(RRnorm(x, Scalar = 2, Offset = -1),
               ((x - min(x)) / (max(x) - min(x))) * 2 - 1)
})
