test_that("median2 returns median for numeric vector", {
  x <- c(1, 3, 2, 5, 4)
  expect_equal(median2(x), 3)
})

test_that("median2 returns correct level for factor vector", {
  x <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
  expect_equal(median2(x), "medium")
})
