library(testthat)

test_that("libraryR loads packages", {
  res <- libraryR("stats")
  expect_true(isTRUE(res["stats"]))
})

# ensure unquoted form works

test_that("libraryR handles unquoted", {
  res <- libraryR(stats)
  expect_true(isTRUE(res["stats"]))
})
