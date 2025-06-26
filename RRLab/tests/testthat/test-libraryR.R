skip_if_not_installed("cli")
skip_if_not_installed("rlang")

test_that("libraryR loads quoted package names", {
  res <- libraryR("stats")
  expect_named(res, "stats")
  expect_true(res[["stats"]])
})

test_that("libraryR loads unquoted package names", {
  res <- libraryR(stats)
  expect_named(res, "stats")
  expect_true(res[["stats"]])
})

test_that("libraryR can load multiple packages", {
  pkgs <- c("stats", "utils")
  res <- libraryR(pkgs)
  expect_equal(names(res), pkgs)
  expect_true(all(res))
})

test_that("libraryR returns FALSE for missing packages", {
  pkg <- "nonexistentPkg12345"
  res <- libraryR(pkg)
  expect_named(res, pkg)
  expect_false(res[[pkg]])
})

test_that("libraryR notes already loaded packages", {
  library(stats)
  expect_message(libraryR(stats), "preloaded")
})
