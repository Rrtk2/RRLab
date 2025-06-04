library(testthat)

test_that("RRFE runs on iris subset", {
  skip_on_cran()
  data <- iris[1:20, ]
  result <- RRFE(dataset = data, f_dataset_class_column_id = 5,
                 s_MaxComponents = 2, s_KRepeats = 1,
                 s_KmeansRepeat = 1, ShowPlots = FALSE, verbose = FALSE)
  expect_type(result, "list")
