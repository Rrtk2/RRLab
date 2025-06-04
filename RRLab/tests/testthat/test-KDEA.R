library(testthat)

skip_if_not_installed("data.table")

test_that("KDEA runs on iris subset", {
  skip_on_cran()
  data <- iris[1:30, ]
  result <- KDEA(dataset = data, f_dataset_class_column_id = 5,
                 s_k = 2, s_MakePlot = FALSE, s_Verbose = FALSE)
  expect_type(result, "list")
})
