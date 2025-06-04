context("RRFE")

set.seed(123)

results <- RRFE(dataset = iris,
                f_dataset_class_column_id = which(colnames(iris)=="Species"),
                s_MinimalVariance = 0.5,
                s_MaxComponents = 2,
                s_KRepeats = 2,
                s_KmeansRepeat = 2,
                verbose = FALSE,
                ShowPlots = FALSE)

# Expect a list with the BestFeatureNames element and at least one feature

test_that("RRFE returns feature names", {
  expect_true("BestFeatureNames" %in% names(results))
  expect_type(results$BestFeatureNames, "character")
  expect_gt(length(results$BestFeatureNames), 0)
})
