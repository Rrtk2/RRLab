if (requireNamespace('bigmemory', quietly = TRUE) &&
    requireNamespace('RcppParallel', quietly = TRUE)) {
  test_that('parallelOuterToBigMat matches outer product', {
    x <- rnorm(4)
    n <- length(x)
    bf <- tempfile(fileext = '.bin')
    df <- tempfile(fileext = '.desc')
    bm <- bigmemory::filebacked.big.matrix(nrow = n, ncol = n,
                                           type = 'double',
                                           backingfile = basename(bf),
                                           descriptorfile = basename(df),
                                           backingpath = tempdir())
    parallelOuterToBigMat(x, bm)
    expect_equal(bm[,], outer(x, x))
  })
} else {
  test_that('parallelOuterToBigMat requires packages', {
    skip('bigmemory or RcppParallel not installed')
  })
}
