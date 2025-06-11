#' parallelOuterToBigMat
#'
#' Compute all pairwise elementwise products of the numeric vector `x`
#' and write the interaction matrix directly to a file-backed
#' `big.matrix`. The heavy lifting is performed in parallel using
#' `RcppParallel`, allowing large interaction matrices to be generated
#' without exhausting RAM.
#'
#' This function requires the `bigmemory` and `RcppParallel` packages.
#' The outer product is computed in parallel and streamed directly to
#' the file-backed matrix so vectors larger than memory can be processed.
#'
#' @param x Numeric vector. Typically a flattened matrix.
#' @param bigmat A `bigmemory::big.matrix` object used to store the
#'   interaction matrix on disk.
#'
#' @return Invisibly returns `bigmat` with updated contents.
#' @examples
#' x <- rnorm(4)
#' bf <- tempfile(fileext = ".bin")
#' df <- tempfile(fileext = ".desc")
#' bm <- bigmemory::filebacked.big.matrix(
#'   nrow = length(x), ncol = length(x),
#'   type = "double",
#'   backingfile = basename(bf),
#'   descriptorfile = basename(df),
#'   backingpath = dirname(bf)
#' )
#' parallelOuterToBigMat(x, bm)
#' bm[, ]
#' all.equal(bm[, ], outer(x, x))
#' @export
parallelOuterToBigMat <- function(x, bigmat) {
  if (!inherits(bigmat, "big.matrix")) {
    stop("bigmat must be a big.matrix object")
  }

  if (!exists("parallelOuterToBigMat_cpp", mode = "function")) {
    RcppParallel::cppFunction(
      '        
#include <RcppParallel.h>
#include <bigmemory/BigMatrix.h>
#include <bigmemory/MatrixAccessor.hpp>
using namespace RcppParallel;
using namespace Rcpp;

// [[Rcpp::depends(RcppParallel, BH, bigmemory)]]
// [[Rcpp::export]]
void parallelOuterToBigMat_cpp(NumericVector x, SEXP pBigMat) {
  Rcpp::XPtr<BigMatrix> xpMat(pBigMat);
  MatrixAccessor<double> mat(*xpMat);
  int n = x.size();

  parallelFor(0, n, [&](int i) {
    for (int j = 0; j < n; ++j) {
      mat[i][j] = x[i] * x[j];
    }
  });
}
      ')
  }

  parallelOuterToBigMat_cpp(x, bigmat@address)
  invisible(bigmat)
}
