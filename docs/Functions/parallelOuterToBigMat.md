| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# parallelOuterToBigMat function

## Description
`parallelOuterToBigMat()` computes the outer product of a numeric vector in parallel and writes the result directly to a file-backed `big.matrix`. This avoids allocating the full matrix in memory and allows the creation of very large interaction matrices. The function relies on the **bigmemory** and **RcppParallel** packages.

## Usage
```R
parallelOuterToBigMat(x, bigmat)
```

## Parameters
- `x`: Numeric vector with values to multiply.
- `bigmat`: A `bigmemory::big.matrix` used as the output container.

## Return Value
Invisibly returns `bigmat` after it has been filled with the outer product of `x`.

## Examples
```R
library(bigmemory)
x <- rnorm(4)
bf <- tempfile(fileext = ".bin")
df <- tempfile(fileext = ".desc")
bm <- bigmemory::filebacked.big.matrix(
  nrow = length(x),
  ncol = length(x),
  type = "double",
  backingfile = basename(bf),
  descriptorfile = basename(df),
  backingpath = dirname(bf)
)
parallelOuterToBigMat(x, bm)
print(bm[, ])
```
