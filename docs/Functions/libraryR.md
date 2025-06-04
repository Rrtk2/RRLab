| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# libraryR function

Tired of calling `install.packages()` only to discover the package lives on Bioconductor? Sick of cluttered install messages and broken workflows? `libraryR()` solves it.

This convenience function automatically checks if the given package(s) are installed. If not, it attempts installation from **CRAN**, and if that fails, falls back to **Bioconductor** — all silently and without interrupting your script. It supports both quoted and unquoted inputs and gives clean, minimal console output via `cli`.

Ideal for reproducible R workflows and anyone who values automation and sanity.

## Parameters

- `pkg`: A single package name or a character vector of package names. Quoted and unquoted formats are both supported.

## Return Value

Invisibly returns a named logical vector indicating whether each package was successfully loaded.

## Examples

```r
# Load multiple packages, install if needed
libraryR(c("limma","Non_existing_package", "dplyr", "BiocManager"))

# Also works with unquoted names (non-standard evaluation)
libraryR(dplyr)

# Calling it on the package itself triggers a fun easter egg
libraryR("RRLab")
```

[ ![Typical output with example](/docs/Functions/example_libraryR.png)](/docs/Functions/example_libraryR.png)