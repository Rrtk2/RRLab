| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# GetGenesFromCpGs Function

This helper returns gene annotations for a set of CpG identifiers using the EPIC array annotation from `missMethyl`.

## Usage
```R
GetGenesFromCpGs(sig.cpg = c("cg25871843","cg26405148"))
```

## Parameters
- `sig.cpg`: Character vector of CpG IDs.

## Returns
A data frame with matching gene and location annotations. The returned object is tagged with the class `"GetGenesFromCpGs"`.
