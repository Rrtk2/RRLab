| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# RRgeneEnrichment Function

`RRgeneEnrichment()` is a light wrapper around the `missMethyl` package. It allows you to run a quick GO or KEGG enrichment analysis for a set of genes or CpG sites in one step. The function now also produces basic plots and automatically converts CpGs to gene symbols when a majority of supplied IDs start with `cg`.

## Usage
```R
RRgeneEnrichment(genes, organism = "org.Hs.eg.db", type = "GO",
                 pvalueCutoff = 0.05, array.type = "EPIC", ...)
```

## Parameters
- `genes`: Character vector of gene identifiers or CpG IDs. If over half of the
  IDs start with `cg` the function treats the input as CpGs and filters out any
  other entries.
- `organism`: Annotation package (for GO) or KEGG organism code.
- `type`: Either `"GO"` or `"KEGG"`.
- `pvalueCutoff`: P-value cutoff for significance.
- `array.type`: Illumina array type (when using CpGs) such as `"EPIC"` or `"450K"`.
- `...`: Additional arguments forwarded to the enrichment functions.

## Returns
Returns a list with the enrichment table, a barplot and a dotplot.

## Example
```R
# assume you already have a list of genes of interest
my_genes <- c("BRCA1", "TP53", "EGFR")

# run GO enrichment using the human annotation database
res <- RRgeneEnrichment(my_genes, organism = "org.Hs.eg.db", type = "GO")
# CpG input
cpgs <- c("cg00000029", "cg00000108")
res_cpg <- RRgeneEnrichment(cpgs, type = "GO", array.type = "EPIC")
```
