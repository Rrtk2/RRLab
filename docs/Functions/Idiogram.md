| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  | 

# Idiogram Function

The `Idiogram` function is used to create an idiogram from a dataframe object based on CpGs (Cytosine-phosphate-Guanine sites). It indicates the chromosomal location of the CpGs found and their significance level. This function currently supports only chromosomes 1 to 22, but it can be extended if needed.

## Usage

```R
Idiogram(data_points, s_barwidth = 0.4, s_size_line = 1, s_alpha_lines = 0.33, s_offset = 0.1, s_size_point = 2)
```

## Parameters

- `data_points`: A dataframe that must contain the following columns: "seqnames" (chromosome name), "start" (start position of CpG), "end" (end position of CpG), and "pvalue" (significance level of CpG). Other columns are optional.
- `s_barwidth`: The width of the chromosome bars in the plot (default is 0.4).
- `s_size_line`: The size of CpG lines in the plot (default is 1).
- `s_alpha_lines`: The alpha (transparency) value of CpG lines in the plot (default is 0.33).
- `s_offset`: The offset of CpG significance points to the lines in the plot (default is 0.1).
- `s_size_point`: The size of CpG significance points in the plot (default is 2).

## Examples

```R
#' Example use
npoints = 1000
data_points <- sort(regioneR::createRandomRegions(nregions=npoints, mask=NA))
data_points$pvalue = runif(npoints)
Idiogram(data_points)
```

## Output

The function will produce an idiogram plot, showing the chromosomes and their respective CpGs. Chromosome bars are plotted with the width specified by `s_barwidth`. CpG lines are plotted with size `s_size_line` and transparency `s_alpha_lines`. CpG significance points are plotted with size `s_size_point` and an offset of `s_offset` from the lines.

Please note that the `regioneR`, `ggplot2`, `scales`, `data.table`, and `ggnewscale` packages are required for this function to work correctly.