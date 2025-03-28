| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  | 

# RRnorm function

This R function normalizes a dataset and offsets/scales it if needed.

## Parameters

- `X`: Object that should be normalized.
- `Scalar`: Object that should be scaled by (default value is 1).
- `Offset`: Object that should be offset by (default value is 0).

## Return Value

The function returns the normalized object.

## Examples

```r
Object = seq(from = -1, to = 1, by = 0.1)^2
NormObject = RRnorm(Object, Scalar = 2, Offset = -1)
plot(NormObject, col = "red")
points(Object, col = "blue")
```

In this example, we create an `Object` with values from -1 to 1, squared at each point. Then, we call the `RRnorm` function on `Object`, scaling it by 2 and offsetting it by -1. The normalized object is stored in `NormObject`, and it is plotted in red, while the original `Object` is plotted in blue.

Please note that the function assumes the input dataset `X` is numeric.
```

To use this Markdown file effectively, you can create a new file with a `.md` extension (e.g., `RRnorm.md`) in your GitHub repository. Copy the content above into the new file, and it will provide a clear explanation of the `RRnorm` function, its purpose, parameters, return value, and an example with an output plot.
