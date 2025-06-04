| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# Rsave Function

`Rsave()` is a lightweight wrapper around `qs::qsavem` to store an object using its name. It automatically appends optional prefixes or postfixes and defaults to saving in `s_saveloc_qc`.

## Usage
```R
Rsave(object, prefix = "", postfix = "", file_location = s_saveloc_qc, ...)
```

## Parameters
- `object`: Object to save or the name of the object.
- `prefix`: Optional prefix added to the file name.
- `postfix`: Optional postfix added to the file name.
- `file_location`: Directory where the file will be stored. Defaults to `s_saveloc_qc`.

## Returns
The full file path invisibly, after the object is saved with `qs::qsavem`.

## Examples
```R
a <- 1:5
Rsave(a)
Rsave(a, prefix = "08A", postfix = "cohort")
```
