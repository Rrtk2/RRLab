| [HOME](https://github.com/Rrtk2/RRLab)  |  [FUNCTIONS](https://github.com/Rrtk2/RRLab/blob/master/docs/Functions/FunctionsOverview.md)  |

# Rload Function

`Rload()` compliments `Rsave()` by loading a previously saved object with `qs::qload`. Provide an object or file name base and it rebuilds the path using the same prefix/postfix logic.

## Usage
```R
Rload(name, prefix = "", postfix = "", file_location = s_saveloc_qc, envir = parent.frame(), ...)
```

## Parameters
- `name`: The object or file name to load.
- `prefix`: Optional prefix that was used when saving.
- `postfix`: Optional postfix used when saving.
- `file_location`: Directory of the saved object, defaults to `s_saveloc_qc`.
- `envir`: Environment to load the object into. Default is the caller's environment.

## Returns
Invisibly returns the result of `qs::qload`.

## Examples
```R
Rload(a)
Rload("08A_a_cohort")
```
