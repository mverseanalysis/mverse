# mverse 0.2.1

## Minor patches

+  Fixed `multiverse_tree()` colouring by branch.
+  Added `label_hjust` and `label_vjust` optional arguments to 
`multiverse_tree()`.
+  Updated `add_family_branch()` and `add_formula_branch()` so that they take 
only one branch at a time. `mverse` can only save one each of the two branch 
types.
+  Added optional arguments `parallel` and `progress` to `execute_multiverse()`.
The arguments are passed to `multiverse::execute_multiverse()` for parallel 
processing and progress bar display.
+  Avoid redundant `execute_multiverse()` in `summary()` functions by extracting
model objects from multiverse environments instead of executing the multiverses 
each time.

# mverse 0.2.0

## Major changes

+  `spe_curve()` is simplified with the introduction of `spec_table()`. 
It also returns a `ggplot` object now for further modification of the 
aesthetics.
+  Renamed `ttest_mverse()` to `t_test_mverse()` to be consistent.

## Internal changes

+  Branch options are named and stored when `*_branch` objects are 
defined. 
+  Global variables are declared to avoid variable missing in scope 
message: ".data_mverse", ".formula_mverse", ".family_mverse", 
".model_mverse".
+  Fixed compatibility issue with `multiverse` 0.6.2 update by using
`multiverse:::Multiverse$new()` to reset multiverse environment
inside `reset_parameters()` to be compatible (#54).
+  Fixed `t_test_mverse()` documentation example.

# mverse 0.1.0
## Major changes

+  First CRAN release
