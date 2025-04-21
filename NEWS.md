# mverse 0.1.1

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
