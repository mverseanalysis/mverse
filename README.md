# mverse: A User Friendly Multiverse Analysis Package in R for Researchers, Educators, and Students in Statistics

_mverse_ is an extension to multiverse package 
which allows users perform to create explorable multiverse analysis
in R. This extension provides user friendly abstraction
and a set of examples for researchers, educators,
and students in statistics.

## Installation

You can install the package from [GitHub](https://github.com/mverseanalysis/mverse) with:

```
devtools::install_github("mverseanalysis/mverse")
```

##  Methods

### Initialisation 

+   `mverse(data)` : Initialize a multiverse objectwith a dataset.
+   `create_multiverse(data)` : Alias of `mverse`.

### Variable branching

+   `variable_branch(...)` : Define a rule for branching a variable.
+   `add_variable_branch(.multiverse, ...)` : Add variable branching rules to a multiverse object.
+   `add_variable_exclusion(.multiverse, ...)` : Add variable branching exclusion rules to a multiverse object.
+   `remove_variable_branch(.multiverse, ...)` : Remove variable branching rules from a multiverse object.
+   `remove_variable_exclusion(.multiverse, ...)` : Remove variable branching exclusion rules from a multiverse object.

### Model specificiations
+   `model_spec(...)` : Define a rule for branching a variable.
+   `add_model_spec(.multiverse, ...)` : Add a model specification to a multiverse object.
+   `remove_model_spec(.multiverse, ...)` : Remove a model specification from a multiverse object.

### View and execute multiverse
+   `multiverse_table(.multiverse)` : View all combinations of parameter options and model specifications.
+   `execute_multiverse(.multiverse)` : Execute all analyses specified in the current multiverse.
+   `visualise_multiverse(.multiverse)` : Visualise multiverse for inspection _(visualisations TBD)_.

### Extract results
+   `summary(.multiverse, <universe_id)` : View analysis results for one ore more universes in the multiverse.

# References

Steegen S, Tuerlinckx F, Gelman A, Vanpaemel W. _Increasing Transparency Through a Multiverse Analysis._ Perspect Psychol Sci. 2016;11(5):702‐712. doi:10.1177/1745691616658637 URL: https://pubmed.ncbi.nlm.nih.gov/27694465/

Sarma, A. and Kay, M. _Multidy: An R package for creating multiverse analysis._ 2020. https://mucollective.github.io/multiverse/
