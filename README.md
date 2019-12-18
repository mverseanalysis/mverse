# multianalysis (tentative name)
Multiverse analysis for research and teaching.

Below is the current draft design. 

## Multiverse analysis steps

1.  Branch variables
2.  Inspect and visualise the resulting multiverse
3.  Exclude illogical/unnecessary universes
    +   Repeat steps 1 to 3 as necessary
4.  Specify models
5.  Inpsect and visualise the resulting multiverse
6.  Exclude illogical/unnecessary universes
    +   Repeat steps 4 to 6 as necessary
7.  Fit models and visualise results

## Methods

### Initialisation 

+   `create_multiverse(data)` : Initialize a multiverse object with a dataset

### Variable transformation rules

+   `variable_branch(<branch definition>)` : Define a rule for branching a variable.
+   `add_variable_branch(.multiverse, <branch definition>)` : Add variable branching rules to a multiverse object.
+   `add_variable_exclusion(.multiverse, <condition definition>)` : Add variable branching exclusion rules to a multiverse object.
+   `remove_variable_branch(.multiverse, <branch names?>)` : Remove variable branching rules from a multiverse object.
+   `remove_variable_exclusion(.multiverse, <condition names?>)` : Remove variable branching exclusion rules from a multiverse object.

### Model specificiations
+   `model_spec(<model specification>)` : Define a rule for branching a variable.
+   `add_model_spec(.multiverse, <model specification>)` : Add a model specification to a multiverse object.
+   `remove_model_spec(.multiverse, <model name?>)` : Remove a model specification from a multiverse object.

### View and execute multiverse
+   `multiverse_table(.multiverse)` : View all combinations of parameter options and model specifications.
+   `execute_multiverse(.multiverse)` : Execute all analyses specified in the current multiverse.
+   `display_multiverse(.multiverse)` : Visualise multiverse for inspection (visualisations TBD).

### Extract results
+   `summary(.multiverse, <universe_id)` : View analysis results for one ore more universes in the multiverse.
