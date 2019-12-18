multiverseEdu (tentative package name)
L create_multiverse(data)
	Initialize a multiverse object with a dataset

  # Add or remove variable transformation rules
    L add_variable_branch(.multiverse, <branch definition>)
		Add rules for branching one (or more) variable
    L add_variable_assert(.multiverse, <condition definition>)
		Add one (or more) assertion rule for combining branches
    L remove_variable_branch(.multiverse, <branch identifier>)
		Remove one (or more) variable branch
    L remove_variable_assert(.multiverse, <condition identifier>)
		Remove one (or more) assertion rule for combining branches

  # Add ore remove model specifications
    L add_model_spec(.multiverse, <model specification>)
		Add one (or more) model specification
    L remove_model_spec(.multiverse, <model identifier>)
		Remove one (or more) model specification

  # View and execute multiverse
    L multiverse_table(.multiverse)
		View all combinations of parameter options and model specifications
    L execute_multiverse(.multiverse)
		Execute all analyses specified in the current multiverse
    L ??visualise_multiverse??
		Visualise multiverse paths? for inspection

  # Extract results
    L multiverse_summary(.multiverse)
		View analysis results for all universes in the multiverse
	L universe_summary(.multiverse, <universe_id>)
		Extract analysis results for a specific universe
