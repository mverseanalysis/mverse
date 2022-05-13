── R CMD check results ────────────────────────────────────────────────────────────────────────────── mverse 0.1.0 ────
Duration: 36.5s

❯ checking examples ... ERROR
  Running examples in ‘mverse-Ex.R’ failed
  The error most likely occurred in:
  
  > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
  > ### Name: spec_curve
  > ### Title: Display a specification curve across the multiverse.
  > ### Aliases: spec_curve spec_curve.lm_mverse spec_curve.glm_mverse
  > ###   spec_curve.glm.nb_mverse
  > 
  > ### ** Examples
  > 
  > # Create a mverse object
  > mv <- mverse(hurricane)
  > # Define and add a mutate branch
  > femininity <- mutate_branch(
  +   MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1
  + )
  > add_mutate_branch(mv, femininity)
  > # Define and add a formula branch
  > model <- formula_branch(
  +   alldeaths ~ femininity, log(alldeaths + 1) ~ femininity
  + )
  > add_formula_branch(mv, model)
  > # Fit a lm model
  > lm_mverse(mv)
  > # Display the specification curve
  > spec_curve(mv, var = "femininityTRUE")
  > 
  > # Create a mverse object
  > mv <- mverse(hurricane)
  > # Define and add a mutate branch
  > femininity <- mutate_branch(
  +   MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1
  + )
  > add_mutate_branch(mv, femininity)
  > # Define and add a formula branch
  > model <- formula_branch(
  +   alldeaths ~ femininity, alldeaths ~ femininity * HighestWindSpeed
  + )
  > add_formula_branch(mv, model)
  > # Define and add a family branch
  > model_family <- family_branch(gaussian, poisson)
  > add_family_branch(mv, model_family)
  > # Fit a glm model
  > glm_mverse(mv)
  > # Display the specification curve
  > spec_curve(mv, var = "femininityTRUE")
  > 
  > # Create a mverse object
  > mv <- mverse(hurricane)
  > # Define and add a mutate branch
  > femininity <- mutate_branch(
  +   MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1
  + )
  > add_mutate_branch(mv, femininity)
  > # Define and add a formula branch
  > model <- formula_branch(
  +   alldeaths ~ femininity, alldeaths ~ femininity * HighestWindSpeed
  + )
  > add_formula_branch(mv, model)
  > # Define and add a family branch
  > # Fit a glm.nb model
  > glm_mverse(mv)
  Warning in (function (.universe_list, .universes, .i)  :
    error in default universe
  Error in UseMethod("family") : 
    no applicable method for 'family' applied to an object of class "NULL"
  glm_mverse -> multiverse::inside -> execute_universe -> mapply ->  -> execute_code_from_universe -> tryStack -> lapply -> FUN -> FUN -> stats::glm -> family -> UseMethod("family")
  > # Display the specification curve
  > spec_curve(mv, var = "femininityTRUE")
  Warning in (function (.universe_list, .universes, .i)  :
    error in default universe
  Error in summary(model)$df : $ operator is invalid for atomic vectors
  spec_curve -> spec_curve.glm_mverse -> summary -> dplyr::filter -> summary -> summary.glm_mverse -> multiverse::inside -> execute_universe -> mapply ->  -> execute_code_from_universe -> tryStack -> lapply -> FUN -> FUN -> summary(model)$df
  Error in `dplyr::filter()`:
  ! Problem while computing `..1 = .data$term == var`.
  Caused by error in `.data$term`:
  ! Column `term` not found in `.data`.
  Backtrace:
       ▆
    1. ├─mverse::spec_curve(mv, var = "femininityTRUE")
    2. ├─mverse:::spec_curve.glm_mverse(mv, var = "femininityTRUE")
    3. │ └─... %>% dplyr::filter(.data$term == var)
    4. ├─dplyr::filter(., .data$term == var)
    5. ├─dplyr:::filter.data.frame(., .data$term == var)
    6. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
    7. │   └─dplyr:::filter_eval(dots, mask = mask, error_call = error_call)
    8. │     ├─base::withCallingHandlers(...)
    9. │     └─mask$eval_all_filter(dots, env_filter)
   10. ├─term
   11. ├─rlang:::`$.rlang_data_pronoun`(.data, term)
   12. │ └─rlang:::data_pronoun_get(...)
   13. └─rlang:::abort_data_pronoun(x, call = y)
   14.   └─rlang::abort(msg, "rlang_error_data_pronoun_not_found", call = call)
  Execution halted

❯ checking Rd \usage sections ... WARNING
  Undocumented arguments in documentation object 'extract'
    ‘mutate_cols’
  Documented arguments not in \usage in documentation object 'extract':
    ‘universe’
  
  Undocumented arguments in documentation object 'glm.nb_mverse'
    ‘.mverse’
  
  Undocumented arguments in documentation object 'summary.glm.nb_mverse'
    ‘object’ ‘conf.int’ ‘conf.level’ ‘output’
  
  Functions with \usage entries need to have the appropriate \alias
  entries, and all their arguments documented.
  The \usage entries must correspond to syntactically valid R code.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.

❯ checking Rd contents ... WARNING
  Argument items with no description in Rd object 'summary.glm.nb_mverse':
    ‘...’

❯ checking R code for possible problems ... NOTE
  extract.mverse : <anonymous>: no visible binding for global variable
    ‘data’
  glm.nb_mverse: no visible binding for global variable ‘formulae’
  glm.nb_mverse: no visible binding for global variable ‘data’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘p.value’
  spec_curve.glm.nb_mverse: no visible binding for global variable ‘term’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘estimate’
  spec_curve.glm.nb_mverse: no visible binding for global variable ‘.’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘.universe’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘conf.low’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘conf.high’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘parameter_name’
  spec_curve.glm.nb_mverse: no visible binding for global variable
    ‘parameter_option’
  spec_curve.glm_mverse: no visible binding for global variable ‘p.value’
  spec_curve.lm_mverse: no visible binding for global variable ‘p.value’
  summary.glm.nb_mverse: no visible binding for global variable ‘model’
  summary.glm.nb_mverse: no visible binding for global variable ‘V1’
  summary.glm.nb_mverse: no visible binding for global variable ‘V2’
  summary.glm.nb_mverse: no visible binding for global variable
    ‘.universe’
  summary.glm.nb_mverse: no visible binding for global variable
    ‘universe’
  Undefined global functions or variables:
    . .universe V1 V2 conf.high conf.low data estimate formulae model
    p.value parameter_name parameter_option term universe
  Consider adding
    importFrom("utils", "data")
  to your NAMESPACE file.

❯ checking Rd files ... NOTE
  prepare_Rd: summary.glm.nb_mverse.Rd:12-14: Dropping empty section \value

1 error ✖ | 2 warnings ✖ | 2 notes ✖
Error: R CMD check found ERRORs
Execution halted

Exited with status 1.
