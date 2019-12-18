# load: Cmd + Shift + L
# check: Cmd + Shift + E
# test: Cmd + Shift + T
# document: Cmd + Shift + D (devtools::document())

#' Define a \code{multiverse} with data.
#'
#' This function defines a \code{multiverse} object with
#' the given data set.
#'
#' @param data A raw data set to create a multiverse from.
#' @return A \code{multivese} object with the source data
#'   stored in \code{$source}.
#' @examples
#' set.seed(100)
#' x1 <- 1:10
#' x2 <- floor(runif(10, 0, 10))
#' df <- data.frame(
#'    x1 = x1,
#'    x2 = x2,
#'    y = x1 + 3 * x2 + rnorm(10)
#' )
#' mverse <- create_multiverse(df)
#' print(mverse$source)
create_multiverse <- function(data) {
  data_ <- rlang::enquo(data)
  M <- multiverse::multiverse()
  M$source <- formula(data_)
  return(M)
}

#' Define a variable branch.
#'
#' @param varname (optional) Name for the new variable.
#' @param ... Branch definitions.
#' @return A named list of branch definition.
variable_branch <- function(..., varname = NULL) {
  arguments <- rlang::enquos(...)
  return(list(branches = arguments, varname = varname))
}

#' Add a variable branch to a \code{multiverse}.
#'
#' This function adds one variable branch to
#' an existing \code{multiverse} object.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param ... Variable branch rules.
#' @returrn 1 if successfully added.
#' @examples
#' set.seed(100)
#' x1 <- 1:10
#' x2 <- floor(runif(10, 0, 10))
#' df <- data.frame(
#'    x1 = x1,
#'    x2 = x2,
#'    y = x1 + 3 * x2 + rnorm(10)
#' )
#' mverse <- create_multiverse(df)
#' new_branch <- variable_branch("x3", x1+x2, mean(c(x1,x2)))
#' mverse %>% add_variable_branch(new_branch)
add_variable_branch <- function(.multiverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  varnames <- mapply(
    function(v, u) {
      ifelse(is.null(u$varname), v, u$varname)
    }, varnames, branch_rules)
  # enforce variable name
  e <- sapply(varnames,
              function(x) {
                if(grepl('^variable_branch(.+)$', x)) {
                  stop(paste(
                    "Please specify a variable name for the branch rule:",
                    x))
                }})
  # TODO: add to multiverse object
  # .multiverse$universes <- ~ source %>%
  #   mutate(
  #     branch$name,
  #   )
}


#' Remove variable branch(es) from a \code{multiverse}.
#'
#' This function removes one or more variable branch to
#' an existing \code{multiverse} object.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param branch_id A variable branch identifier.
remove_variable_branch <- function(.multiverse, branch_id) {
  NULL # ask Abn. (msg)
}

#' Add variable combination assertion(s) to a \code{multiverse}.
#'
#' This function adds one or more constraints on combining variable
#' branches. Without any constraint, all possible combinations
#' are created by default.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param assertion A variable branch combination assertion rule.
add_variable_assert <- function(.multiverse) {
  NULL
}

#' Remove variable combination assertion(s) from a \code{multiverse}.
#'
#' This function removes one or more constraints on combining variable
#' branches.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param assertion A variable branch combination assertion identifier.
remove_variable_assert <- function(.multiverse) {
  NULL
}

#' Add model specification(s) to a \code{multiverse}.
#'
#' @param .muliverse A \code{multiverse} object.
#' @param model_spec A model specification.
add_model_spec <- function(.multiverse) {
  NULL
}

#' Remove model specification(s) to a \code{multiverse}.
#'
#' @param .muliverse A \code{multiverse} object.
#' @param model_spec A model specification.
remove_model_spec <- function(.multiverse) {
  NULL
}

#' Return all combinations of parameter options and model specifications.
#'
#' @param .muliverse A \code{multiverse} object.
#' @return A tibble dispalying all combinations in the multiverse.
multiverse_table <- function(.multiverse) {
  NULL
}

#' Execute all analyses specified in the multiverse.
#'
#' @param .muliverse A \code{multiverse} object.
execute_multiverse <- function(.multiverse) {
  NULL
}

#' Visualise all paths in the multiverse.
#'
#' @param .muliverse A \code{multiverse} object.
visualise_multiverse <- function(.multiverse) {
  NULL
}

#' Extract analyses results for all univeses.
#'
#' @param .muliverse A \code{multiverse} object.
#' @return A tibble with all analyses summaries.
multiverse_summary <- function(.multiverse) {
  NULL
}


#' Extract analyses results for a selected universe.
#'
#' @param .muliverse A \code{multiverse} object.
#' @return An analysis result object. e.g., \code{lm}.
universe_summary <- function(.multiverse) {
  NULL
}
