# load: Cmd + Shift + L
# check: Cmd + Shift + E
# test: Cmd + Shift + T
# document: Cmd + Shift + D (devtools::document())
# lint: lintr::lint_package()

#' Create a new multiverseExtra object
#'
#' This is an alias for \code{multiverseExtra} constructor.
#' Constructs a new multiverseExtra object which extends \code{multiverse::multiverse} object.
#'
#'
#' @examples
#' \dontrun{
#' M <- create_multiverse(df)
#' }
#' @param data Source datafame.
#' @return A multiverseExtra object with the source dataframe attached.
#' @name multiverseExtra
create_multiverse <- function(data) {
  multiverseExtra(data)
}

#' Add a variable branch to a \code{multiverse}.
#'
#' This function adds one variable branch to
#' an existing \code{multiverse} object.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param ... Variable branch rules.
#' @examples
#' \dontrun{
#' mverse <- create_multiverse(df)
#' new_branch <- variableBranch("x3", x1+x2, mean(c(x1,x2)))
#' mverse %>% add_variable_branch(new_branch)
#' }
#' @name add_variable_branch
add_variable_branch <- function(.multiverse, ...) {
  UseMethod("add_variable_branch")
}
#'
#' #' Remove variable branch(es) from a \code{multiverse}.
#' #'
#' #' This function removes one or more variable branch to
#' #' an existing \code{multiverse} object.
#' #'
#' #' @param .multiverse A \code{multiverse} object.
#' #' @param branch_id A variable branch identifier.
#' remove_variable_branch <- function(.multiverse, branch_id) {
#'   NULL # ask Abn. (msg)
#' }
#'
#' #' Add variable combination assertion(s) to a \code{multiverse}.
#' #'
#' #' This function adds one or more constraints on combining variable
#' #' branches. Without any constraint, all possible combinations
#' #' are created by default.
#' #'
#' #' @param .multiverse A \code{multiverse} object.
#' #' @param assertion A variable branch combination assertion rule.
#' add_variable_assert <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Remove variable combination assertion(s) from a \code{multiverse}.
#' #'
#' #' This function removes one or more constraints on combining variable
#' #' branches.
#' #'
#' #' @param .multiverse A \code{multiverse} object.
#' #' @param assertion A variable branch combination assertion identifier.
#' remove_variable_assert <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Add model specification(s) to a \code{multiverse}.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' #' @param model_spec A model specification.
#' add_model_spec <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Remove model specification(s) to a \code{multiverse}.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' #' @param model_spec A model specification.
#' remove_model_spec <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Return all combinations of parameter options and model specifications.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' #' @return A tibble dispalying all combinations in the multiverse.
#' multiverse_table <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Execute all analyses specified in the multiverse.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' execute_multiverse <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Visualise all paths in the multiverse.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' visualise_multiverse <- function(.multiverse) {
#'   NULL
#' }
#'
#' #' Extract analyses results for all univeses.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' #' @return A tibble with all analyses summaries.
#' multiverse_summary <- function(.multiverse) {
#'   NULL
#' }
#'
#'
#' #' Extract analyses results for a selected universe.
#' #'
#' #' @param .muliverse A \code{multiverse} object.
#' #' @return An analysis result object. e.g., \code{lm}.
#' universe_summary <- function(.multiverse) {
#'   NULL
#' }
