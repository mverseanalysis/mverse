#' @describeIn multiverseExtra A descriptive alias
#'   to \code{mutliverseExtra}.
create_multiverse <- function(data) {
  multiverseExtra(data)
}

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
