#' Create a new mverse object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' \dontrun{
#' M <- mverse(df)
#' }
#' @param data Source datafame.
#' @return A mverse object with the source dataframe attached.
#' @importFrom  multiverse multiverse
#' @name mverse
mverse <- function(data) {
  .multiverse <- multiverse(class="mverse")
  attr(.multiverse, 'source') <- data
  attr(.multiverse, 'variable_branches') <- list()
  attr(.multiverse, 'model_branches') <- list()
  attr(.multiverse, 'branch_asserts') <- list()
  .multiverse
}

#' Add variable branches to a \code{multiverse}.
#'
#' This method adds one ore more variable branches to
#' an existing \code{multiverse} object.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param ... Variable branch rules.
#' @examples
#' \dontrun{
#' mverse <- create_multiverse(df)
#' new_branch <- variableBranch("x3", x1+x2, mean(c(x1,x2)))
#' mverse <- mverse %>%
#'   add_variable_branch(new_branch)
#' }
#'
#' @name add_variable_branch
add_variable_branch <- function(.multiverse, ...) {
  UseMethod("add_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname add_variable_branch
add_variable_branch.mverse <- function(.multiverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name variable branches
  branch_rules <- mapply(
    function(rl, nm) {
      if(is.null(rl$name))
        return(name(rl, nm))
      return(rl)
    },
    branch_rules, varnames, SIMPLIFY = FALSE)
  # enforce variable name
  e <- sapply(
    branch_rules,
    function(x) {
      if(grepl('^variable_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.multiverse, 'variable_branches') <- append(
    attr(.multiverse, 'variable_branches'),
    branch_rules)
  # add to multiverse object
  reset_parameters(.multiverse)
}

#' Remove a variable branch to a \code{multiverse}.
#'
#' This method removes one variable branch from
#' an existing \code{multiverse} object.
#'
#' @param .multiverse A \code{multiverse} object.
#' @param name Variable branch name to be removed.
#' @examples
#' \dontrun{
#' mverse <- create_multiverse(df)
#' mverse <- mverse %>%
#'   remove_variable_branch(old_branch)
#' }
#' @name remove_variable_branch
remove_variable_branch <- function(.multiverse, name) {
  UseMethod("remove_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname remove_variable_branch
remove_variable_branch.mverse <- function(.multiverse, name) {
  vB_list <- attr(.multiverse, 'variable_branches')
  # exit if the name doesn't exist
  stopifnot(is.character(name))
  if(!(name %in% sapply(vB_list, function(x) x$name)))
    stop(paste(name, "is not in the variable branch list."))
  # remove from list
  attr(.multiverse, 'variable_branches') <- vB_list[
    sapply(vB_list, function(x) x$name != name)]
  # remove from the multiverse object
  reset_parameters(.multiverse)
}

reset_parameters <- function(.multiverse) {
  attr(.multiverse, "multiverse")[['code']] <- NULL
  .multiverse$orig <- ~ attr(.multiverse, 'source')
  multiverse::inside(.multiverse, data <- orig)
  for (br in attr(.multiverse, 'variable_branches')) {
    multiverse::inside(
      .multiverse,
      data <- dplyr::mutate(
        data, !! rlang::parse_expr(br$name) := !! parse(br)))}
  .multiverse
}
