#' Create a new \code{mverse} object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' \dontrun{
#' M <- mverse(df)
#' }
#' @param data Source datafame.
#' @return A \code{mverse} object with the source dataframe attached.
#' @importFrom  multiverse multiverse
#' @name mverse
mverse <- function(data) {
  .mverse <- multiverse(class="mverse")
  attr(.mverse, 'source') <- data
  attr(.mverse, 'variable_branches') <- list()
  attr(.mverse, 'model_branches') <- list()
  attr(.mverse, 'branch_asserts') <- list()
  .mverse
}

#' @describeIn mverse A descriptive alias
#'   to \code{mverse}.
create_multiverse <- function(data) {
  mverse(data)
}

#' Add variable branches to a \code{mverse} object.
#'
#' This method adds one ore more variable branches to
#' an existing \code{mverse} object.
#'
#' @param .mverse A \code{mverse} object.
#' @param ... \code{variable_branch} object(s).
#' @examples
#' \dontrun{
#' mverse <- create_multiverse(df)
#' x3 <- variable_branch(x1+x2, mean(c(x1,x2)))
#' mverse <- mverse %>%
#'   add_variable_branch(x3)
#' }
#' @return The resulting \code{mverse} object.
#' @name add_variable_branch
add_variable_branch <- function(.mverse, ...) {
  UseMethod("add_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname add_variable_branch
add_variable_branch.mverse <- function(.mverse, ...) {
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
  attr(.mverse, 'variable_branches') <- append(
    attr(.mverse, 'variable_branches'),
    branch_rules)
  # add to mverse object
  reset_parameters(.mverse)
}

#' Remove a variable branch to a \code{mverse}.
#'
#' This method removes one variable branch from
#' an existing \code{mverse} object.
#'
#' @param .mverse A \code{mverse} object.
#' @param name Name of the \code{variable_branch} to be removed.
#' @examples
#' \dontrun{
#' mverse <- create_multiverse(df)
#' mverse <- mverse %>%
#'   remove_variable_branch(old_branch)
#' }
#' @return The resulting \code{mverse} object.
#' @name remove_variable_branch
remove_variable_branch <- function(.mverse, name) {
  UseMethod("remove_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname remove_variable_branch
remove_variable_branch.mverse <- function(.mverse, name) {
  vB_list <- attr(.mverse, 'variable_branches')
  # exit if the name doesn't exist
  stopifnot(is.character(name))
  if(!(name %in% sapply(vB_list, function(x) x$name)))
    stop(paste(name, "is not in the variable branch list."))
  # remove from list
  attr(.mverse, 'variable_branches') <- vB_list[
    sapply(vB_list, function(x) x$name != name)]
  # remove from the multiverse object
  reset_parameters(.mverse)
}

reset_parameters <- function(.mverse) {
  attr(.mverse, "multiverse")[['code']] <- NULL
  .mverse$orig <- ~ attr(.mverse, 'source')
  multiverse::inside(.mverse, data <- orig)
  for (br in attr(.mverse, 'variable_branches')) {
    multiverse::inside(
      .mverse,
      data <- dplyr::mutate(
        data, !! rlang::parse_expr(br$name) := !! parse(br)))}
  .mverse
}
