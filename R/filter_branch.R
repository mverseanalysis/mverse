#' Create a new filter branch.
#'
#' @examples
#' \dontrun{
#' x4 <- filter_branch(x1 + x2, x1 + x3, sum(x1, x2, x3))
#' }
#'
#' @param ... branch definition expressions.
#' @param name Name for the new filter.
#' @return a filter_branch object.
#'
#' @name filter_branch
#' @family {methods for working with a filter branch}
#' @export
filter_branch <- function(..., name = NULL) {
  rules <- rlang::enquos(...)
  if(!(is.character(name) | is.null(name)))
    stop('Error: "name" must be a character object.')
  structure(
    list(
      rules = rules,
      name = name
      ),
    class = c("filter_branch", "branch")
    )
}

#' @rdname add_filter_branch
#' @export
add_filter_branch <- function(.mverse, ...) {
  UseMethod("add_filter_branch")
}

#' Add variable branches to a \code{mverse} object.
#'
#' This method adds one or more filter branches to
#' an existing \code{mverse} object.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{filter_branch} objects.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' x3 <- filter_branch(x1+x2, mean(c(x1,x2)))
#' mv <- mv %>%
#'   add_filter_branch(x3)
#' }
#' @return The resulting \code{mverse} object.
#' @importFrom magrittr %>%
#' @import dplyr
#' @name add_filter_branch
#' @family {methods for working with a filter branch}
#' @export
add_filter_branch.mverse <- function(.mverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name filter
  branch_rules <- mapply(
    function(rl, nm) {
      if(is.null(rl$name))
        return(name(rl, nm))
      return(rl)
    },
    branch_rules, varnames, SIMPLIFY = FALSE)
  # enforce filter name
  e <- sapply(
    branch_rules,
    function(x) {
      if(grepl('^filter_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.mverse, 'manipulate_branches') <- append(
    attr(.mverse, 'manipulate_branches'),
    branch_rules)
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  invisible(.mverse)
}

#' @rdname remove_filter_branch
#' @export
remove_filter_branch <- function(.mverse, name) {
  UseMethod("remove_filter_branch")
}

#' Remove a filter branch from a \code{mverse}.
#'
#' This method removes one filter branch from
#' an existing \code{mverse} object.
#'
#' @param .mverse a \code{mverse} object.
#' @param name name of the \code{filter branch} to be removed.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' mv <- mverse %>%
#'   remove_filter_branch(old_branch)
#' }
#' @return The resulting \code{mverse} object.
#' @importFrom magrittr %>%
#' @import dplyr
#' @name remove_filter_branch
#' @family {methods for working with a variable branch}
#' @export
remove_filter_branch.mverse <- function(.mverse, name) {
  fbranch_list <- attr(.mverse, 'manipulate_branches')
  fbranch_list <- fbranch_list[
    sapply(fbranch_list, function(x) inherits(x, "filter_branch"))]
  # exit if the name doesn't exist
  stopifnot(is.character(name))
  if(!(name %in% sapply(fbranch_list, function(x) x$name)))
    stop(paste(name, "is not in the filter branch list."))
  # remove from list
  attr(.mverse, 'manipulate_branches') <- fbranch_list[
    sapply(fbranch_list, function(x) x$name != name)]
  # remove from the multiverse object
  reset_parameters(.mverse)
}
