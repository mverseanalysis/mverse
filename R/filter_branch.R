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
#' @name filter_branch
#' @family {methods for working with a filter branch}
#' @export
filter_branch <- function(..., name = NULL) {
  rules <- rlang::enquos(...)
  if(!length(rules) > 0)
    stop('Error: Provide at least one rule.')
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

#' Add filter branches to a \code{mverse} object.
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
