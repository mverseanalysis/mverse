#' Create a new family branch.
#'
#' @examples
#' \dontrun{
#' fam <- family_branch(
#'   poisson, gaussian(link = "log")
#' )
#' }
#'
#' @param ... branch definition expressions.
#' @param name Name for the new family.
#' @return a \code{family_branch} object.
#'
#' @name family_branch
#' @family {methods for working with a formula branch}
#' @export
family_branch <- function(..., name = NULL) {
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
    class = c("family_branch", "branch")
    )
}

#' @rdname add_family_branch
#' @export
add_family_branch <- function(.mverse, ...) {
  UseMethod("add_family_branch")
}

#' Add family branches to a \code{mverse} object.
#'
#' This method adds one or more family branches to
#' an existing \code{mverse} object. The family branches
#' are used to specify a model family for the main analysis
#' that takes a family argument.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{family_branch} objects.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' fam <- family_branch(
#'   poisson, gaussian(link = "log")
#' )
#' mv <- mv %>%
#'   add_family_branch(fam)
#' }
#' @return The resulting \code{mverse} object.
#' @importFrom magrittr %>%
#' @import dplyr
#' @name add_family_branch
#' @family {methods for working with a family branch}
#' @export
add_family_branch.mverse <- function(.mverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name variable
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
      if(grepl('^family_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.mverse, 'model_branches') <- append(
    attr(.mverse, 'model_branches'),
    branch_rules)
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  invisible(.mverse)
}
