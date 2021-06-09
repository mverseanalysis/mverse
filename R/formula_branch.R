#' Create a new formula branch.
#'
#' @examples
#' # Define a formula branch.
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity + hurricane_strength,
#'   y ~ femininity * hurricane_strength
#' )
#' # Create a mverse, add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_formula_branch(model_specifications)
#' @param ... branch definition expressions.
#' @param name Name for the new formula.
#' @return a \code{formula_branch} object.
#'
#' @name formula_branch
#' @family {methods for working with a formula branch}
#' @export
formula_branch <- function(..., name = NULL) {
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
    class = c("formula_branch", "branch")
    )
}

#' @rdname add_formula_branch
#' @export
add_formula_branch <- function(.mverse, ...) {
  UseMethod("add_formula_branch")
}

#' Add formula branches to a \code{mverse} object.
#'
#' This method adds one or more formula branches to
#' an existing \code{mverse} object. The formula branches
#' are used to specify a model for the main analysis.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{formula_branch} objects.
#' @examples
#' # Define a formula branch.
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity + hurricane_strength,
#'   y ~ femininity * hurricane_strength
#' )
#' # Create a mverse, add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_formula_branch(model_specifications)
#' @return The resulting \code{mverse} object.
#' @name add_formula_branch
#' @family {methods for working with a formula branch}
#' @export
add_formula_branch.mverse <- function(.mverse, ...) {
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
      if(grepl('^formula_branch(.+)$', x$name)) {
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
