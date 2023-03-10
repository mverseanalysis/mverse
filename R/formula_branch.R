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
#' @family formula branch functions
#' @export
formula_branch <- function(..., name = NULL) {
  opts <- rlang::enquos(...)
  branch(opts, names(opts), name, "formula_branch")
}

#' Add formula branches to a \code{mverse} object.
#'
#' This method adds one or more formula branches to
#' an existing \code{mverse} object. Formula branches
#' are used to specify model structure options for the analysis.
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
#' @rdname add_formula_branch
#' @family formula branch functions
#' @export
add_formula_branch <- function(.mverse, ...) {
  nms <- sapply(rlang::enquos(...), rlang::quo_name)
  brs <- list(...)
  stopifnot(all(sapply(brs, inherits, "formula_branch")))
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}
