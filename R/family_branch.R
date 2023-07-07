#' Create a new family branch.
#'
#' @examples
#' # Define a family branch.
#' model_distributions <- family_branch(
#'   gaussian, poisson(link = "log")
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_family_branch(model_distributions)
#' @param ... branch definition expressions.
#' @param name (optional) Name for the new family.
#' @return a \code{family_branch} object.
#' @name family_branch
#' @family family branch functions
#' @export
family_branch <- function(..., name = NULL) {
  opts <- rlang::enquos(...)
  branch(opts, names(opts), name, "family_branch")
}


#' Add family branches to a \code{mverse} object.
#'
#' This method adds one or more family branches to
#' an existing \code{mverse} object. Family branches
#' are used to define options for the analysis distributions
#' when using \code{glm_mverse()}.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{family_branch} objects.
#' @examples
#' # Define a family branch.
#' model_distributions <- family_branch(
#'   gaussian, poisson(link = "log")
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_family_branch(model_distributions)
#' @return The resulting \code{mverse} object.
#' @name add_family_branch
#' @rdname add_family_branch
#' @family family branch functions
#' @export
add_family_branch <- function(.mverse, ...) {
  nms <- sapply(rlang::enquos(...), rlang::quo_name)
  brs <- list(...)
  stopifnot(all(sapply(brs, inherits, "family_branch")))
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}
