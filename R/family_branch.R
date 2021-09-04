#' Create a new family branch.
#'
#' @examples
#' # Define a family branch.
#' model_distributions <- family_branch(
#'  gaussian, poisson(link = "log")
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_family_branch(model_distributions)
#' @param ... branch definition expressions.
#' @param name Name for the new family.
#' @return a \code{family_branch} object.
#' @name family_branch
#' @family {methods for working with a family branch}
#' @export
family_branch <- function(..., name = NULL) {
  opts <- rlang::enquos(...)
  if(!length(opts) > 0)
    stop('Error: Provide at least one rule.')
  if(!(is.character(name) | is.null(name)))
    stop('Error: "name" must be a character object.')
  structure(
    list(
      opts = opts,
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
#' an existing \code{mverse} object. Family branches
#' are used to define options for the analysis distributions
#' when using \code{glm_mverse()}.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{family_branch} objects.
#' @examples
#' # Define a family branch.
#' model_distributions <- family_branch(
#'  gaussian, poisson(link = "log")
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_family_branch(model_distributions)
#' @return The resulting \code{mverse} object.
#' @name add_family_branch
#' @family {methods for working with a family branch}
#' @export
add_family_branch.mverse <- function(.mverse, ...) {
  nms <- sapply(rlang::enquos(...), rlang::quo_name)
  brs <- list(...)
  stopifnot(all(sapply(brs, inherits, "family_branch")))
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}
