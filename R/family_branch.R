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
  branch(opts, name, "family_branch")
}


#' Add family branches to a \code{mverse} object.
#'
#' This method adds a family branch to an existing \code{mverse} object.
#' A family branch is used to define options for the analysis distributions
#' when using \code{glm_mverse()}.
#'
#' @param .mverse a \code{mverse} object.
#' @param br a \code{family_branch} object.
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
add_family_branch <- function(.mverse, br) {
  nms <- rlang::quo_name(rlang::enquo(br))
  stopifnot(inherits(br, "family_branch"))
  brs <- list(br)
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}

code_branch_family_branch <- function(.mverse, br) {
  multiverse::inside(
    .mverse,
    .family_mverse <- !!parse(br)
  )
  invisible()
}
methods::setOldClass("family_branch")
methods::setMethod("code_branch",
                   signature = signature(br = "family_branch"),
                   code_branch_family_branch)
