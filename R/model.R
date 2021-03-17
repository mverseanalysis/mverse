#' Fit \code{lm} across the multiverse.
#'
#' \code{lm_mverse} fits \code{lm} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#' At least one \code{formula_branch} must have been added.
#'
#' @examples
#' \dontrun{
#' model_spec <- formula_branch(y ~ x1)
#' mv <- mv %>%
#'   add_formula_branch(model_spec) %>%
#'   fit_lm()
#' }
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{lm} fitted.
#' @name lm_mverse
#' @family {model fitting methods}
#' @export
lm_mverse <- function(.mverse, ...) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  if (!any(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")))
    stop("Exactly one formula branch is required.")
  if (sum(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")) > 1)
    stop("Exactly one formula branch is required.")
  # fit lm
  multiverse::inside(.mverse, model <- lm(formulae, data = data))
  attr(.mverse, "class") <- unique(c("lm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}


#' Fit \code{glm} across the multiverse.
#'
#' \code{glm_mverse} fits \code{glm} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#' At least one \code{formula_branch} must have been added.
#' You can also specify the underlying error distribution and
#' the link function by adding a \code{family_branch}. If no
#' \code{family_branch} has been provided, it uses the follows
#' the default behaviour of \code{glm}, which uses the Gaussian
#' distribution with an identity link.
#'
#' @examples
#' \dontrun{
#' model_spec <- formula_branch(y ~ x1)
#' fam <- family_branch(
#'   poisson, gaussian(link = "log")
#' )
#' mv <- mv %>%
#'   add_formula_branch(model_spec) %>%
#'   add_family_banch(fam) %>%
#'   fit_glm()
#' }
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{glm} fitted.
#' @name glm_mverse
#' @family {model fitting methods}
#' @export
glm_mverse <- function(.mverse, family) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  if (!any(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")))
    stop("Exactly one formula branch is required.")
  if (sum(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")) > 1)
    stop("Exactly one formula branch is required.")
  # fit glm
  multiverse::inside(
    .mverse, model <- glm(formulae, data = data, family = family))
  attr(.mverse, "class") <- unique(c("glm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}
