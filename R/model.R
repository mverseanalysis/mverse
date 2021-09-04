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
  brs <- c(attr(.mverse, 'branches_conditioned_list'), attr(.mverse, 'branches_list'))
  if (length(brs) == 0)
    stop("Exactly one formula branch is required.")
  if (sum(sapply(brs,inherits, "formula_branch")) != 1)
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
#' \code{family_branch} has been provided, it follows
#' the default behaviour of \code{glm} using the Gaussian
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
#' @export
glm_mverse <- function(.mverse, ...) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  brs <- c(attr(.mverse, 'branches_conditioned_list'), attr(.mverse, 'branches_list'))
  if (length(brs) == 0)
    stop("Exactly one formula branch is required.")
  if (sum(sapply(brs,inherits, "formula_branch")) != 1)
    stop("Exactly one formula branch is required.")
  # fit glm
  multiverse::inside(
    .mverse, model <- glm(formulae, data = data, family = family))
  attr(.mverse, "class") <- unique(c("glm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}


#' Fit \code{coxph} across the multiverse.
#'
#' \code{coxph_mverse} fits \code{coxph} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#'
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{coxph} fitted.
#' @name coxph_mverse
#' @family {model fitting methods}
#' @export
coxph_mverse <- function(.mverse, ties=NULL) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  if (!any(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")))
    stop("Exactly one formula branch is required.")
  if (sum(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")) > 1)
    stop("Exactly one formula branch is required.")
  # fix coxph
  if (is.null(ties)) {
    multiverse::inside(
      .mverse, model <- coxph(formulae, data=data)
    )
  } else {
    multiverse::inside(
      .mverse, model <- coxph(formulae, data=data, ties=!!rlang::enexpr(ties))
    )
  }
  attr(.mverse, "class") <- unique(c("coxph_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}
