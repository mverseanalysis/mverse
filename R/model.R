#' Fit \code{lm} across the multiverse.
#'
#' \code{lm_mverse} fits \code{lm} across the multiverse
#' according to model specificaitons provided by \code{formula_branch}.
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
#' @name fit_lm
#' @family {model fitting methods}
#' @export
lm_mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  if(!any(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")))
    stop("Exactly one formual branch is required.")
  if(sum(sapply(attr(.mverse, "model_branches"), inherits, "formula_branch")) > 1)
    stop("Exactly one formual branch is required.")
  # fit lm
  multiverse::inside(.mverse, model <- lm(formulae, data = data))
  attr(.mverse, "class") <- unique(c("lm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}