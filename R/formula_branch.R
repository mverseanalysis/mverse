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
#' # You can specify the covariates separately to create branch options with
#' # all possible combinations of the covariates in addition to the explanatory
#' # variables specified in the formulae.
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity + hurricane_strength,
#'   y ~ femininity * hurricane_strength,
#'   covariates = c("Year", "Category", "NDAM")
#' )
#' model_specifications
#' @param ... branch definition expressions.
#' @param covariates (optional) A character vector of optional covariates.
#'   Each unique combination of the supplied covariates is translated into
#'   a unique branch option.
#' @param name (optional) Name for the new formula.
#' @return a \code{formula_branch} object.
#' @name formula_branch
#' @family formula branch functions
#' @export
formula_branch <- function(..., covariates = NULL, name = NULL) {
  opts <- rlang::enquos(...)
  if (is.null(covariates)) {
    return(branch(opts, names(opts), name, "formula_branch"))
  }
  copts <- apply(expand.grid(rep(list(c(FALSE, TRUE)), length(covariates))),
                 1, function(x) paste(covariates[x], collapse = " + "))
  opts <- lapply(opts,
                 function(x) paste(rlang::as_label({{x}}), copts, sep = " + "))
  opts <- lapply(sub(" \\+ $", "", unlist(opts)), rlang::quo)
  return(branch(opts, names(opts), name, "formula_branch"))
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

code_branch_formula_branch <- function(.mverse, br) {
  multiverse::inside(
    .mverse,
    .formula_mverse <- stats::formula(!!parse(br))
  )
  invisible()
}
setMethod("code_branch", signature = signature(br = "formula_branch"),
          code_branch_formula_branch)
