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
#' # Specify the covariates separately.
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   covariates = c("hurricane_strength", "Year", "Category", "NDAM")
#' )
#' model_specifications
#' @param ... branch definition expressions.
#' @param covariates (optional) A character vector of optional covariates.
#'   Each unique combination of the supplied covariates is translated into
#'   a unique branch option. See Details.
#' @param name (optional) Name for the new formula.
#' @return a \code{formula_branch} object.
#' @description
#' The function specifies the model formula for fitting `lm_mverse()` and
#' `glm_mverse()`. You can list the model specification formulae individually
#' or use \code{covariates} option paired with one or more formulae.
#' @details
#' The optional argument \code{covariates} is allows you to specify a set of
#' optional covariates in addition to other independent variable such as
#' treatment variables and blocking variables which are specified using formula.
#' For each covariate provided, a branch is added to the multiverse with the
#' option to include or exclude the covariate in the model.
#'
#' For example, \code{formula_branch(y ~ x, covariates = c("c1", "c2"))} creates
#' the following 4 model specifications:
#'
#' \code{y ~ x}
#'
#' \code{y ~ x + c1}
#'
#' \code{y ~ x + c2}
#'
#' \code{y ~ x + c1 + c2}
#'
#' Here, \code{y} is the outcome variable and \code{x} may be a treatment
#' variable in an experiment setting. \code{c1} and \code{c2} may be additional
#' covariates about the experiment units that may or may not be relevant.
#'
#' @name formula_branch
#' @family formula branch functions
#' @export
formula_branch <- function(..., covariates = NULL, name = NULL) {
  opts <- rlang::enquos(...)
  br <- branch(opts, names(opts), name, "formula_branch")
  structure(
    list(
      opts = br$opts,
      name = br$name,
      covariates = covariates
    ),
    class = class(br)
  )
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

covariate_branch <- function(covariate) {
  head_str <- paste0(
    "branch(covariate_", covariate, "_branch,"
  )
  body_str <- paste0(
    "'include_", covariate, "'~' + ", covariate, "',",
    "'exclude_", covariate, "'~'')"
  )
  rlang::parse_expr(paste0(head_str, body_str))
}

formula_with_covariates <- function(formula, covariates,
                                    .covariate_mverse) {
  paste0(formula, paste0(.covariate_mverse[covariates], collapse = ""))
}

parse.formula_branch <- function(br) {
  # initiate a branch
  head_str <- paste0(
    "branch(", br$name, "_branch,"
  )
  # construct individual formula
  has_cond <- "conds" %in% names(br)
  body_str <- paste0(
    sapply(
      names(br$opts),
      function(opt) {
        paste0(
          "'", opt, "'",
          ifelse(has_cond, br$conds[which(names(br$opts) == opt)], ""),
          "~ ",
          ifelse(is.null(br$covariates), "", "formula_with_covariates("),
          "'", rlang::quo_text(br$opts[[opt]]), "'",
          ifelse(is.null(br$covariates), "",
                 paste0(", c('",  paste(br$covariates, collapse = "', '"),
                        "'), .covariate_mverse)"))
        )
      }
    ),
    collapse = ","
  )
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ")"))
}

code_branch_formula_branch <- function(.mverse, br) {
  if (!is.null(br$covariates)) {
    for (covariate in br$covariates) {
      multiverse::inside(
        .mverse,
        if (! (!!covariate %in% names(.covariate_mverse))) {
          .covariate_mverse[!!covariate] <-
            !!covariate_branch(covariate)
        }
      )
    }
  }
  multiverse::inside(
    .mverse,
    .formula_mverse <- !!parse(br)
  )
  invisible()
}
setOldClass("formula_branch")
setMethod("code_branch", signature = signature(br = "formula_branch"),
          code_branch_formula_branch)

#' @export
print.formula_branch <- function(x, ...) {
  opts <- as_option_list(x)
  opts_m <- ""
  for (i in seq_len(length(opts))) {
    opts_m <- paste0(
      opts_m, "    - ",
      ifelse(is.null(x$name), "", paste0(names(opts)[i], " : ")),
      opts[i], "\n"
    )
  }
  conds_m <- ""
  if ("conds" %in% names(x)) {
    conds <- x$conds
    conds_m <- "  Conditions\n"
    for (j in seq_len(length(conds))) {
      if (nchar(conds[j]) > 0) {
        conds_m <- paste0(
          conds_m, "    - ", names(conds)[j], " : ",
          stringr::str_replace(conds[j], "%when% ", ""), "\n"
        )
      }
    }
  }
  covariates_m <- ""
  if ("covariates" %in% names(x)) {
    covariates_m <- "  Covariates\n"
    for (k in x$covariates) {
      covariates_m <- paste0(
        covariates_m, "    - ", k, "\n"
      )
    }
  }
  cat(
    ifelse(is.null(x$name), "<unnamed branch>", paste0(x$name, "_branch\n")),
    "  Options\n", opts_m, conds_m, covariates_m,
    sep = ""
  )
  invisible()
}
