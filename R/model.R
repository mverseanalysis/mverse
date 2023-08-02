#' Fit linear regression models across the multiverse.
#'
#' \code{lm_mverse} fits \code{lm} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#' At least one \code{formula_branch} must have been added.
#'
#' @examples
#' \donttest{
#'
#' # Fitting \code{lm} models fitted across a multiverse.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' y <- mutate_branch(
#'   alldeaths, log(alldeaths + 1)
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity + hurricane_strength
#' )
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength, y) %>%
#'   add_formula_branch(model_specifications) %>%
#'   lm_mverse()
#' }
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{lm} fitted.
#' @name lm_mverse
#' @family model fitting functions
#' @export
lm_mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  brs <- c(
    attr(.mverse, "branches_conditioned_list"), attr(.mverse, "branches_list")
  )
  if (length(brs) == 0) {
    stop("Exactly one formula branch is required.")
  }
  if (sum(sapply(brs, inherits, "formula_branch")) != 1) {
    stop("Exactly one formula branch is required.")
  }
  # fit lm
  multiverse::inside(
    .mverse,
    .model_mverse <- stats::lm(
      stats::formula(.formula_mverse),
      data = .data_mverse
    )
  )
  attr(.mverse, "class") <- unique(c("lm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}


#' Fit generalized linear regression models across the multiverse.
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
#' \donttest{
#'
#' # Fitting \code{glm} models across a multiverse.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + hurricane_strength
#' )
#' model_distributions <- family_branch(poisson)
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength) %>%
#'   add_formula_branch(model_specifications) %>%
#'   add_family_branch(model_distributions) %>%
#'   glm_mverse()
#' }
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{glm} fitted.
#' @name glm_mverse
#' @family model fitting functions
#' @export
glm_mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  brs <- c(
    attr(.mverse, "branches_conditioned_list"), attr(.mverse, "branches_list")
  )
  if (length(brs) == 0) {
    stop("Exactly one formula branch is required.")
  }
  if (sum(sapply(brs, inherits, "formula_branch")) != 1) {
    stop("Exactly one formula branch is required.")
  }
  # fit glm
  multiverse::inside(
    .mverse,
    .model_mverse <- stats::glm(
      stats::formula(.formula_mverse),
      data = .data_mverse, family = .family_mverse
    )
  )
  attr(.mverse, "class") <- unique(c("glm_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}


#' Fit negative binomial regression models across the multiverse
#'
#' \code{glm.nb_mverse} fits \code{MASS::glm.nb} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#' At least one \code{formula_branch} must have been added.
#'
#' @examples
#' \donttest{
#'
#' # Fitting \code{glm.nb} models across a multiverse.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + hurricane_strength
#' )
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength) %>%
#'   add_formula_branch(model_specifications) %>%
#'   glm.nb_mverse()
#' }
#' @param .mverse a \code{mverse} object.
#' @return A \code{mverse} object with \code{glm.nb} fitted.
#' @name glm.nb_mverse
#' @family model fitting functions
#' @export
glm.nb_mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  # check whether there is a formula branch (should be only 1)
  brs <- c(attr(.mverse, "branches_conditioned_list"), attr(.mverse, "branches_list"))
  if (length(brs) == 0) {
    stop("Exactly one formula branch is required.")
  }
  if (sum(sapply(brs, inherits, "formula_branch")) != 1) {
    stop("Exactly one formula branch is required.")
  }
  # fit glm
  multiverse::inside(
    .mverse,
    .model_mverse <- MASS::glm.nb(
      stats::formula(.formula_mverse),
      data = .data_mverse
    )
  )
  attr(.mverse, "class") <- unique(c("glm.nb_mverse", class(.mverse)))
  execute_multiverse(.mverse)
  invisible(.mverse)
}
