#' Create a new filter branch.
#'
#' @examples
#' # Define a filter branch.
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   !Name %in% c("Katrina"),
#'   !Name %in% c("Katrina"),
#'   TRUE # include all
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers)
#' @param ... branch definition expressions.
#' @param name Name for the new filter.
#' @return a filter_branch object.
#' @name filter_branch
#' @family {methods for working with a filter branch}
#' @export
filter_branch <- function(..., name = NULL) {
  opts <- rlang::enquos(...)
  if (!length(opts) > 0) {
    stop("Error: Provide at least one rule.")
  }
  if (!(is.character(name) | is.null(name))) {
    stop('Error: "name" must be a character object.')
  }
  structure(
    list(
      opts = opts,
      name = name
    ),
    class = c("filter_branch", "branch")
  )
}

#' @rdname add_filter_branch
#' @export
add_filter_branch <- function(.mverse, ...) {
  UseMethod("add_filter_branch")
}

#' Add filter branches to a \code{mverse} object.
#'
#' This method adds one or more filter branches to
#' an existing \code{mverse} object. Filter branches
#' are used to define options for conditions
#' for selecting subsets of data rows.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{filter_branch} objects.
#' @examples
#' # Define a filter branch.
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   !Name %in% c("Katrina"),
#'   !Name %in% c("Katrina"),
#'   TRUE # include all
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers)
#' @return The resulting \code{mverse} object.
#' @name add_filter_branch
#' @family {methods for working with a filter branch}
#' @export
add_filter_branch.mverse <- function(.mverse, ...) {
  nms <- sapply(rlang::enquos(...), rlang::quo_name)
  brs <- list(...)
  stopifnot(all(sapply(brs, inherits, "filter_branch")))
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}
