#' Create a new mutate branch.
#'
#' @examples
#' # Define mutate branches.
#' hurricane_strength <- mutate_branch(
#'   # damage vs. wind speed vs.pressure
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014,
#'   # Standardized versions
#'   scale(NDAM),
#'   scale(HighestWindSpeed),
#'   -scale(Minpressure_Updated_2014),
#' )
#' # Create a mverse and add the branch.
#' mv <- create_multiverse(hurricane) %>%
#'   add_mutate_branch(hurricane_strength)
#' @param ... branch definition expressions.
#' @param name Name for the new variable.
#' @return a mutate_branch object.
#'
#' @name mutate_branch
#' @family {methods for working with a mutate branch}
#' @export
mutate_branch <- function(..., name = NULL) {
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
    class = c("mutate_branch", "branch")
    )
}

#' @rdname add_mutate_branch
#' @export
add_mutate_branch <- function(.mverse, ...) {
  UseMethod("add_mutate_branch")
}

#' Add mutate branches to a \code{mverse} object.
#'
#' This method adds one or more mutate branches to
#' an existing \code{mverse} object. Mutate branches
#' are used to define options for adding a new
#' column to the analysis dataset.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{mutate_branch} objects.
#' @examples
#' # Define mutate branches.
#' hurricane_strength <- mutate_branch(
#'   # damage vs. wind speed vs.pressure
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014,
#'   # Standardized versions
#'   scale(NDAM),
#'   scale(HighestWindSpeed),
#'   -scale(Minpressure_Updated_2014),
#' )
#' y <- mutate_branch(
#'   alldeaths, log(alldeaths + 1)
#' )
#' # Create a mverse and add the branches.
#' mv <- create_multiverse(hurricane) %>%
#'   add_mutate_branch(hurricane_strength) %>%
#'   add_mutate_branch(y)
#' # You can also add multiple branches with a single call.
#' mv <- create_multiverse(hurricane) %>%
#'   add_mutate_branch(hurricane_strength, y)
#' @return The resulting \code{mverse} object.
#' @name add_mutate_branch
#' @family {methods for working with a mutate branch}
#' @export
add_mutate_branch.mverse <- function(.mverse, ...) {
  nms <- sapply(rlang::enquos(...), rlang::quo_name)
  brs <- list(...)
  stopifnot(all(sapply(brs, inherits, "mutate_branch")))
  .mverse <- add_branch(.mverse, brs, nms)
  invisible(.mverse)
}
