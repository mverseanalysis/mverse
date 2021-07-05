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
  rules <- rlang::enquos(...)
  if(!length(rules) > 0)
    stop('Error: Provide at least one rule.')
  if(!(is.character(name) | is.null(name)))
    stop('Error: "name" must be a character object.')
  structure(
    list(
      rules = rules,
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
#' an existing \code{mverse} object.
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
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name variable
  branch_rules <- mapply(
    function(rl, nm) {
      if(is.null(rl$name))
        return(name(rl, nm))
      return(rl)
    },
    branch_rules, varnames, SIMPLIFY = FALSE)
  # enforce variable name
  e <- sapply(
    branch_rules,
    function(x) {
      if(grepl('^mutate_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.mverse, 'manipulate_branches') <- append(
    attr(.mverse, 'manipulate_branches'),
    branch_rules)
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  invisible(.mverse)
}
