#' Create a new mutate branch.
#'
#' @examples
#' \dontrun{
#' x4 <- mutate_branch(x1 + x2, x1 + x3, sum(x1, x2, x3))
#' }
#'
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

#' Add variable branches to a \code{mverse} object.
#'
#' This method adds one or more mutate branches to
#' an existing \code{mverse} object.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{mutate_branch} objects.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' x3 <- mutate_branch(x1+x2, mean(c(x1,x2)))
#' mv <- mv %>%
#'   add_mutate_branch(x3)
#' }
#' @return The resulting \code{mverse} object.
#' @importFrom magrittr %>%
#' @import dplyr
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

# TODO: allow removing branches
#' #' @rdname remove_mutate_branch
#' remove_mutate_branch <- function(.mverse, name) {
#'   UseMethod("remove_mutate_branch")
#' }
#'
#' #' Remove a mutate branch from a \code{mverse}.
#' #'
#' #' This method removes one mutate branch from
#' #' an existing \code{mverse} object.
#' #'
#' #' @param .mverse a \code{mverse} object.
#' #' @param name name of the \code{mutate branch} to be removed.
#' #' @examples
#' #' \dontrun{
#' #' mv <- create_multiverse(df)
#' #' mv <- mverse %>%
#' #'   remove_mutate_branch(old_branch)
#' #' }
#' #' @return The resulting \code{mverse} object.
#' #' @importFrom magrittr %>%
#' #' @import dplyr
#' #' @name remove_mutate_branch
#' #' @family {methods for working with a variable branch}
#' remove_mutate_branch.mverse <- function(.mverse, name) {
#'   mbranch_list <- attr(.mverse, 'manipulate_branches')
#'   mbranch_list <- mbranch_list[
#'     sapply(mbranch_list, function(x) inherits(x, "mutate_branch"))]
#'   # exit if the name doesn't exist
#'   stopifnot(is.character(name))
#'   if(!(name %in% sapply(mbranch_list, function(x) x$name)))
#'     stop(paste(name, "is not in the mutate branch list."))
#'   # remove from list
#'   attr(.mverse, 'manipulate_branches') <- mbranch_list[
#'     sapply(mbranch_list, function(x) x$name != name)]
#'   # remove from the multiverse object
#'   reset_parameters(.mverse)
#' }
