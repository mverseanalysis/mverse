#' Create a new variable_branch object
#'
#' @examples
#' \dontrun{
#' x4 <- variableBranch(x1 + x2, x1 + x3, sum(x1, x2, x3))
#' }
#'
#' @param ... Branch definitions.
#' @param name (optional) Name for the new variable.
#' @return A variable_branch object.
#'
#' @name variable_branch
variable_branch <- function(..., name = NULL) {
  rules <- rlang::enquos(...)
  if(!(is.character(name) | is.null(name)))
    stop('Error: "name" must be a character object.')
  structure(
    list(
      rules = rules,
      name = name
      ),
    class = "variable_branch"
    )
}

name.variable_branch <- function(vB, name = NULL) {
  stopifnot(inherits(vB, "variable_branch"))
  if(is.null(name))
    return(vB$name)
  stopifnot(is.character(name))
  vB$name <- name
  vB
}

name <- function(vB, name = NULL) {
  UseMethod("name")
}


#' @rdname variable_branch
#' @param vBranch A \code{variable_branch} object.
parse <- function(vBranch) {
  UseMethod('parse')
}

#' @rdname variable_branch
parse.variable_branch <- function(vBranch) {
  stopifnot(inherits(vBranch, "variable_branch"))
  # initiate a branch
  head_str <- paste0(
    "branch(", vBranch$name, "_branch,")
  # construct individual branch definitions
  idx <- 1:length(vBranch$rules)
  body_str <- paste0(
    mapply(
      function(i, x) paste0(
        "'", vBranch$name, '_', i, "'~", rlang::quo_name(x)),
      idx, vBranch$rules),
    collapse=',')
  # parse as an expresssion
  rlang::parse_expr(
    paste0(head_str, body_str, ')'))
}
