#' Create a new variable_branch object
#'
#' @examples
#' \dontrun{
#' x4 <- variableBranch(x1 + x2, x1 + x3, sum(x1, x2, x3))
#' }
#'
#' @param ... branch definition expressions.
#' @param nameName for the new variable.
#' @return a variable_branch object.
#'
#' @name variable_branch
#' @export
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


parse <- function(vBranch) {
  UseMethod('parse')
}

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

#' Add variable branches to a \code{mverse} object.
#'
#' This method adds one ore more variable branches to
#' an existing \code{mverse} object.
#'
#' @param .mverse a \code{mverse} object.
#' @param ... \code{variable_branch} objects.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' x3 <- variable_branch(x1+x2, mean(c(x1,x2)))
#' mv <- mv %>%
#'   add_variable_branch(x3)
#' }
#' @return The resulting \code{mverse} object.
#' @name add_variable_branch
#' @export
add_variable_branch <- function(.mverse, ...) {
  UseMethod("add_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname add_variable_branch
#' @export
add_variable_branch.mverse <- function(.mverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name variable branches
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
      if(grepl('^variable_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.mverse, 'variable_branches') <- append(
    attr(.mverse, 'variable_branches'),
    branch_rules)
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  invisible(.mverse)
}

#' Remove a variable branch to a \code{mverse}.
#'
#' This method removes one variable branch from
#' an existing \code{mverse} object.
#'
#' @param .mverse a \code{mverse} object.
#' @param name name of the \code{variable_branch} to be removed.
#' @examples
#' \dontrun{
#' mv <- create_multiverse(df)
#' mv <- mverse %>%
#'   remove_variable_branch(old_branch)
#' }
#' @return The resulting \code{mverse} object.
#' @name remove_variable_branch
#' @export
remove_variable_branch <- function(.mverse, name) {
  UseMethod("remove_variable_branch")
}

#' @importFrom magrittr %>%
#' @import dplyr
#' @rdname remove_variable_branch
#' @export
remove_variable_branch.mverse <- function(.mverse, name) {
  vB_list <- attr(.mverse, 'variable_branches')
  # exit if the name doesn't exist
  stopifnot(is.character(name))
  if(!(name %in% sapply(vB_list, function(x) x$name)))
    stop(paste(name, "is not in the variable branch list."))
  # remove from list
  attr(.mverse, 'variable_branches') <- vB_list[
    sapply(vB_list, function(x) x$name != name)]
  # remove from the multiverse object
  reset_parameters(.mverse)
}

reset_parameters <- function(.mverse) {
  attr(.mverse, "multiverse")[['code']] <- NULL
  .mverse$orig <- ~ attr(.mverse, 'source')
  multiverse::inside(.mverse, data <- orig)
  for (br in attr(.mverse, 'variable_branches')) {
    multiverse::inside(
      .mverse,
      data <- dplyr::mutate(
        data, !! rlang::parse_expr(br$name) := !! parse(br)))}
  .mverse
}

print_variable_branch_all <- function(.mverse) {
  vb <- attr(.mverse, 'variable_branches')
  sapply(vb, print_variable_branch_single)
}

print_variable_branch_single <- function(vb) {
  rules <- character(length(vb$rules))
  for(i in 1:length(vb$rules)) {
    rules[i] <- rlang::quo_name(vb$rules[[i]])
  }
  out <- list(rules)
  names(out) <- vb$name
  out
}

print_variable_branches_table <- function(vbprint, mtable) {
  for(nm in names(vbprint)) {
    replace_this<- paste0(nm, '_', 1:length(vbprint[[nm]]))
    brnch <- paste0(nm,'_branch')
    mtable[[brnch]] <- mtable[[brnch]] %>%
      dplyr::recode(!!! setNames(vbprint[[nm]], replace_this))
  }
  mtable
}
