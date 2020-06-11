#' Create a new \code{mverse} object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' mv <- mverse(soccer)
#' @param data Source datafame.
#' @return A \code{mverse} object with the source dataframe attached.
#' @name mverse
#' @export
mverse <- function(data) {
  .mverse <- multiverse::multiverse(class="mverse")
  attr(.mverse, 'source') <- data
  attr(.mverse, 'variable_branches') <- list()
  attr(.mverse, 'model_branches') <- list()
  attr(.mverse, 'branch_asserts') <- list()
  .mverse
}

#' @rdname mverse
#' @export
create_multiverse <- function(data) {
  mverse(data)
}

#' Execute the entire multiverse.
#'
#' This method executes the analysis steps
#' defined in the \code{mverse} objected
#' across the entire multiverse.
#'
#' @param .mverse A \code{mverse} object.
#' @examples
#' \dontrun{
#' mv <- mv %>%
#'   execute_multiverse(old_branch)
#' }
#' @return The resulting \code{mverse} object.
#' @export
execute_multiverse <- function(.mverse) {
  UseMethod("execute_multiverse")
}
#' @rdname execute_multiverse
#' @export
execute_multiverse.mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  multiverse::execute_multiverse(.mverse)
  invisible(.mverse)
}

#' Display the multiverse table.
#'
#' This method returns the multiverse table
#' displaying all universes defined by the multiverse.
#' Each row corresponds to a universe and each column
#' represents a branch.
#'
#' @param .mverse A \code{mverse} object.
#' @return Multiverse table as a tibble
#' @export
summary <- function(.mverse) {
  UseMethod("summary")
}
#' @rdname summary
#' @importFrom  stringr str_replace
#' @export
summary.mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  mtable <- multiverse::multiverse_table(.mverse)
  if(length(mtable) > 0) {
    colnames(mtable) <- str_replace(
      colnames(mtable), '\\.', '')
    mtable <- mtable %>%
      select(1:(ncol(mtable)-3))
    branches <- print_variable_branch_all(.mverse)
    mtable <- print_variable_branches_table(
      branches, mtable)
  }
  mtable
}

#' Extract branched values.
#'
#' This method returns a table of selected values
#' across the multiverse in a long format. You
#' can select a subset based on the universe and
#' columns.
#'
#' @param .mverse A \code{mverse} object.
#' @param columns (optional) A vector of characters.
#'   Default: NULL.
#' @param universe (optional) Integer.
#'   Default: 0.
#' @param how (optional) 'all', 'manual', or 'random'.
#'   Default: 'all'.
#' @param n (optional) Integer.
#'   Default 0.
#' @name extract
#' @export
extract <- function(...) {
  UseMethod("extract")
}
#' @rdname extract
#' @export
extract.mverse <- function(
  .mverse, columns = NULL, universe = 0,
  how = 'all', n = 0) {
  stopifnot(inherits(.mverse, "mverse"))
  mtable <- summary(.mverse)
  colnames(mtable) <- stringr::str_replace(
    colnames(mtable), "_branch", "")
  columns <- ifelse(
    is.null(columns), names(mtable),
    c(colnames(mtable)[1], columns))
  # mtable <- mtable[c(univ, columns), ]
  mtable
}
