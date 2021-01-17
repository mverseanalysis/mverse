#' Create a new \code{mverse} object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' mv <- mverse(soccer)
#' @param data source datafame.
#' @return A \code{mverse} object with the source dataframe attached.
#' @name mverse
#' @family {mverse methods}
#' @export
mverse <- function(data) {
  .mverse <- multiverse::multiverse(class="mverse")
  attr(.mverse, 'source') <- data
  attr(.mverse, 'manipulate_branches') <- list()
  # attr(.mverse, 'model_branches') <- list()
  # attr(.mverse, 'branch_asserts') <- list()
  .mverse
}

#' @rdname mverse
#' @export
create_multiverse <- function(data) {
  mverse(data)
}

#' @rdname execute_multiverse
#' @export
execute_multiverse <- function(.mverse) {
  UseMethod("execute_multiverse")
}
#' Execute the entire multiverse.
#'
#' This method executes the analysis steps
#' defined in the \code{mverse} objected
#' across the entire multiverse.
#'
#' @param .mverse a \code{mverse} object.
#' @examples
#' \dontrun{
#' mv <- mv %>%
#'   execute_multiverse(old_branch)
#' }
#' @return The resulting \code{mverse} object.
#' @name execute_multiverse
#' @family {mverse methods}
#' @export
execute_multiverse.mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  multiverse::execute_multiverse(.mverse)
  invisible(.mverse)
}

#' @rdname summary
#' @export
summary <- function(.mverse) {
  UseMethod("summary")
}
#' Display the multiverse table.
#'
#' This method returns the multiverse table
#' displaying all universes defined by the multiverse.
#' Each row corresponds to a universe and each column
#' represents a branch.
#'
#' @param .mverse a \code{mverse} object.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {mverse methods}
#' @importFrom  stringr str_replace
#' @export
summary.mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  # mtable <- multiverse::multiverse_table(.mverse)
  mtable <- multiverse::expand(.mverse)
  if(length(mtable) > 0) {
    colnames(mtable) <- str_replace(
      colnames(mtable), '\\.', '')
    mtable <- mtable %>%
      select(1:(ncol(mtable)-3))
    branches <- print_branch_all(.mverse)
    mtable <- print_branches_table(
      branches, mtable)
  }
  mtable
}

#' @rdname extract
#' @export
extract <- function(...) {
  UseMethod("extract")
}
#' Extract branched values.
#'
#' \code{extract} returns a tibble of selected values
#' across the multiverse in a long format.
#'
#' @param .mverse a \code{mverse} object.
#' @param columns a character vector of column names to extract.
#' @param universe an integer vector of universe ids to extract.
#' @param nuni a positive integer for the number of universes to extract.
#' @param frow proportion of rows to extract from each universe.
#' @param ... Ignored.
#' @details This method extracts data values across
#' the multiverse. You can specify a subset of data
#' to extract using \code{columns}, \code{universe},
#'  \code{nuni}, and \code{frow}.
#'
#' You can specify the columns to extract from each
#' universe by passing the column names as a character
#' vector to \code{columns}. The default values is
#' \code{NULL} extracting all columns with branches.
#'
#' Use \code{universe} to specify a set of universes
#' by their integer ids. Use \code{nuni} to specify
#' the number of universes to extract data from. The
#' method then selects the subset randomly. Specifying
#' \code{universe} manually will override \code{nuni} value.
#' By default, they are both set to \code{NULL} and
#' the method returns data from all universes.
#'
#' Use \code{frow} to randomly extract a fraction of
#' data from each universe. The default value is \code{NULL}
#' and all rows are returned as they are. Note if you select
#' 1 the method will return shuffle rows in each universe
#' before returning them. If \code{frow} is greater than 1,
#' the method randomly samples rows with replacement.
#'
#' @name extract
#' @family {mverse methods}
#' @export
extract.mverse <- function(
  .mverse, columns = NULL, universe = NULL,
  nuni = NULL, frow = NULL, ...) {
  stopifnot(inherits(.mverse, "mverse"))
  mtable <- summary(.mverse)
  nms <- stringr::str_replace(
    colnames(mtable), "_branch", "")
  if(is.null(columns)) {
    columns <- nms}
  if(! "universe" %in% colnames(mtable)) {
    columns <- c("universe", columns)}
  if(is.null(universe)) {
    universe <- mtable$universe
    if(!is.null(nuni)) {
      if(length(nuni) > 1) {
        warning("Only the first value of nuni is used.")
        nuni <- nuni[1]
      }
      stopifnot(
        "nuni must be less than or equal to the number of universes." =
          length(universe) >= nuni)
      universe <- sample(universe, nuni)}}
  stopifnot(length(universe) > 0)
  extracted <- lapply(
    universe, function(x) {
      ex <- multiverse::extract_variable_from_universe(.mverse, x, data)
      if(!is.null(frow)) {
        stopifnot("frow must be a positive value." = frow > 0)
        ex <- dplyr::sample_frac(ex, size = frow,
                     replace = (frow > 1))}
      ex$universe <- x
      ex })
  extracted <- dplyr::bind_rows(extracted)
  extracted %>%
    select(columns) %>%
    mutate(universe = factor(universe))
}
