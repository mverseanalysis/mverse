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
#' @examples
#' # Define mutate branches.
#' hurricane_strength <- mutate_branch(
#' # damage vs. wind speed vs.pressure
#' NDAM,
#' HighestWindSpeed,
#' Minpressure_Updated_2014,
#' # Standardized versions
#' scale(NDAM),
#' scale(HighestWindSpeed),
#' -scale(Minpressure_Updated_2014),
#' )
#' y <- mutate_branch(
#' alldeaths, log(alldeaths + 1)
#' )
#' # Create a mverse and add the branches.
#' mv <- create_multiverse(hurricane) %>%
#'  add_mutate_branch(hurricane_strength, y)
#' execute_multiverse(mv)
#' # Extract all branched columns from all universes
#' extract(mv)
#' # Specify the columns to extract from each universe using \code{columns}
#' # You can select both branched and non-branched columns
#' extract(mv, columns = c("hurricane_strength", "NDAM"))
#' # Specify the universe to extract from using \code{universe}
#' extract(mv, universe = 1)
#' # Specify the number of universes to extract from using \code{nuni}
#' # The universes are randomly selected
#' extract(mv, nuni = 3)
#' # Specify the proportion of data to extract from each universe using \code{frow}
#' # The rows are randomly selected
#' extract(mv, frow = 0.7)
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
#' @name extract
#' @family {mverse methods}
#' @export
extract.mverse <- function(
  .mverse, columns = NULL,
  nuni = NULL, frow = NULL, mutate_cols = TRUE,...) {
  stopifnot(inherits(.mverse, "mverse"))
  mtable <- summary(.mverse)
  if(is.null(columns)) columns <- unlist(
    sapply(
      c(
        attr(.mverse, 'branches_list'),
        attr(.mverse, 'branches_conditioned_list')
      ),
      function(x) if(inherits(x, "mutate_branch")) x$name))
  columns <- c("universe", columns)
  #if(is.null(universe)) {
    universe <- mtable$universe
    if(!is.null(nuni)) {
      if(length(nuni) > 1) {
        warning("Only the first value of nuni is used.")
        nuni <- nuni[1]
      }
      stopifnot(
        "nuni must be less than or equal to the number of universes." =
          length(universe) >= nuni)
      universe <- sample(universe, nuni)}#}
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
  #extracted %>%
  #  dplyr::select(columns) %>%
  #  dplyr::left_join(mtable, by = "universe")
  if (mutate_cols == FALSE)
    extracted %>% dplyr::select(columns)
  else
    extracted %>% dplyr::select(columns) %>%
    dplyr::left_join(mtable, by = "universe")
}
