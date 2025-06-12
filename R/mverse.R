#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`

#' Create a new \code{mverse} object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' # Create a mverse object.
#' mv <- mverse(hurricane)
#' # create_multiverse() is an alias of mverse().
#' mv <- create_multiverse(hurricane)
#' @param data source dataframe.
#' @return A \code{mverse} object with the source dataframe attached.
#' @name mverse
#' @import multiverse
#' @export
mverse <- function(data) {
  stopifnot(is.data.frame(data))
  .mverse <- multiverse()
  attr(.mverse, "source") <- data
  attr(.mverse, "branches_list") <- list()
  attr(.mverse, "branches_conditioned_list") <- list()
  attr(.mverse, "conditions_list") <- list()
  attr(.mverse, "covariate_branches_list") <- list()
  attr(.mverse, "class") <- c("mverse", class(.mverse))
  multiverse::inside(.mverse, { .data_mverse <- attr(.mverse, "source") })
  .mverse
}

#' @rdname mverse
#' @export
create_multiverse <- function(data) {
  mverse(data)
}

#' @rdname execute_multiverse
#' @export
execute_multiverse <- function(
    .mverse, parallel = FALSE, progress = FALSE) {
  UseMethod("execute_multiverse")
}
#' Execute the entire multiverse.
#'
#' This method executes the analysis steps
#' defined in the \code{mverse} objected
#' across the entire multiverse.
#'
#' @param .mverse a \code{mverse} object.
#' @param parallel passed to \code{multiverse::execute_multiverse()} to indicate
#'   whether to execute the multiverse analysis in parallel. Defaults to FALSE.
#' @param progress passed to \code{multiverse::execute_multiverse()} to indicate
#'   whether to include a progress bar for each step of the execution. Defaults
#'   to FALSE.
#' @examples
#' # Define a mutate branch.
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
#' # The branched variables are not populated across the multiverse yet.
#' # Execute the multiverse; the variables are populated after the execution.
#' execute_multiverse(mv)
#' @return The resulting \code{mverse} object.
#' @name execute_multiverse
#' @export
execute_multiverse.mverse <- function(
    .mverse, parallel = FALSE, progress = FALSE) {
  stopifnot(inherits(.mverse, "mverse"))
  multiverse::execute_multiverse(
    .mverse, parallel = parallel, progress = progress
  )
  invisible(.mverse)
}
