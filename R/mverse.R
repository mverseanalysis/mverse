#' Create a new \code{mverse} object
#'
#' Constructs a new mverse object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # create_multiverse() is an alias of mverse()
#' mv <- create_multiverse(hurricane)
#' @param data source datafame.
#' @return A \code{mverse} object with the source dataframe attached.
#' @name mverse
#' @family {mverse methods}
#' @export
mverse <- function(data) {
  .mverse <- multiverse::multiverse()
  attr(.mverse, 'source') <- data
  attr(.mverse, 'manipulate_branches') <- list()
  attr(.mverse, 'model_branches') <- list()
  # attr(.mverse, 'branch_asserts') <- list()
  attr(.mverse, "class") <- c("mverse", class(.mverse))
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
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'  MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1)
#' mv <- mv %>%
#'  add_mutate_branch(femininity)
#' # Execute the multiverse
#' execute_multiverse(mv)
#' @return The resulting \code{mverse} object.
#' @name execute_multiverse
#' @family {mverse methods}
#' @export
execute_multiverse.mverse <- function(.mverse) {
  stopifnot(inherits(.mverse, "mverse"))
  multiverse::execute_multiverse(.mverse)
  invisible(.mverse)
}
