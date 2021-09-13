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
