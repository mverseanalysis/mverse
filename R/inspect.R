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
  mtable <- multiverse::expand(.mverse)
  colnames(mtable) <- str_replace(
    colnames(mtable), '\\.', '')
  mtable <- mtable %>%
    select(1:(ncol(mtable)-3)) %>%
    mutate(universe = factor(universe))
  # extract all branches
  branches <- sapply(
    attr(.mverse, 'manipulate_branches'),
    function(vb) {
      rules <- character(length(vb$rules))
      for(i in 1:length(vb$rules)) rules[i] <- rlang::quo_name(vb$rules[[i]])
      out <- list(rules)
      names(out) <- vb$name
      out })
  for(nm in names(branches)) {
    replace_this <- paste(nm, 1:length(branches[[nm]]), sep = '_')
    brnch <- paste(nm, 'branch', sep = "_")
    mtable[[brnch]] <- mtable[[brnch]] %>%
      dplyr::recode(!!! stats::setNames(branches[[nm]], replace_this))
  }
  mtable
}
