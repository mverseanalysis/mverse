display_branch_rules <- function(mtable, object) {
  # extract all branches
  branches <- sapply(
    c(attr(object, 'manipulate_branches'), attr(object, "model_branches")),
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
      dplyr::recode(!!! stats::setNames(branches[[nm]], replace_this)) %>%
      factor(levels = branches[[nm]])
  }
  mtable
}

#' Display the multiverse table with results.
#'
#' This method returns the multiverse table
#' displaying all universes defined by the multiverse.
#' Each row corresponds to a universe and each column
#' represents a branch.
#'
#' @param object a \code{mverse} object.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @importFrom  stringr str_replace
#' @export
summary.mverse <- function(object) {
  mtable <- multiverse::extract_variables(object) %>%
    mutate(universe = factor(.universe)) %>%
    select(-starts_with(".")) %>%
    select(universe, everything())
  display_branch_rules(mtable, object)
}


#' Display the s
#'
#' \code{summary.lm_mverse} returns the multiverse table
#' displaying all universes defined by the multiverse.
#' Each row corresponds to a universe and each column
#' represents a branch.
#'
#' @param object a \code{mverse} object.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @importFrom  stringr str_replace
#' @export
summary.lm_mverse <- function(object, conf.int = FALSE, conf.level = 0.95) {
  conf.int <- conf.int
  conf.level <- conf.level
  multiverse::inside(
    object,
    coefs <- broom::tidy(model)
    )
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, coefs) %>%
    unnest(coefs) %>%
    mutate(universe = factor(.universe)) %>%
    select(-starts_with(".")) %>%
    select(universe, everything())
  display_branch_rules(mtable, object)
}
