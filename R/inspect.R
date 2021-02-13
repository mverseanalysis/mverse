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
#' @return a multiverse table as a tibble.
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
#' \code{summary.lm_mverse} returns the \code{lm} regression
#' results across the multiverse.
#'
#' @param object a \code{mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confindence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates", "df", "fstatistic", and "r.squared".
#'   Alternatively, the first letters may be used. Default value
#'   is "estimates".
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @importFrom  stringr str_replace
#' @export
summary.lm_mverse <- function(
  object, conf.int = TRUE, conf.level = 0.95, output = "estimates") {
  if(output %in% c("estimates", "e")) {
    # multiverse::inside evaluates global environments
    # set "inside" parameters as global variables before passing in
    conf.int <<- conf.int
    conf.level <<- conf.level
    multiverse::inside(
      object, {
        if(summary(model)$df[1] > 0) out <- broom::tidy(model, conf.int = conf.int, conf.level = conf.level)
        else {
          out <- data.frame(
            term = "(None)", estimate = NA, std.error = NA, statistic = NA, p.value = NA)
          if(conf.int)
            out <- out %>% mutate(conf.low = NA, conf.high = NA)}})
  } else if(output %in% c("df", "d")) {
    multiverse::inside(
      object, {
        if(summary(model)$df[1] > 0) out <- as.data.frame(t(summary(model)$df)) %>%
            rename(p = V1, n.minus.p = V2, p.star = V3)
        else out <- data.frame(p = NA, n.minus.p = NA, p.star = NA)})
  } else if(output %in% c("r.squared", "r")) {
    multiverse::inside(
      object, {
        if(summary(model)$df[1] > 0)
          out <- as.data.frame(t(c(summary(model)$r.squared, summary(model)$adj.r.squared))) %>%
            rename(r.squared = V1, adj.r.squared = V2)
        else
          out <- data.frame(r.squared = NA, adj.r.squared = NA)})
  } else if(output %in% c("fstatistic", "f")) {
    multiverse::inside(
      object, {
        if(summary(model)$df[1] > 1) out <- as.data.frame(t(summary(model)$fstatistic)) %>%
          rename(fstatistic = value, numdf.f = numdf, dendf.f = dendf)
        else out <- data.frame(fstatistic = NA, numdf.f = NA, dendf.f = NA)})
  } else stop("Invalid output argument.")
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>%
    tidyr::unnest(out) %>%
    mutate(universe = factor(.universe)) %>%
    select(-starts_with(".")) %>%
    select(universe, everything())
  display_branch_rules(mtable, object)
}
