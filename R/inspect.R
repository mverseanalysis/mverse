display_branch_rules <- function(mtable, object) {
  # extract all branches
  branches <- sapply(c(
    attr(object, 'manipulate_branches'),
    attr(object, "model_branches")
  ),
  function(vb) {
    rules <- character(length(vb$rules))
    for (i in 1:length(vb$rules))
      rules[i] <- rlang::quo_name(vb$rules[[i]])
    out <- list(rules)
    names(out) <- vb$name
    out
  })
  for (nm in names(branches)) {
    replace_this <- paste(nm, 1:length(branches[[nm]]), sep = '_')
    brnch <- paste(nm, 'branch', sep = "_")
    mtable[[brnch]] <- mtable[[brnch]] %>%
      dplyr::recode(!!!stats::setNames(branches[[nm]], replace_this)) %>%
      factor()
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
#' # Display the multiverse table while
#' # defining a multiverse provides a way
#' # to inspect the universes created.
#' summary(mv)
#' # Define a filter branch.
#' hurricane_outliers <- filter_branch(
#'   ! Name %in% c("Katrina", "Audrey", "Andrew"),
#'   ! Name %in% c("Katrina"),
#'   ! Name %in% c("Katrina"),
#'   TRUE # include all
#' )
#' # Add the additional branch and inspect.
#' mv <- add_filter_branch(mv, hurricane_outliers)
#' summary(mv)
#' @param object a \code{mverse} object.
#' @param ... Ignored.
#' @return a multiverse table as a tibble.
#' @name summary
#' @family {summary method}
#' @export
summary.mverse <- function(object, ...) {
  mtable <- multiverse::extract_variables(object) %>%
    dplyr::mutate(universe = factor(.universe)) %>%
    dplyr::select(-tidyselect::starts_with(".")) %>%
    dplyr::select(universe, tidyselect::everything())
  display_branch_rules(mtable, object)
}


#' Display a summary of fitting \code{lm} across the multiverse.
#'
#' \code{summary.lm_mverse} returns the \code{lm} regression
#' results across the multiverse.
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
#' # Define a filter branch.
#' hurricane_outliers <- filter_branch(
#' ! Name %in% c("Katrina", "Audrey", "Andrew"),
#' ! Name %in% c("Katrina"),
#' ! Name %in% c("Katrina"),
#' TRUE # include all
#' )
#' # Define a formula branch.
#' model_specifications <- formula_branch(
#' y ~ femininity,
#' y ~ femininity + hurricane_strength,
#' y ~ femininity * hurricane_strength
#' )
#' # Create a mverse, add the branches, and fit lm models.
#' mv <- create_multiverse(hurricane) %>%
#' add_filter_branch(hurricane_outliers) %>%
#' add_mutate_branch(hurricane_strength, y) %>%
#' add_formula_branch(model_specifications) %>%
#' lm_mverse()
#' # Display the multiverse table with estimated coefficients.
#' summary(mv)
#' @param object a \code{lm_mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates" ("e"), "df", "fstatistic" ("f"), and "r.squared" ("r").
#'   Default value is "estimates".
#' @param ... Ignored.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @export
summary.lm_mverse <- function(object,
                              conf.int = TRUE,
                              conf.level = 0.95,
                              output = "estimates",
                              ...) {
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <- broom::tidy(model, !!rlang::enexpr(conf.int), !!rlang::enexpr(conf.level))
      else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int))
          out <- out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
      }
    })
  } else if (output == "df") {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <- as.data.frame(t(summary(model)$df)) %>%
          dplyr::rename(p = V1,
                 n.minus.p = V2,
                 p.star = V3)
      else
        out <- data.frame(p = NA,
                          n.minus.p = NA,
                          p.star = NA)
    })
  } else if (output %in% c("r.squared", "r")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <-
          as.data.frame(t(c(
            summary(model)$r.squared,
            summary(model)$adj.r.squared
          ))) %>%
          dplyr::rename(r.squared = V1, adj.r.squared = V2)
      else
        out <- data.frame(r.squared = NA, adj.r.squared = NA)
    })
  } else if (output %in% c("fstatistic", "f")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 1)
        out <- as.data.frame(t(summary(model)$fstatistic)) %>%
          dplyr::rename(fstatistic = value,
                 numdf.f = numdf,
                 dendf.f = dendf)
      else
        out <- data.frame(fstatistic = NA,
                          numdf.f = NA,
                          dendf.f = NA)
    })
  } else
    stop("Invalid output argument.")
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>%
    tidyr::unnest(out) %>%
    dplyr::mutate(universe = factor(.universe)) %>%
    dplyr::select(-tidyselect::starts_with(".")) %>%
    dplyr::select(universe, tidyselect::everything())
  display_branch_rules(mtable, object)
}


#' Display a summary of fitting \code{glm} across the multiverse.
#'
#' \code{summary.glm_mverse} returns the \code{glm} regression
#' results across the multiverse.
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
#' # Define a filter branch.
#' hurricane_outliers <- filter_branch(
#'   ! Name %in% c("Katrina", "Audrey", "Andrew"),
#' ! Name %in% c("Katrina"),
#' ! Name %in% c("Katrina"),
#' TRUE # include all
#' )
#' # Define a formula branch.
#' model_specifications <- formula_branch(
#' y ~ femininity,
#' y ~ femininity + hurricane_strength,
#' y ~ femininity * hurricane_strength
#' )
#' # Define a family branch.
#' model_distributions <- family_branch(gaussian, poisson)
#' # Create a mverse, add the branches, and fit glm models.
#' mv <- create_multiverse(hurricane) %>%
#' add_filter_branch(hurricane_outliers) %>%
#' add_mutate_branch(hurricane_strength, y) %>%
#' add_formula_branch(model_specifications) %>%
#' add_family_branch(model_distributions) %>%
#' glm_mverse()
#' # Display the multiverse table with estimated coefficients.
#' summary(mv)
#' @param object a \code{glm_mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates" ("e"), "df", "deviance" ("de"), and "aic" ("bic").
#'   Alternatively, the first letters may be used. Default value
#'   is "estimates".
#' @param ... Ignored.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @export
summary.glm_mverse <- function(object,
                               conf.int = TRUE,
                               conf.level = 0.95,
                               output = "estimates",
                               ...) {
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <-
          broom::tidy(model, !!rlang::enexpr(conf.int), !!rlang::enexpr(conf.level))
      else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int))
          out <- out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
      }
    })
  } else if (output == "df") {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <-
          as.data.frame(t(c(
            summary(model)$df.residual, summary(model)$df.null
          ))) %>%
          dplyr::rename(df.residual = V1,
                 df.null = V2)
      else
        out <- data.frame(df.residual = NA,
                          df.null = NA)
    })
  } else if (output %in% c("de", "deviance")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <-
          as.data.frame(t(c(
            summary(model)$deviance, summary(model)$null.deviance
          ))) %>%
          dplyr::rename(deviance = V1,
                 null.deviance = V2)
      else
        out <- data.frame(deviance = NA,
                          null.deviance = NA)
    })
  } else if (tolower(output) %in% c("aic", "bic")) {
    multiverse::inside(object, {
      if (summary(model)$df[1] > 0)
        out <-
          as.data.frame(t(c(AIC(model), BIC(model)))) %>%
          dplyr::rename(AIC = V1,
                 BIC = V2)
      else
        out <- data.frame(AIC = NA,
                          BIC = NA)
    })
  } else {
    stop("Invalid output argument.")
  }
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>%
    tidyr::unnest(out) %>%
    dplyr::mutate(universe = factor(.universe)) %>%
    dplyr::select(-tidyselect::starts_with(".")) %>%
    dplyr::select(universe, tidyselect::everything())
  display_branch_rules(mtable, object)
}



#' Display the AIC and BIC score of the fitted models across the multiverse
#'
#' display the AIC and BIC score of \code{glm} regression
#' results across the multiverse.
#'
#' @param object a \code{glm_mverse} object.
#' @return a multiverse table as a tibble
#' @name AIC
#' @family {summary method}
#' @importFrom  stringr str_replace
#' @export
AIC.glm_mverse <- function(object) {
  df <- summary.glm_mverse(object, output = "aic")
  df$BIC <- NULL
  df
}


#' @rdname AIC
#' @export
BIC.glm_mverse <- function(object) {
  df <- summary.glm_mverse(object, output = "aic")
  df$AIC <- NULL
  df
}
