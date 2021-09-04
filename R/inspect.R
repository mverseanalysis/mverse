display_branch_opts <- function(mtable, .mverse) {
  # extract all branches
  branches <- sapply(c(
    attr(.mverse, 'branches_list'),
    attr(.mverse, 'branches_conditioned_list')
  ),
  function(vb) {
    opts <- character(length(vb$opts))
    for (i in 1:length(vb$opts))
      opts[i] <- rlang::quo_name(vb$opts[[i]])
    out <- list(opts)
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
  display_branch_opts(mtable, object)
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
        out <-
          broom::tidy(model,
                      !!rlang::enexpr(conf.int),
                      !!rlang::enexpr(conf.level))
      else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int))
          out <-
            out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
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
  display_branch_opts(mtable, object)
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
          broom::tidy(model,
                      !!rlang::enexpr(conf.int),
                      !!rlang::enexpr(conf.level))
      else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int))
          out <-
            out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
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
  display_branch_opts(mtable, object)
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


#' Display a summary of fitting \code{coxph} across the multiverse.
#'
#' \code{summary.coxph_mverse} returns the \code{coxph} regression
#' results across the multiverse.
#' @param object a \code{coxph_mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates" ("e"), "concordance" ("c"), "logtest" ("log"), "waldtest" ("wald"), and "sctest" ("sc").
#'   Default value is "estimates".
#' @param ... Ignored.
#' @return a multiverse table as a tibble
#' @name summary
#' @family {summary method}
#' @export
summary.coxph_mverse <- function(object,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 scale = 1,
                                 output = "estimates",
                                 ...)
{
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(model)$n > 0) {
        if (!!rlang::enexpr(conf.int) == FALSE) {
          out <-
            as.data.frame(t(c(
              summary(model, scale = !!rlang::enexpr(scale))$coefficients
            ))) %>%
            dplyr::rename(
              estimate = V1,
              std.err = V3,
              statistic = V4,
              p.value = V5,
              "exp(coef)" = V2
            )
        } else {
          out <-
            as.data.frame(t(c(
              summary(model, scale = !!rlang::enexpr(scale))$coefficients[-2]
              ,
              summary(model, conf.int = !!rlang::enexpr(conf.level))$conf.int
            ))) %>%
            dplyr::rename(
              estimate = V1,
              std.err = V2,
              statistic = V3,
              p.value = V4,
              "exp(coef)" = V5,
              "exp(-conf)" = V6,
              conf.low = V7,
              conf.hight = V8
            )
        }
      }

      else {
        out <- data.frame(
          estimate = NA,
          std.err = NA,
          statistic = NA,
          p.value = NA,
          "exp(coef)" = NA
        )
        if (!!rlang::enexpr(conf.int))
          out <-
            out %>% dplyr::mutate(
              "exp(-conf)" = NA,
              conf.low = NA,
              conf.hight = NA
            )
      }
    })
  } else if (output %in% c("c", "concordance")) {
    multiverse::inside(object, {
      if (summary(model)$n > 0)
        out <-
          as.data.frame(t(summary(model)$concordance)) %>%
          dplyr::rename(statistic = C,
                        std.err = "se(C)")
      else
        out <- data.frame(statistic = NA,
                          std.err = NA)
    })
  } else if (tolower(output) %in% c("log", "logtest")) {
    multiverse::inside(object, {
      if (summary(model)$n > 0)
        out <-
          as.data.frame(t(summary(model)$logtest))
      else
        out <- data.frame(test = NA,
                          df = NA,
                          pvalue = NA)
    })
  } else if (tolower(output) %in% c("wald", "waldtest")) {
    multiverse::inside(object, {
      if (summary(model)$n > 0)
        out <-
          as.data.frame(t(summary(model)$waldtest))
      else
        out <- data.frame(test = NA,
                          df = NA,
                          pvalue = NA)
    })
  }
  else if (tolower(output) %in% c("sc", "sctest")) {
    multiverse::inside(object, {
      if (summary(model)$n > 0)
        out <-
          as.data.frame(t(summary(model)$sctest))
      else
        out <- data.frame(test = NA,
                          df = NA,
                          pvalue = NA)
    })
  }
  else {
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




#' Plot a multiverse tree diagram.
#'
#' @param .mverse A \code{mverse} object.
#' @param label A logical. Display options as labels when TRUE.
#' @param branches A character vector. Display a subset of branches
#'   when specified. Display all when NULL.
#' @import igraph ggraph ggplot2
#' @name multiverse_tree
#' @export
multiverse_tree <-
  function(.mverse,
           label = FALSE,
           branches = NULL) {
    # sort: conditioned -> conditioned on -> others
    brs <- unique(sapply(c(
      attr(.mverse, "branches_conditioned_list"),
      sapply(attr(.mverse, "branches_conditioned_list"), function(t)
        t$conds_on),
      attr(.mverse, "branches_list")
    ),
    function(s)
      name(s)))
    if (!is.null(branches))
      brs <- brs[sapply(brs, function(x)
        x %in% branches)]
    brs_name <- paste0(brs, "_branch")
    combs <- summary(.mverse)[brs_name] %>%
      dplyr::distinct_all() %>%
      dplyr::mutate(across(.fns = as.character))
    edges_list <-
      list(data.frame(
        from = "Data",
        to = unique(combs[[1]]),
        branch = brs[1]
      ))
    v_labels <- c("Data")
    for (i in 1:length(brs_name)) {
      pairs <- combs[, 1:i] %>%
        dplyr::distinct_all() %>%
        tidyr::unite("to", 1:i, remove = FALSE) %>%
        tidyr::unite("from", 2:i, remove = FALSE) %>%
        dplyr::mutate(branch = brs[i])
      if (i > 1)
        edges_list[[length(edges_list) + 1]] <- pairs %>%
          dplyr::select(from, to, branch) %>%
          dplyr::distinct_all()
      v_labels <- c(v_labels, pairs %>% dplyr::pull(i + 2))
    }
    edges <- do.call(rbind, edges_list)
    g <- graph_from_data_frame(edges)
    plt <- ggraph(g, layout = 'dendrogram', circular = FALSE) +
      geom_edge_link(aes(color = branch)) +
      theme_void() +
      coord_flip() +
      scale_y_reverse(expand = c(0.1, 0.1)) +
      scale_x_continuous(expand = c(0.1, 0.1)) +
      scale_edge_color_manual(name = NULL,
                              values = hcl(h = seq(0, 360, length.out = length(brs) + 1), l = 50),
                              limits = brs) +
      theme(legend.position = "top")
    if (label)
      plt <- plt +
      geom_node_label(
        aes(label = v_labels),
        hjust = 1,
        vjust = 1.2,
        label.size = 0,
        size = max(2, min(4, 300 / length(v_labels)))
      )
    plt + geom_node_point(size = min(1, 100 / length(v_labels)))
  }
