display_branch_opts <- function(mtable, .mverse) {
  branches <- c(
    attr(.mverse, "branches_list"),
    attr(.mverse, "branches_conditioned_list")
  )
  for (br in branches) {
    nm <- paste0(br$name, "_branch")
    cs <- sapply(br$opts, rlang::quo_text)
    mtable[[paste0(nm, "_code")]] <- factor(
      sapply(
        mtable[[nm]],
        function(x) unname(cs)[names(cs) == x],
        USE.NAMES = FALSE
      ),
      levels = cs
    )
    mtable[[nm]] <- factor(mtable[[nm]], levels = names(cs))
  }
  mtable %>%
    dplyr::mutate(universe = factor(.data$.universe)) %>%
    dplyr::select(-tidyselect::starts_with(".")) %>%
    dplyr::select(tidyselect::all_of("universe"), tidyselect::everything())
}

#' Display the multiverse table with results.
#'
#' This method returns the multiverse table
#' displaying all universes defined by the multiverse.
#' Each row corresponds to a universe and the columns
#' include universe number, branch option name, and
#' branch option definition.
#'
#' When you pass a \code{mverse} objected fitted with model,
#' the summary table includes results of the fitted models
#' across the multiverse.
#'
#' @param object a \code{mverse} object.
#' @param ... Ignored.
#' @return a multiverse table as a tibble.
#' @name summary
#' @examples
#' \donttest{
#'
#' # Displaying the multiverse table without any fitted values.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' mv <- create_multiverse(hurricane) %>%
#'   add_mutate_branch(hurricane_strength)
#' summary(mv)
#' ## Displaying after adding a a filter branch.
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   !Name %in% c("Katrina"),
#'   TRUE # include all
#' )
#' mv <- add_filter_branch(mv, hurricane_outliers)
#' summary(mv)
#' }
#' @param object a \code{mverse} object.
#' @param ... Ignored.
#' @importFrom rlang .data
#' @export
summary.mverse <- function(object, ...) {
  mtable <- multiverse::extract_variables(object)
  display_branch_opts(mtable, object)
}


#' @examples
#' \donttest{
#'
#' # Displaying the multiverse table with \code{lm} models fitted.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' y <- mutate_branch(
#'   alldeaths, log(alldeaths + 1)
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity + hurricane_strength
#' )
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength, y) %>%
#'   add_formula_branch(model_specifications) %>%
#'   lm_mverse()
#' summary(mv)
#' }
#' @param object a \code{lm_mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates" ("e"), "df", "fstatistic" ("f"), and "r.squared" ("r").
#'   Default value is "estimates".
#' @param ... Ignored.
#' @rdname summary
#' @importFrom rlang .data
#' @export
summary.lm_mverse <- function(object, conf.int = TRUE, conf.level = 0.95,
                              output = "estimates", ...) {
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- broom::tidy(
          .model_mverse, !!rlang::enexpr(conf.int), !!rlang::enexpr(conf.level)
        )
      } else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int)) {
          out <- out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
        }
      }
    })
  } else if (output == "df") {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- as.data.frame(t(summary(.model_mverse)$df)) %>%
          dplyr::rename(
            p = "V1",
            n.minus.p = "V2",
            p.star = "V3"
          )
      } else {
        out <- data.frame(
          p = NA,
          n.minus.p = NA,
          p.star = NA
        )
      }
    })
  } else if (output %in% c("r.squared", "r")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- as.data.frame(
          t(c(summary(.model_mverse)$r.squared, summary(.model_mverse)$adj.r.squared))
        ) %>%
          dplyr::rename(r.squared = "V1", adj.r.squared = "V2")
      } else {
        out <- data.frame(r.squared = NA, adj.r.squared = NA)
      }
    })
  } else if (output %in% c("fstatistic", "f")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 1) {
        out <- as.data.frame(t(summary(.model_mverse)$fstatistic)) %>%
          dplyr::rename(
            fstatistic = "value",
            numdf.f = "numdf",
            dendf.f = "dendf"
          )
      } else {
        out <- data.frame(fstatistic = NA, numdf.f = NA, dendf.f = NA)
      }
    })
  } else {
    stop("Invalid output argument.")
  }
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>% tidyr::unnest(out) 
  display_branch_opts(mtable, object)
}


#' @examples
#' \donttest{
#'
#' # Displaying the multiverse table with \code{glm} models fitted.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + hurricane_strength
#' )
#' model_distributions <- family_branch(poisson)
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength) %>%
#'   add_formula_branch(model_specifications) %>%
#'   add_family_branch(model_distributions) %>%
#'   glm_mverse()
#' summary(mv)
#' }
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
#' @rdname summary
#' @importFrom rlang .data
#' @export
summary.glm_mverse <- function(object, conf.int = TRUE, conf.level = 0.95,
                               output = "estimates", ...) {
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- broom::tidy(
          .model_mverse, !!rlang::enexpr(conf.int), !!rlang::enexpr(conf.level)
        )
      } else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int)) {
          out$conf.low <- NA
          out$conf.hight <- NA
        }
      }
    })
  } else if (output == "df") {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- as.data.frame(
          t(c(summary(.model_mverse)$df.residual, summary(.model_mverse)$df.null))
        ) %>%
          dplyr::rename(df.residual = "V1", df.null = "V2")
      } else {
        out <- data.frame(df.residual = NA, df.null = NA)
      }
    })
  } else if (output %in% c("de", "deviance")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- as.data.frame(
          t(c(summary(.model_mverse)$deviance, summary(.model_mverse)$null.deviance))
        ) %>%
          dplyr::rename(deviance = "V1", null.deviance = "V2")
      } else {
        out <- data.frame(deviance = NA, null.deviance = NA)
      }
    })
  } else if (tolower(output) %in% c("aic", "bic")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <- as.data.frame(t(c(stats::AIC(.model_mverse), stats::BIC(.model_mverse)))) %>%
          dplyr::rename(AIC = "V1", BIC = "V2")
      } else {
        out <- data.frame(AIC = NA, BIC = NA)
      }
    })
  } else {
    stop("Invalid output argument.")
  }
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>% tidyr::unnest(out) 
  display_branch_opts(mtable, object)
}

#' @examples
#' \donttest{
#'
#' # Displaying the multiverse table with \code{glm.nb} models fitted.
#' hurricane_strength <- mutate_branch(
#'   NDAM,
#'   HighestWindSpeed,
#'   Minpressure_Updated_2014
#' )
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + hurricane_strength
#' )
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
#'   add_mutate_branch(hurricane_strength) %>%
#'   add_formula_branch(model_specifications) %>%
#'   glm.nb_mverse()
#' summary(mv)
#' }
#' @param object a \code{glm.nb_mverse} object.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param output The output of interest. The possible values are
#'   "estimates" ("e"), "df", "deviance" ("de"), and "aic" ("bic").
#'   Alternatively, the first letters may be used. Default value
#'   is "estimates".
#' @param ... Ignored.
#' @rdname summary
#' @importFrom rlang .data
#' @export
summary.glm.nb_mverse <- function(object, conf.int = TRUE, conf.level = 0.95,
                                  output = "estimates", ...) {
  if (output %in% c("estimates", "e")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <-
          broom::tidy(
            .model_mverse,
            !!rlang::enexpr(conf.int),
            !!rlang::enexpr(conf.level)
          )
      } else {
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        if (!!rlang::enexpr(conf.int)) {
          out <-
            out %>% dplyr::mutate(conf.low = NA, conf.high = NA)
        }
      }
    })
  } else if (output == "df") {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <-
          as.data.frame(t(c(
            summary(.model_mverse)$df.residual, summary(.model_mverse)$df.null
          ))) %>%
          dplyr::rename(
            df.residual = "V1",
            df.null = "V2"
          )
      } else {
        out <- data.frame(
          df.residual = NA,
          df.null = NA
        )
      }
    })
  } else if (output %in% c("de", "deviance")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <-
          as.data.frame(t(c(
            summary(.model_mverse)$deviance, summary(.model_mverse)$null.deviance
          ))) %>%
          dplyr::rename(
            deviance = "V1",
            null.deviance = "V2"
          )
      } else {
        out <- data.frame(
          deviance = NA,
          null.deviance = NA
        )
      }
    })
  } else if (tolower(output) %in% c("aic", "bic")) {
    multiverse::inside(object, {
      if (summary(.model_mverse)$df[1] > 0) {
        out <-
          as.data.frame(t(c(stats::AIC(.model_mverse), stats::BIC(.model_mverse)))) %>%
          dplyr::rename(
            AIC = "V1",
            BIC = "V2"
          )
      } else {
        out <- data.frame(
          AIC = NA,
          BIC = NA
        )
      }
    })
  } else {
    stop("Invalid output argument.")
  }
  execute_multiverse(object)
  mtable <- multiverse::extract_variables(object, out) %>% tidyr::unnest(out) 
  display_branch_opts(mtable, object)
}

#' Display the AIC and BIC score of the fitted models across the multiverse
#'
#' Display the AIC and BIC score of \code{glm} regression
#' results across the multiverse.
#'
#' @param object a \code{glm_mverse} object.
#' @param ... ignored. for compatibility only.
#' @param k ignored. for compatibility only.
#' @return a multiverse table as a tibble
#' @name AIC
#' @export
AIC.glm_mverse <- function(object, ..., k = 2) {
  df <- summary.glm_mverse(object, output = "aic")
  df$BIC <- NULL
  df
}

#' @rdname AIC
#' @export
BIC.glm_mverse <- function(object, ...) {
  df <- summary.glm_mverse(object, output = "aic")
  df$AIC <- NULL
  df
}

#' @rdname AIC
#' @export
AIC <- function(object, ..., k = 2) {
  UseMethod("AIC")
}

#' @rdname AIC
#' @export
BIC <- function(object, ...) {
  UseMethod("BIC")
}



#' Plot a multiverse tree diagram.
#'
#' A multiverse tree diagram displays the branching combination
#' of all the branches added to the given \code{mverse} object
#' taking any branch conditions defined. The method also allows
#' zooming into a subset of branches using \code{branches} parameter.
#'
#' @examples
#' {
#' # Display a multiverse tree with multiple branches.
#' outliers <- filter_branch(!Name %in% c("Katrina", "Audrey"), TRUE)
#' femininity <- mutate_branch(MasFem, Gender_MF)
#' strength <- mutate_branch(
#'   NDAM, HighestWindSpeed, Minpressure_Updated_2014, log(NDAM)
#' )
#' y <- mutate_branch(alldeaths, log(alldeaths + 1))
#' model <- formula_branch(y ~ femininity * strength, y ~ femininity + strength)
#' distribution <- family_branch(poisson, gaussian)
#' mv <- mverse(hurricane) %>%
#'   add_filter_branch(outliers) %>%
#'   add_mutate_branch(femininity, strength, y) %>%
#'   add_formula_branch(model) %>%
#'   add_family_branch(distribution)
#' multiverse_tree(mv)
#' # Display a multiverse tree with branch conditions.
#' match_poisson <- branch_condition(alldeaths, poisson)
#' match_log_lin <- branch_condition(log(alldeaths + 1), gaussian)
#' add_branch_condition(mv, match_poisson)
#' add_branch_condition(mv, match_log_lin)
#' multiverse_tree(mv)
#' # You can adjust colour scale of the edges
#' # using a ggraph::scale_edge_colour*() function.
#' multiverse_tree(mv) + ggraph::scale_edge_colour_viridis(
#'   discrete = TRUE,
#'   labels = c("Distribution", "Model", "Strength",
#'              "Femininity", "Outliers", "y")
#' )
#' # Display a multiverse tree for a subset of branches
#' # with name label for each option.
#' multiverse_tree(mv, branches = c("y", "distribution"), label = "name")
#' # with code label for each option.
#' multiverse_tree(mv, branches = c("y", "distribution"), label = "code")
#' # adjusting size and orientation of the labels
#' multiverse_tree(mv, branches = c("y", "distribution"),
#'   label = "name", label_size = 4, label_angle = 45)
#' }
#' @param .mverse A \code{mverse} object.
#' @param label Display the branch option name when "name" or the definition
#'   when "code". No label is displayed when "none" (default).
#' @param branches A character vector. Display a subset of branches
#'   when specified. Display all when NULL.
#' @param label_size A numeric. Set size of option labels.
#' @param label_angle A numeric. Rotate option labels.
#' @import igraph ggraph ggplot2
#' @return A \code{ggplot} object displaying the multiverse tree.
#' @name multiverse_tree
#' @export
multiverse_tree <- function(.mverse, label = "none",
                            branches = NULL, label_size = NULL,
                            label_angle = 0) {
  stopifnot(label %in% c("none", "code", "name"))
  # sort: conditioned -> conditioned on -> others
  brs <- unique(sapply(
    c(
      attr(.mverse, "branches_conditioned_list"),
      sapply(attr(.mverse, "branches_conditioned_list"), function(t) {
        t$conds_on
      }),
      attr(.mverse, "branches_list")
    ),
    function(s) {
      name(s)
    }
  ))
  if (length(brs) == 0) stop("No branch to display in a tree.")
  if (!is.null(branches)) {
    brs <- brs[sapply(brs, function(x) {
      x %in% branches
    })]
  }
  brs_name <- paste0(brs, ifelse(label == "code", "_branch_code", "_branch"))
  combs <- summary.mverse(.mverse, conf.int = FALSE)[brs_name] %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), .fns = as.character))
  edges_list <-
    list(data.frame(
      from = "Data",
      to = unique(combs[[1]]),
      branch = brs[1]
    ))
  v_labels <- c("Data")
  for (i in seq_len(length(brs_name))) {
    pairs <- combs[, 1:i] %>%
      dplyr::distinct_all() %>%
      tidyr::unite("to", tidyselect::all_of(1:i), remove = FALSE) %>%
      tidyr::unite("from", tidyselect::all_of(2:i), remove = FALSE) %>%
      dplyr::mutate(branch = brs[i])
    if (i > 1) {
      edges_list[[length(edges_list) + 1]] <- pairs %>%
        dplyr::select(tidyselect::all_of(c("from", "to", "branch"))) %>%
        dplyr::distinct_all()
    }
    v_labels <- c(v_labels, pairs %>% dplyr::pull(i + 2))
  }
  edges <- do.call(rbind, edges_list)
  g <- graph_from_data_frame(edges)
  plt <- ggraph(g, layout = "dendrogram", circular = FALSE) +
    geom_edge_link(aes(color = "branch")) +
    theme_void() +
    coord_flip() +
    scale_y_reverse(expand = c(0.1, 0.1)) +
    scale_x_continuous(expand = c(0.1, 0.1)) +
    labs(edge_colour = NULL) +
    theme(legend.position = "top")
  if (label %in% c("code", "name")) {
    plt <- plt +
      geom_node_text(
        aes(label = v_labels),
        hjust = 1,
        vjust = 1.2,
        size = ifelse(is.null(label_size), 7, label_size),
        angle = label_angle
      )
  }
  plt + geom_node_point(size = min(1, 100 / length(v_labels)))
}
