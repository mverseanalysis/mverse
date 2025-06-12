#' @noRd
#' @keywords internal
display_branch_opts <- function(mtable, .mverse) {
  branches <- c(
    attr(.mverse, "branches_list"),
    attr(.mverse, "branches_conditioned_list"),
    attr(.mverse, "covariate_branches_list")
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
  mtable <- multiverse::expand(object)
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
#'   y ~ MasFem,
#'   y ~ MasFem + hurricane_strength
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
#'   "estimates" ("e"), "df", "fstatistic" ("f"), "r.squared" ("r"), 
#'   and "aic" ("bic"). Default value is "estimates".
#' @param ... Ignored.
#' @rdname summary
#' @importFrom rlang .data
#' @export
summary.lm_mverse <- function(object, conf.int = TRUE, conf.level = 0.95,
                              output = "estimates", ...) {
  results <- multiverse::extract_variables(object, .model_mverse)
  if (output %in% c("estimates", "e")) {
    results[["out"]] <- lapply(
      results[[".model_mverse"]],
      function(x)  {
        out <- broom::tidy(x, conf.int = conf.int, cnf.level = conf.level)
        if (nrow(out) > 0) {
          return(out)
        }
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        return(out)
      }
    )
    mtable <- results %>% tidyr::unnest("out")
  } else if (output == "df") {
    mtable <- cbind(
      results, 
      lapply(
        results[[".model_mverse"]],
        function(x) as.data.frame(t(summary(x)$df))
      ) %>% 
        dplyr::bind_rows() %>%
        dplyr::rename(
          p = "V1",
          n.minus.p = "V2",
          p.star = "V3"
        )
    )
  } else if (output %in% c("r.squared", "r")) {
    mtable <- cbind(
      results, 
      lapply(
        results[[".model_mverse"]],
        function(x) data.frame(
          r.squared = summary(x)$r.squared,
          adj.r.squared = summary(x)$adj.r.squared
        )
      ) %>%
        dplyr::bind_rows()
    )
  } else if (output %in% c("fstatistic", "f")) {
    mtable <- cbind(
      results, 
      lapply(
        results[[".model_mverse"]],
        function(x) {
          out <- summary(x)$fstatistic 
          if (length(out) > 0) {
            return(as.data.frame(t(out)))
          }
          out <- data.frame(
            value = NA,
            numdf = NA,
            dendf = NA
          )
          return(out)
        }
      ) %>%
        dplyr::bind_rows()
    )
  } else if (output %in% c("aic", "bic")) {
    mtable <- results
    mtable["AIC"] <- sapply(
      results[[".model_mverse"]],
      stats::AIC
    )
    mtable["BIC"] <- sapply(
      results[[".model_mverse"]],
      stats::BIC
    )
  } else {
    stop("Invalid output argument.")
  }
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
#'   alldeaths ~ MasFem,
#'   alldeaths ~ MasFem + hurricane_strength
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
#'   Default value is "estimates".
#' @param ... Ignored.
#' @rdname summary
#' @importFrom rlang .data
#' @export
summary.glm_mverse <- function(object, conf.int = TRUE, conf.level = 0.95,
                               output = "estimates", ...) {
  results <- multiverse::extract_variables(object, .model_mverse)
  if (output %in% c("estimates", "e")) {
    results[["out"]] <- lapply(
      results[[".model_mverse"]],
      function(x)  {
        if (length(x$coefficients) > 0) {
          out <- broom::tidy(x, conf.int = conf.int, conf.level = conf.level)
          return(out)
        }
        out <- data.frame(
          term = "(None)",
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA
        )
        return(out)
      }
    )
    mtable <- results %>% tidyr::unnest("out")
  } else if (output == "df") {
    mtable <- cbind(
      results, 
      lapply(
        results[[".model_mverse"]],
        function(x) as.data.frame(t(summary(x)$df))
      ) %>% 
        dplyr::bind_rows() %>%
        dplyr::rename(
          rank = "V1",
          df.residual = "V2",
          n.coef = "V3"
        )
    )
  } else if (output %in% c("de", "deviance")) {
    mtable <- results
    mtable["deviance"] <- sapply(
      results[[".model_mverse"]],
      function(x) summary(x)$deviance
    )
    mtable["null.deviance"] <- sapply(
      results[[".model_mverse"]],
      function(x) summary(x)$null.deviance
    )
  } else if (output %in% c("aic", "bic")) {
    mtable <- results
    mtable["AIC"] <- sapply(
      results[[".model_mverse"]],
      stats::AIC
    )
    mtable["BIC"] <- sapply(
      results[[".model_mverse"]],
      stats::BIC
    )
  } else {
    stop("Invalid output argument.")
  }
  display_branch_opts(mtable, object)
}

#' @examples
#' \donttest{
#' # Displaying the multiverse table with \code{glm.nb} models fitted.
#' hurricane_outliers <- filter_branch(
#'   !Name %in% c("Katrina", "Audrey", "Andrew"),
#'   TRUE # include all
#' )
#' model_specifications <- formula_branch(alldeaths ~ MasFem)
#' mv <- create_multiverse(hurricane) %>%
#'   add_filter_branch(hurricane_outliers) %>%
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
  summary.glm_mverse(
    object, 
    conf.int = conf.int, 
    conf.level = conf.level, 
    output = output
  )
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
AIC.mverse <- function(object, ..., k = 2) {
  df <- summary(object, output = "aic")
  df$BIC <- NULL
  df
}

#' @rdname AIC
#' @export
BIC.mverse <- function(object, ...) {
  df <- summary(object, output = "bic")
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
#' @param label_hjust A numeric. Set the horizontal justification of the node
#'   labels.
#' @param label_vjust A numeric. Set the vertical justification of the node
#'   labels.
#' @import igraph ggraph ggplot2
#' @return A \code{ggplot} object displaying the multiverse tree.
#' @name multiverse_tree
#' @export
multiverse_tree <- function(.mverse,
                            label = "none",
                            branches = NULL,
                            label_size = NULL,
                            label_angle = 0,
                            label_hjust = 0,
                            label_vjust = 0) {
  stopifnot(label %in% c("none", "code", "name"))
  brs <- c(
    attr(.mverse, "branches_conditioned_list"),
    sapply(attr(.mverse, "branches_conditioned_list"), function(t) {
      t$conds_on
    }),
    attr(.mverse, "branches_list")
  )
  if (length(brs) == 0) stop("No branch to display in a tree.")
  br_names <- unique(unlist(sapply(
    brs,
    function(x) {
      if ("formula_branch" %in% class(x) && length(x$covariates) > 0) {
        c(name(x), paste0("covariate_", x$covariates))
      } else {
        name(x)
      }
    }
  )))
  if (!is.null(branches)) {
    br_names <- br_names[sapply(br_names, function(x) {
      x %in% branches
    })]
  }
  col_names <- paste0(
    br_names, ifelse(label == "code", "_branch_code", "_branch")
  )
  combs <- summary.mverse(.mverse, conf.int = FALSE)[col_names] %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::everything(),
        .fns = as.character
      )
    )
  edges_list <-
    list(data.frame(
      from = "Data",
      to = unique(combs[[1]]),
      branch = br_names[1]
    ))
  v_labels <- c("Data")
  for (i in seq_len(length(col_names))) {
    pairs <- combs[, 1:i] %>%
      dplyr::distinct_all() %>%
      tidyr::unite("to", tidyselect::all_of(1:i), remove = FALSE) %>%
      tidyr::unite("from", tidyselect::all_of(2:i), remove = FALSE) %>%
      dplyr::mutate(branch = br_names[i])
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
    geom_edge_link(aes(color = branch)) +
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
        hjust = label_hjust,
        vjust = label_vjust,
        size = ifelse(is.null(label_size), 7, label_size),
        angle = label_angle
      )
  }
  plt + geom_node_point(size = min(1, 100 / length(v_labels)))
}
