#' Create a specification table for a selected variable.
#'
#' Returns estimates for a selected variable across the multiverse along with
#' the universe specification information in a table. The resulting table
#' can be used for \code{spe_curve()}.
#'
#' @param .mverse A \code{mverse} object.
#' @param var A character specifying the variable of interest.
#' @param conf.int Whether the table should include confidence intervals.
#' @param conf.level The confidence level for the confidence level and \code{is_significant}.
#' @examples
#' femininity <- mutate_branch(
#'   1 * (MasFem > 6), 1 * (MasFem > mean(MasFem))
#' )
#' intensity <- mutate_branch(
#'   Minpressure_Updated_2014,
#'   Category,
#'   NDAM,
#'   HighestWindSpeed
#' )
#' model <- formula_branch(
#'   log(alldeaths + 1) ~ femininity,
#'   log(alldeaths + 1) ~ femininity * intensity
#' )
#' mv <- mverse(hurricane) %>%
#'   add_mutate_branch(femininity) %>%
#'   add_mutate_branch(intensity) %>%
#'   add_formula_branch(model) %>%
#'   lm_mverse()
#' spec_summary(mv, "femininity")
#' @return A \code{spec_summary} object that includes estimates and specification
#'   acorss the multiverse for the selected term(s). A boolean column \code{is_significant}
#'   indicates whether \code{p.value} for the universe is less than the specified
#'   significance level (\code{1 - conf.level}).
#' @name spec_summary
#' @importFrom rlang .data
#' @family specification curve analysis
#' @export
spec_summary <- function(.mverse, var, conf.int = TRUE, conf.level = .95) {
  UseMethod("spec_summary")
}

#' @export
spec_summary.mverse <- function(.mverse, var, conf.int = TRUE, conf.level = .95){
  .spec_summary <- summary(.mverse,
          conf.int = !!rlang::enexpr(conf.int),
          conf.level = !!rlang::enexpr(conf.level)) %>%
    dplyr::filter(.data$term == var) %>%
    dplyr::select(
      tidyselect::any_of(c(
        "universe", "term", "estimate", "p.value", "conf.low", "conf.high"
      )),
      tidyselect::contains("_branch")
    ) %>%
    dplyr::mutate(is_significant = .data$p.value < (1 - conf.level)) 
  attr(.spec_summary, "var") <- var
  attr(.spec_summary, "conf.int") <- conf.int
  attr(.spec_summary, "conf.level") <- conf.level
  attr(.spec_summary, "class") <- c("spec_summary", class(.spec_summary))
  .spec_summary
}

#' @export
print.spec_summary <- function(x, ...) {
  cat("Specification table for variable:", attr(x, "var"))
  if (attr(x, "conf.int"))
    cat(" at confidence intervals", attr(x, "conf.level"), "\n")
  else
    cat("\n")
  attr(x, "class") <- class(x)[class(x) != "spec_summary"]
  print(x)
}

#' Display a specification curve across the multiverse.
#'
#' Returns a \code{ggplot} object that displays
#' the specification curve  as proposed by \insertCite{speccurvessn}{mverse}.
#' Note that the order of universes may not correspond to the order
#' in the summary table.
#'
#' @param .spec_summary A specification table created using \code{spec_summary()}.
#' @param label If "name", uses the branch option names. If "code", display
#'   the codes used to define the branch options.
#' @param order_by A character vector by which the curve is sorted.
#' @param colour_by The name of the variable to colour the curve. 
#' @param palette_common A character vector of colours to match the values of
#'   the varible \code{colour_by} in the specification curve and the specification 
#'   matrix. The palette must contain more colours than the number of unique values
#'   of \code{colour_by} variable.
#' @param pointsize Size of the points in the specification curve and 
#'   the specification matrix.
#' @param linewidth Width of confidence interval lines.
#' @param spec_matrix_spacing A numeric for adjusting the specification matrix
#'   spacing passed to \code{combmatrix.label.extra_spacing} 
#'   in \code{ggupset::theme_combmatrix()}.
#' @param theme_common A \code{ggplot} theme to be used for both the specification
#'   curve and the specification matrix.
#' @param sep A string used internally to create the spcification matrix. The string
#'   must be distinct from all branch names, option names, and option codes. Use a
#'   different value if any of them contains the default value.
#' @importFrom Rdpack reprompt
#' @return a \code{ggplot} object with the specification curve plot for
#'   the estimates passed in the \code{spec_summary()}.
#' @examples
#' femininity <- mutate_branch(
#'   1 * (MasFem > 6), 1 * (MasFem > mean(MasFem))
#' )
#' y <- mutate_branch(log(alldeaths + 1), alldeaths)
#' intensity <- mutate_branch(
#'   Minpressure_Updated_2014,
#'   Category,
#'   NDAM,
#'   HighestWindSpeed
#' )
#' model <- formula_branch(
#'   y ~ femininity,
#'   y ~ femininity * intensity
#' )
#' family <- family_branch(
#'   gaussian, poisson
#' )
#' match_poisson <- branch_condition(alldeaths, poisson)
#' match_gaussian <- branch_condition(log(alldeaths + 1), gaussian)
#' stable <- mverse(hurricane) %>%
#'   add_mutate_branch(y, femininity, intensity) %>%
#'   add_formula_branch(model) %>%
#'   add_family_branch(family) %>%
#'   add_branch_condition(match_poisson, match_gaussian) %>%
#'   glm_mverse() %>%
#'   spec_summary("femininity")
#' # default behaviour
#' spec_curve(stable)
#' # coloring and sorting based on other variable
#' stable %>%
#'   dplyr::mutate(colour_by = y_branch) %>%
#'   spec_curve(order_by = c("estimate", "colour_by"), colour_by = "colour_by")
#' # Because the output is a \code{ggplot} object, you can
#' # further modify the asethetics of the specification curve 
#' # using \code{ggplot2::theme()} and the specication matrix
#' # using \code{ggupset::theme_combmatrix()}
#' spec_curve(stable) +
#'   ggplot2::labs(y = "Estimates", colour = "Significant at 0.05 level",
#'                 title = "Specification curve of femininity") +
#'   ggplot2::theme(legend.position = "bottom") +
#'   ggupset::theme_combmatrix(
#'     combmatrix.label.width = ggplot2::unit(c(25, 100, 0, 0), "pt")
#'   )
#' @references
#' \insertAllCited{}
#' @name spec_curve
#' @family specification curve analysis
#' @export
spec_curve <- function(
    .spec_summary, label = "name",
    order_by = c("estimate", "is_significant"),
    colour_by = "is_significant", palette_common = NULL,
    pointsize = 2, linewidth = .5, spec_matrix_spacing = 10,
    theme_common = ggplot2::theme_minimal(),
    sep = "---"
) {
  UseMethod("spec_curve")
}

#' @import ggplot2
#' @export
spec_curve.spec_summary <- function(
    .spec_summary, label = "name",
    order_by = c("estimate", "is_significant"),
    colour_by = "is_significant", palette_common = NULL,
    pointsize = 2, linewidth = .5, spec_matrix_spacing = 10,
    theme_common = ggplot2::theme_minimal(),
    sep = "---"
) {
  stopifnot(label %in% c("name", "code"))
  branch_end <- ifelse(label == "name", "_branch", "_branch_code")
  n_colours <- length(unique(.spec_summary[[colour_by]]))
  sep_internal <- "::::"
  tmp <- .spec_summary %>%
    tidyr::pivot_longer(tidyselect::ends_with(branch_end)) %>%
    dplyr::mutate(
      spec = paste0(
        .data$name, sep_internal, .data$value, sep_internal, .data[[colour_by]]
      )
    ) %>%
    dplyr::select(-dplyr::all_of(c("name", "value"))) %>%
    dplyr::group_by(dplyr::across(-tidyselect::any_of("spec"))) %>%
    dplyr::summarise(
      spec = paste0(.data$spec, collapse = sep),
      .groups = "drop"
    )
  if (is.factor(tmp[[colour_by]])) {
    tmp[[colour_by]] <- factor(
      tmp[[colour_by]], sort(levels(tmp[[colour_by]])))
  }
  for (ord in order_by) {
    tmp <- tmp %>% dplyr::arrange(.data[[ord]])
  }
  plt <- tmp %>%
    dplyr::mutate(spec = stats::reorder(.data$spec, dplyr::row_number())) %>%
    ggspec_curve(
      theme_common, colour_by, palette_common, n_colours,
      pointsize, linewidth, spec_matrix_spacing, branch_end, sep, sep_internal
    )

  if (attr(.spec_summary, "conf.int")) {
    plt <- plt +
      geom_segment(
        aes(y = .data$conf.low, yend = .data$conf.high, xend = .data$spec),
        linewidth = linewidth, show.legend = FALSE
      )
  }
  if (!is.null(palette_common)) {
    plt <- plt +
      scale_colour_manual(values = palette_common[1:n_colours])
  }
  plt
}

#' @import ggplot2
#' @import ggupset
ggspec_curve <- function(specs, theme_common,
                         colour_by, palette_common, n_colours,
                         pointsize, linewidth, spec_matrix_spacing,
                         branch_end, sep, sep_internal) {
  ggplot(
    specs,
    aes(x = .data$spec, y = .data$estimate, colour = .data[[colour_by]])
    ) +
    theme_common + xlab(NULL) +
    geom_point(size = pointsize) +
    axis_combmatrix(
      sep = sep,
      override_plotting_function = function(df) {
        df <- df[df$observed, ]
        labs_cols <- strsplit(as.character(df$single_label), sep_internal)
        brs <- sapply(labs_cols, "[[", 1)
        opts <- sapply(labs_cols, "[[", 2)
        cols <- factor(sapply(labs_cols, "[[", 3))
        df$single_label <- factor(
          opts,
          levels = rev(unlist(
            sapply(
              unique(brs), function(x) c(paste(x, 1:2), x, rev(unique(opts[brs == x])))
            )
          )),
          ordered = FALSE
        )
        tbl <- ggplot(df, aes(x = .data$at, y = .data$single_label, colour = cols)) +
          labs(x = NULL, y = NULL) +
          scale_x_continuous(
            limits = c(0, 1), expand = c(0, 0),
            breaks = unique(df$at)
          ) +
          scale_y_discrete(breaks = opts, drop = FALSE) +
          geom_point(size = pointsize, show.legend = FALSE) +
          annotate(
            "text", x = 0, hjust = 1, vjust = 0, y = unique(brs),
            label = sub(branch_end, "", unique(brs))
          ) +
          coord_cartesian(clip = "off") +
          theme_common +
          theme(
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(hjust = 0)
          )
        if (!is.null(palette_common)) {
          tbl <- tbl +
            scale_colour_manual(values = palette_common[1:n_colours])
        }
        tbl
      }
    ) +
    theme_combmatrix(
      combmatrix.label.extra_spacing = spec_matrix_spacing
    )
}
