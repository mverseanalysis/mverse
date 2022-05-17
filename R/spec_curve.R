#' Display a specification curve across the multiverse.
#'
#' \code{spec_curve} returns the specification curve  as
#' proposed by Simonsohn, Simmons, and Nelson
#' (2020) <doi:10.1038/s41562-020-0912-z>.
#' \code{spec_curve} are available for \code{mverse} objects fitted with
#' \code{lm_mverse()}, \code{glm_mverse()}, and \code{glm.nb_mverse()}.
#' Notice that the order of universes may not correspond to the order
#' in the summary table.
#'
#' @param .object a \code{glm.nb_mverse} object.
#' @param var name for the variable to show.
#' @param conf.int when \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level the confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param option a vector of branches to show the options included.
#' @param universe_order when \code{TRUE}, order the universes according to
#'   the order in the summary table.
#' @param color_order when \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @param branch_order name for the branch to order.
#' @param point_size size of points on the top plot.
#' @param grid_size size of points on the bottom plot.
#' @param point_alpha alpha level of points and point ranges.
#' @param brewer_palette name of colorbrewer palette for the plot.
#' @param yaxis_text_size text size of y-axis label
#' @param ... ignored.
#' @return a specification curve plot for the estimates
#' @source
#' Uri Simonsohn, Joseph P. Simmons, and Leif D. Nelson. (2020).
#' “Specification curve analysis” \emph{Nature Human Behaviour},
#' 4, 1208–14. \doi{10.1038/s41562-020-0912-z}
#' @name spec_curve
#' @export
spec_curve <- function(.object, var, ...) {
  UseMethod("spec_curve")
}

#' @rdname spec_curve
#' @examples
#' \dontrun{
#'
#' # Display a specification curve for \code{lm} models
#' # fitted across the multiverse.
#' femininity <- mutate_branch(
#'   MasFem > 6, MasFem > mean(MasFem)
#' )
#' model <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + HighestWindSpeed
#' )
#' mv <- mverse(hurricane) %>%
#'   add_mutate_branch(femininity) %>%
#'   add_formula_branch(model) %>%
#'   lm_mverse()
#' spec_curve(mv, var = "femininityTRUE")
#' }
#' @importFrom rlang .data
#' @export
spec_curve.lm_mverse <- function(.object, var , conf.int = TRUE,
                              conf.level = 0.95,
                              option = names(multiverse::parameters(.object)),
                              universe_order = FALSE, color_order = FALSE,
                              color = "p.value < 0.05", branch_order = NULL,
                              point_size = .25, grid_size = 2,
                              point_alpha = 1, brewer_palette = "Set2",
                              yaxis_text_size = 8, ...) {
  stopifnot(inherits(.object, "mverse"))
  spec_curve_table <- get_spec_curve_table(
    .object, var, !!rlang::enexpr(conf.int),!!rlang::enexpr(conf.level),
    !!rlang::enexpr(branch_order), universe_order, color_order,
    !!rlang::enexpr(color)
  )

  p1 <- plot_spec_curve_curve(
    spec_curve_table, var, !!rlang::enexpr(conf.int), !!rlang::enexpr(color),
    option, point_size, point_alpha, brewer_palette, yaxis_text_size
  )
  p2 <- plot_spec_curve_grid(
    spec_curve_table, names(multiverse::parameters(.object)), option,
    !!rlang::enexpr(color), grid_size, brewer_palette, yaxis_text_size
  )
  if (universe_order) {
    p2 <- p2 + xlab("universe number")
  }
  cowplot::plot_grid(
    p1, p2, axis = "bltr", align = "v", ncol = 1, rel_heights = c(1, 2)
  )
}

#' @rdname spec_curve
#' @examples
#' \dontrun{
#'
#' # Display a specification curve for \code{glm} models
#' # fitted across the multiverse.
#' femininity <- mutate_branch(
#'   MasFem > 6, MasFem > mean(MasFem)
#' )
#' model <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + HighestWindSpeed
#' )
#' fam <- family_branch(gaussian)
#' mv <- mverse(hurricane) %>%
#'   add_mutate_branch(femininity) %>%
#'   add_formula_branch(model) %>%
#'   add_family_branch(fam) %>%
#'   glm_mverse()
#' spec_curve(mv, var = "femininityTRUE")
#' }
#' @importFrom rlang .data
#' @export
spec_curve.glm_mverse <- function(.object, var , conf.int = TRUE,
                                 conf.level = 0.95,
                                 option = names(multiverse::parameters(.object)),
                                 universe_order = FALSE, color_order = FALSE,
                                 color = "p.value < 0.05", branch_order = NULL,
                                 point_size = .25, grid_size = 2,
                                 point_alpha = 1, brewer_palette = "Set2",
                                 yaxis_text_size = 8, ...) {
  stopifnot(inherits(.object, "mverse"))
  spec_curve_table <- get_spec_curve_table(
    .object, var, !!rlang::enexpr(conf.int),!!rlang::enexpr(conf.level),
    !!rlang::enexpr(branch_order), universe_order, color_order,
    !!rlang::enexpr(color)
  )

  p1 <- plot_spec_curve_curve(
    spec_curve_table, var, !!rlang::enexpr(conf.int), !!rlang::enexpr(color),
    option, point_size, point_alpha, brewer_palette, yaxis_text_size
  )
  p2 <- plot_spec_curve_grid(
    spec_curve_table, names(multiverse::parameters(.object)), option,
    !!rlang::enexpr(color), grid_size, brewer_palette, yaxis_text_size
  )
  if (universe_order) {
    p2 <- p2 + xlab("universe number")
  }
  cowplot::plot_grid(
    p1, p2, axis = "bltr", align = "v", ncol = 1, rel_heights = c(1, 2)
  )
}

#' @examples
#' \dontrun{
#'
#' # Display a specification curve for \code{glm.nb} models
#' # fitted across the multiverse.
#' femininity <- mutate_branch(
#'   MasFem > 6, MasFem > mean(MasFem)
#' )
#' model <- formula_branch(
#'   alldeaths ~ femininity,
#'   alldeaths ~ femininity + HighestWindSpeed
#' )
#' mv <- mverse(hurricane) %>%
#'   add_mutate_branch(femininity) %>%
#'   add_formula_branch(model) %>%
#'   glm.nb_mverse()
#' spec_curve(mv, var = "femininityTRUE")
#' }
#' @rdname spec_curve
#' @importFrom rlang .data
#' @export
spec_curve.glm.nb_mverse <- function(.object, var , conf.int = TRUE,
                                 conf.level = 0.95,
                                 option = names(multiverse::parameters(.object)),
                                 universe_order = FALSE, color_order = FALSE,
                                 color = "p.value < 0.05", branch_order = NULL,
                                 point_size = .25, grid_size = 2,
                                 point_alpha = 1, brewer_palette = "Set2",
                                 yaxis_text_size = 8, ...) {
  stopifnot(inherits(.object, "mverse"))
  spec_curve_table <- get_spec_curve_table(
    .object, var, !!rlang::enexpr(conf.int),!!rlang::enexpr(conf.level),
    !!rlang::enexpr(branch_order), universe_order, color_order,
    !!rlang::enexpr(color)
  )

  p1 <- plot_spec_curve_curve(
    spec_curve_table, var, !!rlang::enexpr(conf.int), !!rlang::enexpr(color),
    option, point_size, point_alpha, brewer_palette, yaxis_text_size
  )
  p2 <- plot_spec_curve_grid(
    spec_curve_table, names(multiverse::parameters(.object)), option,
    !!rlang::enexpr(color), grid_size, brewer_palette, yaxis_text_size
  )
  if (universe_order) {
    p2 <- p2 + xlab("universe number")
  }
  cowplot::plot_grid(
    p1, p2, axis = "bltr", align = "v", ncol = 1, rel_heights = c(1, 2)
  )
}

#' @importFrom rlang .data
get_spec_curve_table <- function(.object, var, conf.int, conf.level,
                                 branch_order, universe_order,
                                 color_order, color) {
  branch_order <- rlang::enquo(branch_order)
  spec_curve_table <- summary(
    .object,
    conf.int = !!rlang::enexpr(conf.int),
    conf.level = !!rlang::enexpr(conf.level)
  ) %>%
    dplyr::filter(.data$term == var) %>%
    dplyr::arrange(.data$universe)
  if (!universe_order) {
    if (color_order) {
      if (rlang::quo_is_null(branch_order)) {
        spec_curve_table <- dplyr::arrange(
          spec_curve_table, {{ color }}, .data$estimate
        )
      } else {
        spec_curve_table <- dplyr::arrange(
          spec_curve_table, !! branch_order, {{ color }}, .data$estimate
        )
      }
    } else {
      if (rlang::quo_is_null(branch_order)) {
        spec_curve_table <- dplyr::arrange(spec_curve_table, .data$estimate)
      } else {
        spec_curve_table <- dplyr::arrange(
          spec_curve_table, !! branch_order, .data$estimate
        )
      }
    }
  }
  dplyr::mutate(spec_curve_table, x = seq_len(nrow(spec_curve_table)))
}

#' @import ggplot2
#' @importFrom rlang .data
plot_spec_curve_curve <- function(spec_curve_table, var, conf.int, color,
                                  option, point_size, point_alpha,
                                  brewer_palette, yaxis_text_size) {
  plt <- ggplot(
    spec_curve_table,
    aes(.data$x, .data$estimate, color = {{ color }})) +
    geom_point(size = point_size) +
    labs(x = NULL, y = paste0("coefficient of \n:", var)) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = yaxis_text_size),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    scale_colour_brewer(palette = brewer_palette)
  if (conf.int) {
    return(
      plt + geom_pointrange(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        alpha = point_alpha, size = point_size
      )
    )
  }
  return(plt)
}

#' @import ggplot2
#' @importFrom rlang .data
plot_spec_curve_grid <- function(spec_curve_table, parameters, option, color,
                                 grid_size, brewer_palette, yaxis_text_size) {
  spec_curve_table %>%
    tidyr::pivot_longer(
      parameters,
      names_to = "parameter_name",
      values_to = "parameter_option"
    ) %>%
    dplyr::filter(.data$parameter_name %in% option) %>%
    ggplot() +
    geom_point(
      aes(x = .data$x, y = .data$parameter_option, color = {{ color }}),
      size = grid_size, shape = 124
    ) +
    labs(x = NULL, y = "Branch Options") +
    facet_grid(parameter_name ~ ., space = "free_y", scales = "free_y") +
    scale_colour_brewer(palette = brewer_palette) +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.y = element_text(size = yaxis_text_size),
      strip.placement = "outside",
      strip.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(size = rel(0.5)),
      panel.spacing.x = unit(0.15, "cm"),
      panel.spacing.y = unit(1.25, "lines"),
      strip.text.y = element_text(angle = 0, face = "bold", size = 8),
      legend.position = "none"
    )
}
