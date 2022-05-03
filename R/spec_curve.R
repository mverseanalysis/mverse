
#' Display a specification curve across the multiverse.
#' @rdname spec_curve
#' @export
spec_curve <- function(.object, var, conf.int, conf.level,
                       option, universe_order, color_order,
                       color, branch_order) {
  UseMethod("spec_curve")
}

#' \code{spec_curve.lm_mverse} returns the specification curve of \code{lm}
#' regression results across the multiverse.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .object a \code{lm_mverse} object.
#' @param var name for the variable to show.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param option a vector of branches to show the options included.
#' @param universe_order When \code{TRUE}, order the universes according to
#'   the order in the summary table.
#' @param color_order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @param branch_order name for the branch to order.
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'   MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1
#' )
#' add_mutate_branch(mv, femininity)
#' # Define and add a formula branch
#' model <- formula_branch(
#'   alldeaths ~ femininity, log(alldeaths + 1) ~ femininity
#' )
#' add_formula_branch(mv, model)
#' # Fit a lm model
#' lm_mverse(mv)
#' # Display the specification curve
#' spec_curve(mv, var = "femininityTRUE")
#'
#' @return a specification curve plot for the estimates
#' @name spec_curve
#' @family {spec_curve method}
#' @import ggplot2
#' @importFrom rlang .data
#' @export
spec_curve.lm_mverse <- function(
    .object, var = NULL, conf.int = TRUE, conf.level = 0.95,
    option = names(multiverse::parameters(.object)),
    universe_order = FALSE, color_order = FALSE,
    color = "p.value < 0.05", branch_order = NULL) {
  stopifnot(inherits(.object, "lm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  branch_order <- rlang::enquo(branch_order)
  data.spec_curve <- summary.lm_mverse(
    .object,
    conf.int = !!rlang::enexpr(conf.int),
    conf.level = !!rlang::enexpr(conf.level)
  ) %>%
    dplyr::filter(.data$term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      dplyr::arrange(.data$estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, {{ color }}, .data$estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, .data$estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange({{ color }}, .data$estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    dplyr::mutate(.universe = seq_len(nrow(data.spec_curve))) %>%
    dplyr::select(-.data$term)

  p1 <- data.spec_curve %>%
    ggplot(ggplot2::aes(.data$.universe, .data$estimate, color = {{ color }})) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:", var))

  if (conf.int) {
    p1 <- p1 +
      geom_pointrange(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        alpha = 0.2, size = 0.25
      )
  }
  p1 <- p1 +
    theme_minimal() +
    scale_colour_brewer(palette = "Set1")

  data.info <- data.spec_curve %>%
    tidyr::pivot_longer(!!names(multiverse::parameters(.object)),
      names_to = "parameter_name",
      values_to = "parameter_option"
    ) %>%
    dplyr::filter(.data$parameter_name %in% option)

  p2 <- data.info %>%
    ggplot() +
    geom_point(
      aes(
        x = .data$.universe, y = .data$parameter_option,
        color = {{ color }}
      ),
      size = 2, shape = 124
    )

  if (!universe_order) {
    p2 <- p2 +
      xlab("") +
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      )
    p1 <- p1 +
      theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      )
  } else {
    p2 <- p2 + xlab("universe #")
  }

  p2 <- p2 + ylab("option included in the analysis specification") +
    facet_grid(parameter_name ~ ., space = "free_y", scales = "free_y") +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(size = rel(0.5)),
      panel.spacing.x = unit(0.15, "cm"),
      panel.spacing.y = unit(1.25, "lines"),
      strip.text.y = element_text(angle = 0, face = "bold", size = 8),
      legend.position = "none"
    ) +
    scale_colour_brewer(palette = "Set1")

  cowplot::plot_grid(
    p1, p2,
    axis = "bltr",
    align = "v", ncol = 1, rel_heights = c(1, 2)
  )
}

#' Display a specification curve across the multiverse.
#'
#' \code{spec_curve.glm_mverse} returns the specification curve of \code{glm}
#' regression results across the multiverse.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .object a \code{glm_mverse} object.
#' @param var name for the variable to show.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param option a vector of branches to show the options included.
#' @param universe_order When \code{TRUE}, order the universes according to
#'   the order in the summary table.
#' @param color_order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @param branch_order name for the branch to order.
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'   MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1
#' )
#' add_mutate_branch(mv, femininity)
#' # Define and add a formula branch
#' model <- formula_branch(
#'   alldeaths ~ femininity, alldeaths ~ femininity * HighestWindSpeed
#' )
#' add_formula_branch(mv, model)
#' # Define and add a family branch
#' model_family <- family_branch(gaussian, poisson)
#' add_family_branch(mv, model_family)
#' # Fit a glm model
#' glm_mverse(mv)
#' # Display the specification curve
#' spec_curve(mv, var = "femininityTRUE")
#'
#' @return a specification curve plot for the estimates
#' @import ggplot2
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.glm_mverse <- function(
    .object, var = NULL, conf.int = TRUE, conf.level = 0.95,
    option = names(multiverse::parameters(.object)),
    universe_order = FALSE, color_order = FALSE, color = "p.value < 0.05",
    branch_order = NULL) {
  stopifnot(inherits(.object, "glm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  branch_order <- rlang::enquo(branch_order)
  data.spec_curve <- summary(
    .object,
    conf.int = !!rlang::enexpr(conf.int),
    conf.level = !!rlang::enexpr(conf.level)
  ) %>%
    dplyr::filter(.data$term == var)

  if (!universe_order) {
    data.spec_curve <- data.spec_curve %>%
      dplyr::arrange(.data$estimate)
    if (color_order & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, {{ color }}, .data$estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, .data$estimate)
    } else if (color_order) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange({{ color }}, .data$estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    dplyr::mutate(.universe = seq_len(nrow(data.spec_curve))) %>%
    dplyr::select(-.data$term)

  p1 <- data.spec_curve %>%
    ggplot(aes(.data$.universe, .data$estimate, color = {{ color }})) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:", var))

  if (conf.int) {
    p1 <- p1 + geom_pointrange(
      aes(ymin = .data$conf.low, ymax = .data$conf.high),
      alpha = 0.2, size = 0.25
    )
  }
  p1 <- p1 +
    theme_minimal() +
    scale_colour_brewer(palette = "Set1")

  data.info <- data.spec_curve %>%
    tidyr::pivot_longer(
      !!names(multiverse::parameters(.object)),
      names_to = "parameter_name",
      values_to = "parameter_option"
    ) %>%
    dplyr::filter(.data$parameter_name %in% option)

  p2 <- data.info %>%
    ggplot() +
    geom_point(
      aes(x = .data$.universe, y = .data$parameter_option, color = {{ color }}),
      size = 2, shape = 124
    )

  if (!universe_order) {
    p2 <- p2 +
      xlab("") +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    p1 <- p1 +
      theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  } else {
    p2 <- p2 + xlab("universe #")
  }

  p2 <- p2 + ylab("option included in the analysis specification") +
    facet_grid(parameter_name ~ ., space = "free_y", scales = "free_y") +
    theme(
      strip.placement = "outside",
      strip.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(size = rel(0.5)),
      panel.spacing.x = unit(0.15, "cm"),
      panel.spacing.y = unit(1.25, "lines"),
      strip.text.y = element_text(angle = 0, face = "bold", size = 8),
      legend.position = "none"
    ) +
    scale_colour_brewer(palette = "Set1")

  cowplot::plot_grid(
    p1, p2,
    axis = "bltr", align = "v", ncol = 1, rel_heights = c(1, 2)
  )
}
