
#' Display a specification curve across the multiverse.
#' @rdname spec_curve
#' @export
spec_curve <- function(
  .object, var, conf.int, conf.level,
  option, universe_order, color_order,
  color, branch_order, yaxis_text_size) {
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
#' @param yaxis_text_size text size of y-axis label
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'  MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1)
#' add_mutate_branch(mv, femininity)
#' # Define and add a formula branch
#' model <- formula_branch(
#'  alldeaths ~ femininity, log(alldeaths + 1) ~ femininity)
#' add_formula_branch(mv, model)
#' # Fit a lm model
#' lm_mverse(mv)
#' # Display the specification curve
#' spec_curve(mv, var = "femininityTRUE")
#'
#' @return a specification curve plot for the estimates
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.lm_mverse <- function(
  .object, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.object)),
  universe_order = FALSE,
  color_order = FALSE, color = (p.value < 0.05),
  branch_order = NULL, yaxis_text_size = 8) {
  stopifnot(inherits(.object, "lm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  conf.int <<- conf.int
  conf.level <<- conf.level
  color_order <<- color_order
  universe_order <<- universe_order
  color <- rlang::enquo(color)
  branch_order <- rlang::enquo(branch_order)
  mtable <- summary(.object,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    dplyr::filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      dplyr::arrange(estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, !!color, estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!color, estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    dplyr::mutate(.universe = 1:nrow(.)) %>%
    dplyr::select(-term)

  p1 <- data.spec_curve %>%
    ggplot2::ggplot(ggplot2::aes(.universe, estimate, color = !!color)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + ggplot2::geom_pointrange(ggplot2::aes(ymin = conf.low,
                                   ymax = conf.high),
                               alpha = 0.2,size=0.25)}
  p1 <- p1 +
    ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))
    ggplot2::scale_colour_brewer(palette = "Set1")

  data.info <- data.spec_curve %>%
    tidyr::pivot_longer( !! names(multiverse::parameters(.object)),
      names_to = "parameter_name",
      values_to = "parameter_option" ) %>%
    dplyr::filter(parameter_name %in% option)

  p2 <- data.info %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .universe, y = parameter_option,
                   color = !!color), size = 2,shape = 124)

  if (universe_order == FALSE) {
    p2 <- p2 + ggplot2::xlab("") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())
    p1 <- p1 + ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                              axis.text.x = ggplot2::element_blank())
  } else {
    p2 <- p2 + ggplot2::xlab("universe #")
  }

  p2 <- p2 + ggplot2::ylab("Branch Options") +
    ggplot2::facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
    ggplot2::theme(strip.placement = "outside",
          strip.background = ggplot2::element_rect(fill=NA,colour=NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.grid = ggplot2::element_line(colour = "grey92"),
          panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
          panel.spacing.x=ggplot2::unit(0.15,"cm"),
          panel.spacing.y=ggplot2::unit(1.25, "lines"),
          strip.text.y = ggplot2::element_text(angle = 0, face="bold", size=8),
          legend.position = "none") +
    ggplot2::scale_colour_brewer(palette = "Set1") + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
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
#' @param yaxis_text_size text size of y-axis label
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'  MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1)
#' add_mutate_branch(mv, femininity)
#' # Define and add a formula branch
#' model <- formula_branch(
#'  alldeaths ~ femininity, alldeaths ~ femininity * HighestWindSpeed)
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
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.glm_mverse <- function(
  .object, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.object)),
  universe_order = FALSE,
  color_order = FALSE, color = (p.value < 0.05),
  branch_order = NULL, yaxis_text_size = 8) {
  stopifnot(inherits(.object, "glm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  color <- rlang::enquo(color)
  conf.int <<- conf.int
  conf.level <<- conf.level
  branch_order <- rlang::enquo(branch_order)
  mtable <- summary(.object,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    dplyr::filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      dplyr::arrange(estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, !!color, estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!color, estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    dplyr::mutate(.universe = 1:nrow(.)) %>%
    dplyr::select(-term)

  p1 <- data.spec_curve %>%
    ggplot2::ggplot(ggplot2::aes(.universe, estimate, color = !!color)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + ggplot2::geom_pointrange(ggplot2::aes(ymin = conf.low,
                                            ymax = conf.high),
                                        alpha = 0.2,size=0.25)}
  p1 <- p1 +
    ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))
    ggplot2::scale_colour_brewer(palette = "Set1")

  data.info <- data.spec_curve %>%
    tidyr::pivot_longer( !! names(multiverse::parameters(.object)),
                         names_to = "parameter_name",
                         values_to = "parameter_option" ) %>%
    dplyr::filter(parameter_name %in% option)

  p2 <- data.info %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .universe, y = parameter_option,
                            color = !!color), size = 2,shape = 124)

  if (universe_order == FALSE) {
    p2 <- p2 + ggplot2::xlab("") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())
    p1 <- p1 + ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                              axis.text.x = ggplot2::element_blank())
  } else {
    p2 <- p2 + ggplot2::xlab("universe #")
  }

  p2 <- p2 + ggplot2::ylab("Branch Options") +
    ggplot2::facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
    ggplot2::theme(strip.placement = "outside",
                   strip.background = ggplot2::element_rect(fill=NA,colour=NA),
                   panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.grid = ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
                   panel.spacing.x=ggplot2::unit(0.15,"cm"),
                   panel.spacing.y=ggplot2::unit(1.25, "lines"),
                   strip.text.y = ggplot2::element_text(angle = 0, face="bold", size=8),
                   legend.position = "none") +
    ggplot2::scale_colour_brewer(palette = "Set1") + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}


#' Display a specification curve across the multiverse.
#'
#' \code{spec_curve.glm.nb_mverse} returns the specification curve of \code{glm.nb}
#' regression results across the multiverse.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .object a \code{glm.nb_mverse} object.
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
#' @param yaxis_text_size text size of y-axis label
#'
#' @examples
#' # Create a mverse object
#' mv <- mverse(hurricane)
#' # Define and add a mutate branch
#' femininity <- mutate_branch(
#'  MasFem > 6, MasFem > mean(MasFem), Gender_MF == 1)
#' add_mutate_branch(mv, femininity)
#' # Define and add a formula branch
#' model <- formula_branch(
#'  alldeaths ~ femininity, alldeaths ~ femininity * HighestWindSpeed)
#' add_formula_branch(mv, model)
#' # Define and add a family branch
#' # Fit a glm.nb model
#' glm_mverse(mv)
#' # Display the specification curve
#' spec_curve(mv, var = "femininityTRUE")
#'
#' @return a specification curve plot for the estimates
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.glm.nb_mverse <- function(
    .object, var = NULL, conf.int = TRUE,
    conf.level = 0.95,
    option = names(multiverse::parameters(.object)),
    universe_order = FALSE,
    color_order = FALSE, color = (p.value < 0.05),
    branch_order = NULL, yaxis_text_size = 8) {
  stopifnot(inherits(.object, "glm.nb_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  color <- rlang::enquo(color)
  conf.int <<- conf.int
  conf.level <<- conf.level
  branch_order <- rlang::enquo(branch_order)
  mtable <- summary(.object,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    dplyr::filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      dplyr::arrange(estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, !!color, estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!branch_order, estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        dplyr::arrange(!!color, estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    dplyr::mutate(.universe = 1:nrow(.)) %>%
    dplyr::select(-term)

  p1 <- data.spec_curve %>%
    ggplot2::ggplot(ggplot2::aes(.universe, estimate, color = !!color)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + ggplot2::geom_pointrange(ggplot2::aes(ymin = conf.low,
                                                     ymax = conf.high),
                                        alpha = 0.2,size=0.25)}
  p1 <- p1 +
    ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))
    ggplot2::scale_colour_brewer(palette = "Set1")

  data.info <- data.spec_curve %>%
    tidyr::pivot_longer( !! names(multiverse::parameters(.object)),
                         names_to = "parameter_name",
                         values_to = "parameter_option" ) %>%
    dplyr::filter(parameter_name %in% option)

  p2 <- data.info %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .universe, y = parameter_option,
                                     color = !!color), size = 2,shape = 124)

  if (universe_order == FALSE) {
    p2 <- p2 + ggplot2::xlab("") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())
    p1 <- p1 + ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                              axis.text.x = ggplot2::element_blank())
  } else {
    p2 <- p2 + ggplot2::xlab("universe #")
  }

  p2 <- p2 + ggplot2::ylab("Branch Options") +
    ggplot2::facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
    ggplot2::theme(strip.placement = "outside",
                   strip.background = ggplot2::element_rect(fill=NA,colour=NA),
                   panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                   panel.grid = ggplot2::element_line(colour = "grey92"),
                   panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
                   panel.spacing.x=ggplot2::unit(0.15,"cm"),
                   panel.spacing.y=ggplot2::unit(1.25, "lines"),
                   strip.text.y = ggplot2::element_text(angle = 0, face="bold", size=8),
                   legend.position = "none") +
    ggplot2::scale_colour_brewer(palette = "Set1") + ggplot2::theme(axis.title.y = element_text(size = yaxis_text_size))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}
