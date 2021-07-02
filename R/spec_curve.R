#' Display a specification curve of fitting \code{lm} across the multiverse.
#'
#' \code{spec_curve.lm_mverse} returns the specification curve of \code{lm}
#' regression results across the multiverse.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .lm_mverse a \code{lm_mverse} object.
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
#' @return a multiverse table as a tibble
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.lm_mverse <- function(
  .lm_mverse, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.lm_mverse)),
  universe_order = FALSE,
  color_order = FALSE, color = (p.value < 0.05),
  branch_order = NULL) {
  stopifnot(inherits(.lm_mverse, "lm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  conf.int <<- conf.int
  conf.level <<- conf.level
  color_order <<- color_order
  universe_order <<- universe_order
  color <- rlang::enquo(color)
  branch_order <- rlang::enquo(branch_order)
  mtable <- summary(.lm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      arrange(estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!branch_order, !!color, estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!branch_order, estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!color, estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  p1 <- data.spec_curve %>%
    ggplot(aes(.universe, estimate, color = !!color)) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + geom_pointrange(aes(ymin = conf.low,
                                   ymax = conf.high),
                               alpha = 0.2,size=0.25)}
  p1 <- p1 + theme_minimal()

  data.info <- data.spec_curve %>%
    pivot_longer( !! names(multiverse::parameters(.lm_mverse)),
      names_to = "parameter_name",
      values_to = "parameter_option" ) %>%
    filter(parameter_name %in% option)

  p2 <- data.info %>%
    ggplot() +
    geom_point(aes(x = .universe, y = parameter_option,
                   color = !!color), size = 2,shape = 124)

  if (universe_order == FALSE) {
    p2 <- p2 + xlab("universe counts")
  } else {
    p2 <- p2 + xlab("universe #")
  }

  p2 <- p2 + ylab("option included in the analysis specification") +
    facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour=NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(size = rel(0.5)),
          panel.spacing.x=unit(0.15,"cm"),
          strip.text.y = element_text(angle = 0, face="bold", size=8),
          legend.position = "none",
          panel.spacing = unit(0.25, "lines"))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}

#' Display a specification curve of fitting \code{glm} across the multiverse.
#'
#' \code{spec_curve.glm_mverse} returns the specification curve of \code{glm}
#' regression results across the multiverse.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .glm_mverse a \code{glm_mverse} object.
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
#' @return a multiverse table as a tibble
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.glm_mverse <- function(
  .glm_mverse, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.glm_mverse)),
  universe_order = FALSE,
  color_order = FALSE, color = (p.value < 0.05),
  branch_order = NULL) {
  stopifnot(inherits(.glm_mverse, "glm_mverse"))
  if (is.null(var)) {
    stop("Please specify the variable to display.")
  }
  color <- rlang::enquo(color)
  conf.int <<- conf.int
  conf.level <<- conf.level
  branch_order <- rlang::enquo(branch_order)
  mtable <- summary(.glm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      arrange(estimate)

    if (color_order == TRUE & !rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!branch_order, !!color, estimate)
    } else if (!rlang::quo_is_null(branch_order)) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!branch_order, estimate)
    } else if (color_order == TRUE) {
      data.spec_curve <- data.spec_curve %>%
        arrange(!!color, estimate)
    }
  }

  data.spec_curve <- data.spec_curve %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  p1 <- data.spec_curve %>%
    ggplot(aes(.universe, estimate, color = !!color)) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + geom_pointrange(aes(ymin = conf.low,
                                   ymax = conf.high),
                               alpha = 0.2,size=0.25)}
  p1 <- p1 + theme_minimal()

  data.info <- data.spec_curve %>%
    pivot_longer( !! names(multiverse::parameters(.glm_mverse)),
                  names_to = "parameter_name",
                  values_to = "parameter_option" ) %>%
    filter(parameter_name %in% option)

  p2 <- data.info %>%
    ggplot() +
    geom_point(aes(x = .universe, y = parameter_option,
                   color = !!color), size = 2,shape = 124)

  if (universe_order == FALSE) {
    p2 <- p2 + xlab("universe counts")
  } else {
    p2 <- p2 + xlab("universe #")
  }

  p2 <- p2 + ylab("option included in the analysis specification") +
    facet_grid(parameter_name ~ ., space="free_y", scales="free_y")+
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA,colour=NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(size = rel(0.5)),
          panel.spacing.x=unit(0.15,"cm"),
          strip.text.y = element_text(angle = 0, face="bold", size=8),
          legend.position = "none",
          panel.spacing = unit(0.25, "lines"))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}
