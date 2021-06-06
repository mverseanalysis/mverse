#' @rdname spec_curve
#' @export
spec_curve <- function(...) {
  UseMethod("spec_curve")
}
#' Display the specification curve.
#'
#' This method returns the specification curve plot
#' displaying all universes defined by the multiverse.
#' x-aixs corresponds to a universe and y-axis
#' represents one choice in one branch.
#' Notice that the order of universes is not corresponding
#' to the order in the summary table.
#'
#' @param .mverse a \code{mverse} object.
#' @param var name for the variable to show.
#' @param conf When \code{TRUE}, the plot shows the confidence interval.
#' @param option a vector of branches to show the options included.
#' @param universe_order When \code{TRUE}, order the universes according to
#'   the order in the summary table.
#' @param color_order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @return a specification curve graph.
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_cuve method}
#' @export
spec_curve.mverse <- function(
  .mverse, var = NULL, conf = FALSE,
  option = names(multiverse::parameters(.mverse)),
  universe_order = FALSE,
  color_order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.mverse, "mverse"))
  color <- rlang::enquo(color)
  mtable <- multiverse::expand(.mverse) %>%
    mutate(res = map(.results, 'coef')) %>%
    unnest(res) %>%
    select(.universe, !! names(multiverse::parameters(.mverse)),
           term, estimate, p.value, std.error)
  mtable <- display_branch_rules(mtable, .mverse)

  data.spec_curve <- mtable %>%
    filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      arrange(estimate)
  }

  data.spec_curve <- data.spec_curve %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (color_order == TRUE) {
    data.spec_curve <- data.spec_curve %>%
      mutate(color = ifelse(!!color,TRUE,FALSE)) %>%
      arrange(color,estimate) %>%
      mutate(.universe = seq(1,nrow(data.spec_curve)))
  }

  p1 <- data.spec_curve %>%
    ggplot(aes(.universe, estimate, color = !!color)) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:",var))

  if (conf == TRUE) {
    p1 <- p1 + geom_pointrange(aes(ymin = estimate - qnorm(0.025)*std.error,
                                   ymax = estimate + qnorm(0.025)*std.error),
                               alpha = 0.2,size=0.25)}

  data.info <- data.spec_curve %>%
    pivot_longer( !! names(multiverse::parameters(.mverse)),
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
          panel.spacing.x=unit(0.15,"cm"),
          strip.text.y = element_text(angle = 0, face="bold", size=8),
          legend.position = "none",
          panel.spacing = unit(0.25, "lines"))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}


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
  color_order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.lm_mverse, "lm_mverse"))
  conf.int <<- conf.int
  conf.level <<- conf.level
  color <- rlang::enquo(color)
  mtable <- summary(.lm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      arrange(estimate)
  }

  data.spec_curve <- data.spec_curve %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (color_order == TRUE) {
    data.spec_curve <- data.spec_curve %>%
      mutate(color = ifelse(!!color,TRUE,FALSE)) %>%
      arrange(color,estimate) %>%
      mutate(.universe = seq(1,nrow(data.spec_curve)))
  }

  p1 <- data.spec_curve %>%
    ggplot(aes(.universe, estimate, color = !!color)) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + geom_pointrange(aes(ymin = conf.low,
                                   ymax = conf.high),
                               alpha = 0.2,size=0.25)}

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
#' @param .glm_mverse a \code{lm_mverse} object.
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
  color_order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.glm_mverse, "glm_mverse"))
  color <- rlang::enquo(color)
  conf.int <<- conf.int
  conf.level <<- conf.level
  mtable <- summary(.glm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var)

  if (universe_order == FALSE) {
    data.spec_curve <- data.spec_curve %>%
      arrange(estimate)
  }

  data.spec_curve <- data.spec_curve %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (color_order == TRUE) {
    data.spec_curve <- data.spec_curve %>%
      mutate(color = ifelse(!!color,TRUE,FALSE)) %>%
      arrange(color,estimate) %>%
      mutate(.universe = seq(1,nrow(data.spec_curve)))
  }

  p1 <- data.spec_curve %>%
    ggplot(aes(.universe, estimate, color = !!color)) +
    geom_point(size = 0.25) +
    labs(x = "", y = paste("coefficient of\n:",var))

  if (conf.int == TRUE) {
    p1 <- p1 + geom_pointrange(aes(ymin = conf.low,
                                   ymax = conf.high),
                               alpha = 0.2,size=0.25)}

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
          panel.spacing.x=unit(0.15,"cm"),
          strip.text.y = element_text(angle = 0, face="bold", size=8),
          legend.position = "none",
          panel.spacing = unit(0.25, "lines"))

  cowplot::plot_grid(p1, p2, axis = "bltr",
                     align = "v", ncol = 1, rel_heights = c(1, 2))
}

