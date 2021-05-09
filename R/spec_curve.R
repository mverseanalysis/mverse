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
#'
#' @param .mverse a \code{mverse} object.
#' @param var name for the variable to show.
#' @param conf a logical to decide if we show the confidence interval.
#' @param option a vector of branches to show the options included.
#' @param order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @return a specification curve graph.
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_cuve method}
#' @export
spec_curve.mverse <- function( # need to define spec_curve for glm_mverse, lm_mverse, etc.
  .mverse, var = NULL, conf = FALSE,
  option = names(multiverse::parameters(.mverse)),
  order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.mverse, "mverse"))
  color <- rlang::enquo(color)
  mtable <- multiverse::expand(.mverse) %>%
    mutate(res = map(.results, 'coef')) %>% # see glm_mverse and summary.glm_mverse for the variable name
    unnest(res) %>%
    select(.universe, !! names(multiverse::parameters(.mverse)),
           term, estimate, p.value, std.error)
  mtable <- display_branch_rules(mtable, .mverse)

  data.spec_curve <- mtable %>%
    filter(term == var) %>%
    arrange(estimate) %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (order == TRUE) {
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
                   color = !!color), size = 2,shape = 124) +
    labs( x = "universe #",
          y = "option included in the analysis specification")+
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
#'
#' @param .lm_mverse a \code{lm_mverse} object.
#' @param var name for the variable to show.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param option a vector of branches to show the options included.
#' @param order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @return a multiverse table as a tibble
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.lm_mverse <- function(
  .lm_mverse, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.lm_mverse)),
  order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.lm_mverse, "lm_mverse"))
  conf.int <<- conf.int
  conf.level <<- conf.level
  color <- rlang::enquo(color)
  mtable <- summary(.lm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var) %>%
    arrange(estimate) %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (order == TRUE) {
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
                   color = !!color), size = 2,shape = 124) +
    labs( x = "universe #",
          y = "option included in the analysis specification")+
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
#'
#' @param .glm_mverse a \code{lm_mverse} object.
#' @param var name for the variable to show.
#' @param conf.int When \code{TRUE} (default), the estimate output
#'   includes the confidence intervals.
#' @param conf.level The confidence level of the confidence interval
#'   returned using \code{conf.int = TRUE}. Default value is 0.95.
#' @param option a vector of branches to show the options included.
#' @param order When \code{TRUE}, the estimated value will be ordered
#'   according to the color.
#' @param color specify the color in the plot.
#' @return a multiverse table as a tibble
#' @import dplyr stringr str_replace ggplot2
#' @name spec_curve
#' @family {spec_curve method}
#' @export
spec_curve.glm_mverse <- function(
  .glm_mverse, var = NULL, conf.int = TRUE,
  conf.level = 0.95,
  option = names(multiverse::parameters(.glm_mverse)),
  order = FALSE, color = (p.value < 0.05)) {
  stopifnot(inherits(.glm_mverse, "glm_mverse"))
  color <- rlang::enquo(color)
  conf.int <<- conf.int
  conf.level <<- conf.level
  mtable <- summary(.glm_mverse,
                    conf.int = conf.int, conf.level = conf.level)

  data.spec_curve <- mtable %>%
    filter(term == var) %>%
    arrange(estimate) %>%
    mutate(.universe = 1:nrow(.)) %>%
    select(-term)

  if (order == TRUE) {
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
                   color = !!color), size = 2,shape = 124) +
    labs( x = "universe #",
          y = "option included in the analysis specification")+
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

