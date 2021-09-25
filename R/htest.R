#' Performs one or two sample t-tests on data columns.
#'
#' \code{ttest_mverse} performs t-tests across the multiverse.
#' If x or y is specified, then performs one and two sample t-tests
#' on specified columns of the data. If both x and y are NULL, then
#' performs t.test based on the formula branches.
#'
#' @examples
#' \dontrun{
#' mv <- mv %>%
#'   ttest_mverse()
#' }
#' @param .mverse a \code{mverse} object.
#' @param x (optional) column name of data within mverse object
#' @param y (optional) column name of data within mverse object
#' @param alternative a character string specifying the alternative hypothesis,
#'        must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#' @param conf.level confidence level of the interval.
#' @return A \code{ttest_mverse} object.
#' @name ttest
#' @family {hypothesis testing methods}
#' @export
ttest_mverse <-
  function(.mverse,
           x = NULL,
           y = NULL,
           alternative = "two.sided",
           mu = 0,
           paired = FALSE,
           var.equal = FALSE,
           conf.level = 0.95) {
    stopifnot(inherits(.mverse, "mverse"))
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    if (rlang::quo_is_null(x))
      multiverse::inside(.mverse, {
        htest <- stats::t.test(
          formulae,
          data = data,
          alternative = !!rlang::enexpr(alternative),
          mu = !! rlang::enexpr(mu),
          paried = !! rlang::enexpr(paired),
          var.equal = !! rlang::enexpr(var.equal),
          conf.level = !! rlang::enexpr(conf.level)
        )
      })
    else if (rlang::quo_is_null(y))
      multiverse::inside(.mverse, {
        htest <- stats::t.test(
          x = data %>% dplyr::pull(!! rlang::get_expr(x)),
          alternative = !! rlang::enexpr(alternative),
          mu = !! rlang::enexpr(mu),
          paried = !! rlang::enexpr(paired),
          var.equal = !! rlang::enexpr(var.equal),
          conf.level = !! rlang::enexpr(conf.level)
        )
      })
    else
      multiverse::inside(.mverse, {
        htest <- stats::t.test(
          x = data %>% dplyr::pull(!! rlang::get_expr(x)),
          y = data %>% dplyr::pull(!! rlang::get_expr(y)),
          alternative = !! rlang::enexpr(alternative),
          mu = !! rlang::enexpr(mu),
          paried = !! rlang::enexpr(paired),
          var.equal = !! rlang::enexpr(var.equal),
          conf.level = !! rlang::enexpr(conf.level)
        )
      })
    multiverse::inside(.mverse, {
      out <-
        as.data.frame(t(
          c(
            htest$statistic,
            htest$p.value,
            htest$conf.int,
            htest$estimate
          )
        )) %>%
        dplyr::rename(
          statistic = t,
          p.value = V2,
          conf.lower = V3,
          conf.upper = V4
        )
    })
    execute_multiverse(.mverse)
    mtable <- multiverse::extract_variables(.mverse, out) %>%
      tidyr::unnest(out) %>%
      dplyr::mutate(universe = factor(.universe)) %>%
      dplyr::select(-dplyr::starts_with(".")) %>%
      dplyr::select(universe, dplyr::everything())
    display_branch_opts(mtable, .mverse)
  }
