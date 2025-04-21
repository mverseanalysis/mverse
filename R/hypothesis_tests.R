#' Performs one or two sample t-tests on data columns.
#'
#' \code{t_test_mverse} performs t-tests across the multiverse.
#' If x or y is specified, then performs one and two sample t-tests
#' on specified columns of the data. If both x and y are NULL, then
#' performs t.test based on the formula branches.
#'
#' @examples
#' # Performing a unpaired two sample t-test.
#' library(dplyr)
#' mv <- soccer |>
#'   filter(!is.na(rater1), !is.na(rater2)) |>
#'   mverse()
#' x <- mutate_branch(
#'   ((rater1 + rater2) / 2) > mean((rater1 + rater2) / 2),
#'   ifelse(rater1 > rater2, rater1 > 0.5, rater2 > 0.5)
#' )
#' y <- mutate_branch(
#'   redCards, yellowCards, yellowReds
#' )
#' two_sample_form <- formula_branch(y ~ x)
#' mv <- mv %>%
#'   add_mutate_branch(x, y) %>%
#'   add_formula_branch(two_sample_form)
#' t_test_mverse(mv)
#' @param .mverse a \code{mverse} object.
#' @param x (optional) column name of data within mverse object
#' @param y (optional) column name of data within mverse object
#' @param alternative a character string specifying the alternative hypothesis,
#'        must be one of "two.sided" (default), "greater" or "less". You can
#'        specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference
#'        in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two
#'        variances as being equal.
#' @param conf.level confidence level of the interval.
#' @return a multiverse table displaying the t-test results as a tibble.
#' @name t_test_mverse
#' @importFrom rlang .data
#' @export
t_test_mverse <-
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
    if (rlang::quo_is_null(x)) {
      multiverse::inside(.mverse, {
        .ttest_mverse <- stats::t.test(
          stats::formula(.formula_mverse),
          data = .data_mverse,
          alternative = !!rlang::enexpr(alternative),
          mu = !!rlang::enexpr(mu),
          paried = !!rlang::enexpr(paired),
          var.equal = !!rlang::enexpr(var.equal),
          conf.level = !!rlang::enexpr(conf.level)
        )
      })
    } else if (rlang::quo_is_null(y)) {
      multiverse::inside(.mverse, {
        .ttest_mverse <- stats::t.test(
          x = .data_mverse %>% dplyr::pull(!!rlang::get_expr(x)),
          alternative = !!rlang::enexpr(alternative),
          mu = !!rlang::enexpr(mu),
          paried = !!rlang::enexpr(paired),
          var.equal = !!rlang::enexpr(var.equal),
          conf.level = !!rlang::enexpr(conf.level)
        )
      })
    } else {
      multiverse::inside(.mverse, {
        .ttest_mverse <- stats::t.test(
          x = .data_mverse %>% dplyr::pull(!!rlang::get_expr(x)),
          y = .data_mverse %>% dplyr::pull(!!rlang::get_expr(y)),
          alternative = !!rlang::enexpr(alternative),
          mu = !!rlang::enexpr(mu),
          paried = !!rlang::enexpr(paired),
          var.equal = !!rlang::enexpr(var.equal),
          conf.level = !!rlang::enexpr(conf.level)
        )
      })
    }
    multiverse::inside(.mverse, {
      out <-
        as.data.frame(t(
          c(
            .ttest_mverse$statistic,
            .ttest_mverse$p.value,
            .ttest_mverse$conf.int,
            .ttest_mverse$estimate
          )
        )) %>%
        dplyr::rename(
          statistic = t,
          p.value = "V2",
          conf.lower = "V3",
          conf.upper = "V4"
        )
    })
    execute_multiverse(.mverse)
    mtable <- multiverse::extract_variables(.mverse, out) %>%
      tidyr::unnest(out)
    display_branch_opts(mtable, .mverse)
  }
