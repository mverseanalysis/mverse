#' Performs one and two sample t-tests on data columns.
#'
#' \code{t.test.mverse} fits \code{lm} across the multiverse
#' according to model specifications provided by \code{formula_branch}.
#' At least one \code{formula_branch} must have been added.
#'
#' @examples
#' \dontrun{
#' model_spec <- formula_branch(y ~ x1)
#' mv <- mv %>%
#'   add_formula_branch(model_spec) %>%
#'   fit_lm()
#' }
#' @param .mverse a \code{mverse} object.
#' @param x column name of data within mverse object
#' @param y (optional) column name of data within mverse object
#' @param alternative a character string specifying the alternative hypothesis,
#'        must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal.
#' @param conf.level confidence level of the interval.
#' @return A \code{mverse} object with \code{lm} fitted.
#' @name fit_lm
#' @family {model fitting methods}
#' @export
ttest_mverse <-
  function(.mverse,
           x,
           y = NULL,
           alternative = "two.sided",
           mu = 0,
           paired = FALSE,
           var.equal = FALSE,
           conf.level = 0.95) {
    stopifnot(inherits(.mverse, "mverse"))

    # pass function parameters
    if (!exists('mverse_params')) {
      mverse_params <<- list()
    }


    multiverse::inside(.mverse, {
      y = if (is.null(!!rlang::enexpr(y)))
        NULL
      else
        data[!!rlang::enexpr(y)]
      htest <-
        t.test(
          x = data[!!rlang::enexpr(x)],
          y = y,
          alternative = !!rlang::enexpr(alternative),
          mu = !!rlang::enexpr(mu),
          paried = !!rlang::enexpr(paired),
          var.equal = !!rlang::enexpr(var.equal),
          conf.level = !!rlang::enexpr(conf.level)
        )
      out <-
        as.data.frame(t(
          c(
            htest$statistic,
            htest$p.value,
            htest$conf.int,
            htest$estimate
          )
        )) %>%
        rename(
          statistic = t,
          p.value = V2,
          conf.lower = V3,
          conf.upper = V4
        )
    })

    attr(.mverse, "class") <- unique(c("htest_mv", class(.mverse)))
    execute_multiverse(.mverse)
    mtable <- multiverse::extract_variables(.mverse, out) %>%
      tidyr::unnest(out) %>%
      mutate(universe = factor(.universe)) %>%
      select(-starts_with(".")) %>%
      select(universe, everything())
    display_branch_rules(mtable, .mverse)
  }
