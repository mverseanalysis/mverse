test_that("ttest_mverse() computes t.test with specified x, y columns", {
  mydf <- data.frame(
    a = rnorm(100, 0, 2),
    b = rnorm(100)
  )
  mv <- create_multiverse(mydf)
  mbranch <- mutate_branch(a + b, a - b, a * b, name = "m")
  mv %>%
    add_mutate_branch(mbranch) %>%
    ttest_mverse(x='a')
  fitmverse <- multiverse::extract_variables(mv, out) %>%
    tidyr::unnest(out)
  fitmanual <- t.test(mydf$a)
  expect_true(fitmverse$statistic[1] == fitmanual$statistic)
  ttest_mverse(mv, 'a', 'm')
  fitmverse <- multiverse::extract_variables(mv, out) %>%
    tidyr::unnest(out)
  fitmanual <- t.test(mydf$a, mydf$a + mydf$b)
  expect_true(fitmverse$statistic[1] == fitmanual$statistic)
})


test_that("ttest_mverse() computes t.test with formula branch", {
  mv <- create_multiverse(sleep)
  mbranch <- formula_branch(extra ~ group)
  mv %>%
    add_formula_branch(mbranch) %>%
    ttest_mverse()
  fitmverse <- multiverse::extract_variables(mv, out) %>%
    tidyr::unnest(out)
  fitmanual <- t.test(extra ~ group, data=sleep)
  expect_true(fitmverse$statistic[1] == fitmanual$statistic)
})
