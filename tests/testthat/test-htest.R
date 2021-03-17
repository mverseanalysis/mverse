test_that("ttest_mverse() computes t.test.", {
  mydf <- data.frame(
    x=rnorm(100, 0, 2),
    y=rnorm(100)
  )
  mv <- create_multiverse(mydf)
  mbranch <- mutate_branch(x + y, x - y, x * y, name = "m")
  mv %>%
    add_mutate_branch(mbranch) %>%
    ttest_mverse(x)
  fitmverse <- (multiverse::extract_variables(mv, htest) %>% pull(htest))[[1]]
  fitmanual <- t.test(mydf$x)
  expect_identical(fitmverse$statistic, fitmanual$statistic)
  ttest_mverse(mv, 'x', 'm')
  fitmverse <- (multiverse::extract_variables(mv, htest) %>% pull(htest))[[1]]
  fitmanual <- t.test(mydf$x, mydf$x + mydf$y)
  expect_identical(fitmverse$statistic, fitmanual$statistic)
})
