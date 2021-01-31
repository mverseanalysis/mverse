context("Extract Variables from Multiverse")

test_that("extract() extracts values of branched variables from multiverse data.", {
  mydf <- data.frame(
    x=c(-1,0,1),
    y=c(2,3,4))
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m")) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, name = "f")) %>%
    execute_multiverse()
  mexp <- c(
    (mydf$x + mydf$y)[c(mydf$x > 0)],
    (mydf$x + mydf$y)[c(mydf$x < 0)],
    (mydf$x - mydf$y)[c(mydf$x > 0)],
    (mydf$x - mydf$y)[c(mydf$x < 0)],
    (mydf$x * mydf$y)[c(mydf$x > 0)],
    (mydf$x * mydf$y)[c(mydf$x < 0)])
  expect_equal(ncol(extract(mv)), 2)
  expect_equal(nrow(extract(mv)), 6)
  expect_identical(extract(mv)[['m']], mexp)
})

test_that("Universe is a categorical variable in the extracted table.", {
  mydf <- data.frame(
    x=c(-1,0,1),
    y=c(2,3,4))
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m")) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, name = "f")) %>%
    execute_multiverse()
  etable <- extract(mv)
  expect_true(is.factor(etable$universe))
})
