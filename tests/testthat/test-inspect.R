context("Inspect Multiverse")

test_that("summary() prints the multiverse table for a mverse object.", {
  mydf <- data.frame(
    x=c(1,2,3),
    y=c(4,5,6))
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2"))
  mtable <- summary(mv)
  mtable_expected <- dplyr::tibble(
    universe = factor(1:(3^4)),
    m1_branch = rep(c("x + y", "x - y", "x * y"), each = 3^3),
    m2_branch = rep(rep(c("x + y", "x - y", "x * y"), each = 3^2), 3),
    f1_branch = rep(rep(c("x > 0", "x < 0", "x == 0"), each = 3), 3^2),
    f2_branch = rep(c("x > 0", "x < 0", "x == 0"), 3^3))
  expect_equal(nrow(mtable), 3^4)
  expect_equal(ncol(mtable), 5)
  expect_equal(
    names(mtable),
    c("universe", "m1_branch", "m2_branch", "f1_branch", "f2_branch"))
  expect_identical(mtable, mtable_expected)
})

test_that("Universe is a categorical variable in the mutiverse table.", {
  mydf <- data.frame(
    x=c(1,2,3),
    y=c(4,5,6))
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2"))
  mtable <- summary(mv)
  expect_true(is.factor(mtable$universe))
})
