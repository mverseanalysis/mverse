context("mverse")

test_that("Instantiation works with a data frame.", {
  mydf <- data.frame(col1=c(1,2,3))
  mverse <- mverse(mydf)
  expect_equal(attr(mverse, 'source'), mydf)
})

test_that("Adding a variable branch rule checks for new variable name.", {
  mydf <- data.frame(
    col1=c(1,2,3),
    col2=c(4,5,6))
  mverse <- create_multiverse(mydf)
  expect_error(
    mverse %>% add_mutate_branch(
      mutate_branch(col1 + col2)),
    "Please specify a variable name for the branch rule:.*")
})

test_that("Multiple variable branch rules are added.", {
  mydf <- data.frame(
    col1=c(1,2,3),
    col2=c(4,5,6))
  x3 <- mutate_branch(col1 + col2, col1 - col2, name='col3')
  x4 <- mutate_branch(col1 * col2, col2 / col1, name='col4')
  mverse <- create_multiverse(mydf)
  mverse <- mverse %>%
    add_mutate_branch(x3, x4)
  expect_true('col3' %in% names(mverse$data))
  expect_true('col4' %in% names(mverse$data))
  expect_equal(mydf$col1 + mydf$col2, mverse$data$col3)
  expect_equal(mydf$col1 * mydf$col2, mverse$data$col4)

})
