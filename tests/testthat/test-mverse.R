context("Initiate mverse")

test_that("Instantiation works with a data frame (mverse).", {
  mydf <- data.frame(col1=c(1,2,3))
  mverse <- mverse(mydf)
  expect_equal(attr(mverse, 'source'), mydf)
})

test_that("Instantiation works with a data frame (create_multiverse).", {
  mydf <- data.frame(col1=c(1,2,3))
  mverse <- mverse(mydf)
  expect_equal(attr(mverse, 'source'), mydf)
})
