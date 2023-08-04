test_that("Instantiation works with a data frame (mverse).", {
  mydf <- data.frame(col1 = c(1, 2, 3))
  mv <- mverse(mydf)
  expect_equal(attr(mv, "source"), mydf)
})

test_that("Instantiation works with a data frame (create_multiverse).", {
  mydf <- data.frame(col1 = c(1, 2, 3))
  mv <- mverse(mydf)
  expect_equal(attr(mv, "source"), mydf)
})

test_that(
  "Initializing with an object that is not a data frame throws an error.", {
    a_list <- list("a", "b", "c")
    expect_error(mv <- mverse(a_list))
    an_arrary <- c("a", "b", "c")
    expect_error(mv <- mverse(an_array))
  }
)
