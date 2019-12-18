context("Helpers")

test_that("Instantiation works with a data frame.", {
  mydf <- data.frame(col1=c(1,2,3))
  mverse <- create_multiverse(
    data.frame(col1=c(1,2,3))
  )
  expect_equal(mverse$source, mydf)
})

test_that("Multiple variable branches can be defined using enquos.", {
  new_variable <- variable_branch(x1 + x2, mean(c(x1,x2)))
  expect_equal(
    rlang::quo_name(new_variable$branches[[1]]), "x1 + x2")
  expect_equal(
    rlang::quo_name(new_variable$branches[[2]]), "mean(c(x1, x2))")
  expect_equal(
    length(new_variable$branches), 2)
})

test_that("Variable branches can be defined wihtout a varname specified.", {
  new_variable <- variable_branch(x1 + x2, mean(c(x1,x2)))
  expect_equal(
    new_variable$varname, NULL)
})

test_that("Variable branches can be defined with a varname specified.", {
  new_variable <- variable_branch(
    varname="x3", x1 + x2, mean(c(x1,x2)))
  expect_equal(
    new_variable$varname, "x3")
})

test_that("Adding a variable branch rule checks for new variable name.", {
  mydf <- data.frame(col1=c(1,2,3))
  mverse <- create_multiverse(
    data.frame(col1=c(1,2,3))
  )
  expect_error(
    mverse %>% add_variable_branch(
      variable_branch(x1 + x2)),
    "Please specify a variable name for the branch rule:.*")
})
