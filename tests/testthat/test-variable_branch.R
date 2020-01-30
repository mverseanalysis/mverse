context("variable_branch")

test_that("Multiple variable branches can be defined using enquos.", {
  new_variable <- variable_branch(x1 + x2, mean(c(x1, x2)))
  expect_equal(
    rlang::quo_name(new_variable$rules[[1]]), "x1 + x2")
  expect_equal(
    rlang::quo_name(new_variable$rules[[2]]), "mean(c(x1, x2))")
  expect_equal(
    length(new_variable$rules), 2)
})

test_that("Variable branches can be defined wihtout a name specified.", {
  new_variable <- variable_branch(x1 + x2, mean(c(x1,x2)))
  expect_equal(
    new_variable$name, NULL)
})

test_that("Variable branches can be defined with a name specified.", {
  new_variable <- variable_branch(
    x1 + x2, mean(c(x1,x2)), name="x3")
  expect_equal(
    new_variable$name, "x3")
})

test_that("Variable branch name is a character.", {
  expect_error(
    variable_branch(x1 + x2, mean(c(x1,x2)), name=0),
    'Error: "name" must be a character object.'
  )
})
