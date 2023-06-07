mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

test_that("branch_condition() stores x and y options without evaluating the expressions.", {
  cond <- branch_condition(log(x + 1), y)
  expect_equal(cond$x, "log(x + 1)")
  expect_equal(cond$y, "y")
})

test_that("branch_condition() expects expressions not strings.", {
  expect_error(
    cond <- branch_condition("log(x + 1)", "y"),
    "^You must provide the options as expressions not strings..+",
  )
})

test_that("branch_condition() stores negation.", {
  cond <- branch_condition(log(x + 1), y)
  expect_false(cond$reject)
  cond <- branch_condition(log(x + 1), y, TRUE)
  expect_true(cond$reject)
})


test_that("add_branch_condition() adds a branch condition.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  cond <- branch_condition(x, x - y)
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_branch_condition(cond)
  expect_equal(attr(mv, "conditions_list")[[1]], cond)
  expect_equal(attr(mv, "branches_conditioned_list")[[1]]$conds_on[[1]], w)
  expect_match(attr(mv, "branches_conditioned_list")[[1]]$conds["z_1"], "==")
})

test_that("add_branch_condition() stores negation.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  cond <- branch_condition(x, x - y)
  cond_n <- branch_condition(x, x - y, TRUE)
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_branch_condition(cond)
  expect_match(attr(mv, "branches_conditioned_list")[[1]]$conds["z_1"], "==")
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_branch_condition(cond_n)
  expect_match(attr(mv, "branches_conditioned_list")[[1]]$conds["z_1"], "!=")
})

test_that("add_branch_condition() stops double conditioning.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  cond <- branch_condition(x, x - y)
  cond_illegal <- branch_condition(x, x + y, TRUE)
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_branch_condition(cond)
  expect_error(
    add_branch_condition(mv, cond_illegal),
    "Option x is already conditioned."
  )
})

test_that("add_branch_condition() works with named branch options.", {
  z <- mutate_branch(x = x, y, name = "z")
  w <- mutate_branch(x + y, subtract = x - y, name = "w")
  cond <- branch_condition(x, x - y)
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w) %>%
    add_branch_condition(cond)
  expect_match(
    attr(mv, "branches_conditioned_list")[[1]]$conds['x'], "subtract")
})
