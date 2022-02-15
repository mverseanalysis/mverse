context("Branch Definition")

test_that("Branches require at least one rule.", {
  expect_error(
    mutate_branch(),
    'Error: Provide at least one rule.')
  expect_error(
    filter_branch(),
    'Error: Provide at least one rule.')
})

test_that("*_branch() defines branches without 'name'.", {
  expect_equal(
    rlang::quo_name(mutate_branch(x + y)$opts[[1]]), "x + y")
  expect_equal(
    rlang::quo_name(filter_branch(x > 0)$opts[[1]]), "x > 0")
})

test_that("*_branch() defines branches with names specified.", {
  expect_equal(mutate_branch(x + y, name = "xnew")$name, "xnew")
  expect_equal(filter_branch(x + y, name = "xnew")$name, "xnew")
})

test_that("*_branch() checks a provided name is a character.", {
  expect_error(
    mutate_branch(x + y, name = 0),
    'Error: "name" must be a character object.')
  expect_error(
    filter_branch(x > 0, name = 0.5),
    'Error: "name" must be a character object.')
})

test_that("*_brach() defines branches with multiple options.", {
  mbranch <- mutate_branch(x + y, x - y, x * y)
  expect_equal(
    rlang::quo_name(mbranch$opts[[1]]), "x + y")
  expect_equal(
    rlang::quo_name(mbranch$opts[[2]]), "x - y")
  expect_equal(
    rlang::quo_name(mbranch$opts[[3]]), "x * y")
  expect_equal(length(mbranch$opts), 3)
  fbranch <- filter_branch(x > 0, x < 0, x == 0)
  expect_equal(
    rlang::quo_name(fbranch$opts[[1]]), "x > 0")
  expect_equal(
    rlang::quo_name(fbranch$opts[[2]]), "x < 0")
  expect_equal(
    rlang::quo_name(fbranch$opts[[3]]), "x == 0")
  expect_equal(length(fbranch$opts), 3)

})

context("Branch Naming and Parsing")

test_that("name() extracts the name of a branch.", {
  mbranch <- mutate_branch(x + y, x - y, x * y, name = "mutate")
  expect_equal(name(mbranch), "mutate")
  fbranch <- filter_branch(x > 0, x < 0, x == 0, name = "filter")
  expect_equal(name(fbranch), "filter")
  frmbranch <- formula_branch(y ~ x, y ~ log(x), name = "formula")
  expect_equal(name(frmbranch), "formula")
  fambranch <- family_branch(poisson, gaussian(link = "log"), name = "family")
  expect_equal(name(fambranch), "family")
})

test_that("name() renames a branch.", {
  mbranch <- mutate_branch(x + y, x - y, x * y, name = "mutate")
  mbranch <- name(mbranch, "mrename")
  expect_equal(name(mbranch), "mrename")
  fbranch <- filter_branch(x > 0, x < 0, x == 0, name = "filter")
  fbranch <- name(fbranch, "frename")
  expect_equal(name(fbranch), "frename")
})

test_that("parse() creates a branching command for multiverse.", {
  mbranch <- mutate_branch(x + y, x - y, x * y, name = "m")
  expect_equal(parse(mbranch), rlang::parse_expr(
    'branch(m_branch, "m_1" ~ x + y, "m_2" ~ x - y, "m_3" ~ x * y)'))
  fbranch <- filter_branch(x > 0, x < 0, x == 0, name = "f")
  expect_equal(parse(fbranch), rlang::parse_expr(
    'branch(f_branch, "f_1" ~ x > 0, "f_2" ~ x < 0, "f_3" ~ x == 0)'))
})

test_that("parse() handles long branch options.", {
  mydf <- data.frame(col1=c(1,2,3))
  mbranch <- mutate_branch(
    dplyr::if_else(col1 > 1, "a", dplyr::if_else(col1 == 1, "b", "c")))
  mv <- mverse(mydf) %>%
    add_mutate_branch(mbranch)
  execute_multiverse(mv)
  multiverse::code(mv)[[3]]
  expect_true(any(
    stringr::str_detect(
      sapply(multiverse::code(mv), as.character),
      "dplyr::if_else\\(col1 > 1,")))
  fbranch <- formula_branch(
    cbind(col1, col2 - col1) ~ col3 + col3^2 + col3^3 + col3^4 + exp(col3 + col3^2),
    cbind(col1, col2 - col1) ~ col3 + col3^2 + col3^3 + col3^4 + exp(col3) + exp(col3^2)
  )
  add_formula_branch(mv, fbranch)
  expect_true(any(
    stringr::str_detect(
      sapply(multiverse::code(mv), as.character),
      "cbind\\(col1, col2 - col1\\)")))
})

context("Branch Add and Remove")

test_that("add_*_branch() adds a branch.", {
  mydf <- data.frame(
    x=c(1,2,3),
    y=c(4,5,6))
  mv <- mverse(mydf)
  mbranch <- mutate_branch(x + y, x - y, x * y, name = "m")
  fbranch <- filter_branch(x > 0, x < 0, x == 0, name = "f")
  mv %>%
    add_mutate_branch(mbranch) %>%
    add_filter_branch(fbranch)
  expect_equal(attr(mv, "branches_list")[[1]]$name, "m")
  expect_equal(attr(mv, "branches_list")[[2]]$name, "f")
})

test_that("add_*_branch() adds multiple branches in order.", {
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
  nms <- sapply(attr(mv, "branches_list"), function(x) x$name)
  expect_equal(nms, c("m1", "m2", "f1", "f2"))
})

test_that("add_*_branch() checks for a new variable name.", {
  mydf <- data.frame(
    x=c(1,2,3),
    y=c(4,5,6))
  mverse <- create_multiverse(mydf)
  expect_error(
    mverse %>% add_mutate_branch(
      mutate_branch(x + y)),
    "Please specify a variable name for the branch rule:.*")
  expect_error(
    mverse %>% add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0)),
    "Please specify a variable name for the branch rule:.*")
})
