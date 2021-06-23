context("Specification curve of mverse")

test_that("spec_curve.lm_mverse() prints the specification curve for a lm_mverse object.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  spec_mv <- spec_curve(mv, var = "x1")

  expect_is(spec_mv, "ggplot")
})

test_that("spec_curve.glm_mverse() prints the specification curve for a glm_mverse object.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  model_family <- family_branch(gaussian)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(model_family) %>%
    glm_mverse()
  spec_mv <- spec_curve(mv, var = "x1")

  expect_is(spec_mv, "ggplot")
})
