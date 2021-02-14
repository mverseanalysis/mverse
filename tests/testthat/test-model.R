context("Fit Model Branches")

test_that("lm_mverse() fits a lm model.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  fitmverse <- (multiverse::extract_variables(mv, model) %>%
    pull(model))[[1]]
  fitmanual <- lm(y ~ x1 * x2, data = mydf)
  expect_equal(class(fitmverse), "lm")
  expect_identical(coef(fitmverse), coef(fitmanual))
})

test_that("lm_mverse() expects a formual branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(lm_mverse(mv), "Exactly one formual branch is required.")
})
