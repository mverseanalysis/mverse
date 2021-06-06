context("Fit Model Branches")

test_that("lm_mverse() fits a lm model.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  fitmverse <- (multiverse::extract_variables(mv, model) %>%
    dplyr::pull(model))[[1]]
  fitmanual <- lm(y ~ x1 * x2, data = mydf)
  expect_equal(class(fitmverse), "lm")
  expect_identical(coef(fitmverse), coef(fitmanual))
})


test_that("glm_mverse() fits a glm model.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = max(rpois(1, x1 + x2), 1))
  model_spec <- formula_branch(y ~ x1 * x2)
  fam <- family_branch(
      poisson, gaussian(link = "log")
    )
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(fam) %>%
    glm_mverse(mv)
  fitmverse <- (multiverse::extract_variables(mv, model) %>%
                  dplyr::pull(model))[[1]]
  fitmanual <- glm(y ~ x1 * x2, data = mydf, family = poisson)
  expect_equal(class(fitmverse)[1], "glm")
  expect_identical(coef(fitmverse), coef(fitmanual))
})

test_that("lm_mverse() expects a formula branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(lm_mverse(mv), "Exactly one formula branch is required.")
})
