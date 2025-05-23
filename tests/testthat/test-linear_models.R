test_that("lm_mverse() fits a lm model.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  fitmverse <- summary(mv)
  fitmanual <- lm(y ~ x1 * x2, data = mydf)
  expect_true(all(fitmverse[1:4, 4] == coef(fitmanual)))
})


test_that("glm_mverse() fits a glm model.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = max(rpois(1, x1 + x2), 1))
  model_spec <- formula_branch(y ~ x1 * x2)
  fam <- family_branch(poisson, gaussian(link = "log"))
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(fam) %>%
    glm_mverse()
  fitmverse <- summary(mv)
  fitmanual <- glm(y ~ x1 * x2, data = mydf, family = poisson)
  expect_true(all(fitmverse[1:4, 5] == coef(fitmanual)))
})

test_that("lm_mverse() expects a formula branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(lm_mverse(mv), "Exactly one formula branch is required.")
})

test_that("glm_mverse() expects a formula branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(glm_mverse(mv), "Exactly one formula branch is required.")
})
