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
  fam <- family_branch(poisson, gaussian(link = "log"))
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(fam) %>%
    glm_mverse()
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

test_that("glm_mverse() expects a formula branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(glm_mverse(mv), "Exactly one formula branch is required.")
})

test_that("coxph_mverse() fits a coxph model.", {
  mydf <- data.frame(
    start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
    stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
    event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    x    = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
  )
  model_spec <- formula_branch(Surv(start, stop, event) ~ x)

  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    coxph_mverse()
  fitmverse <- (multiverse::extract_variables(mv, model) %>%
                  dplyr::pull(model))[[1]]
  fitmanual <-
    survival::coxph(Surv(start, stop, event) ~ x, data = mydf)
  expect_equal(class(fitmverse)[1], "coxph")
  expect_identical(coef(fitmverse), coef(fitmanual))
})


test_that("coxph_mverse() expects a formula branch.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  mv <- mverse(mydf)
  expect_error(coxph_mverse(mv), "Exactly one formula branch is required.")
})
