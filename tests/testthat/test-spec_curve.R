n <- 10
mydf <- data.frame(x1 = 1:n,
                   x2 = sample(1:n),
                   x3 = rnorm(n, sd = 0.5)) %>%
  dplyr::mutate(y = rnorm(n, .data$x1 + .data$x2))
model_spec <- formula_branch(none = y ~ 0,
                             intercept = y ~ 1,
                             y ~ .,
                             y ~ x1 * x2)
model_spec_2 <- formula_branch(y ~ x1,
                               covariates = c("x2", "x3"))
family <- family_branch(gaussian)

test_that("spec_summary() returns a specification table.", {
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(family) %>%
    glm_mverse()
  specsummary <- spec_summary(mv, "x1")
  expect_true(all(specsummary["term"] == "x1"))
  expect_true(all(!is.na(specsummary["estimate"])))
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec_2) %>%
    lm_mverse()
  specsummary <- spec_summary(mv, "x1")
  expect_true(all(specsummary["term"] == "x1"))
  expect_true(all(!is.na(specsummary["estimate"])))
  expect_equal(nrow(specsummary), 4)
})

test_that(
  "spec_curve() prints a ggplot object.", {
    mv <- mverse(mydf) %>%
      add_formula_branch(model_spec) %>%
      lm_mverse()
    scurve <- spec_summary(mv, "x1") %>%
      spec_curve()
    scurve
    expect_s3_class(scurve, "ggplot")
    scurve <- spec_summary(mv, "x1") %>%
      spec_curve(label = "code")
    scurve
    expect_s3_class(scurve, "ggplot")
    mv <- mverse(mydf) %>%
      add_formula_branch(model_spec_2) %>%
      add_family_branch(family) %>%
      glm_mverse()
    scurve <- spec_summary(mv, "x1") %>%
      spec_curve()
    scurve
    expect_s3_class(scurve, "ggplot")
    scurve <- spec_summary(mv, "x1") %>%
      spec_curve(label = "code")
    scurve
    expect_s3_class(scurve, "ggplot")
  }
)

