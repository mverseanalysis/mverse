n <- 10
mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
  dplyr::mutate(y = rnorm(n, x1 + x2))
model_spec <- formula_branch(none = y ~ 0, intercept = y ~ 1, y ~ ., y ~ x1 * x2)
family <- family_branch(gaussian)
test_that("spec_table() returns a specification table.", {
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    add_family_branch(family) %>%
    glm_mverse()
  specsummary <- spec_summary(mv, "x1")
  expect_true(all(specsummary["term"] == "x1"))
  expect_true(all(!is.na(specsummary["estimate"])))
})

test_that("spec_curve.() prints the specification curve for a glm_mverse object.", {
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  scurve <- spec_summary(mv, "x1") %>%
    spec_curve()
  expect_s3_class(scurve, "ggplot")
})
