context("Inspect Multiverse")

test_that("summary() prints the multiverse table for a mverse object.", {
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
  mtable <- summary(mv)
  mtable_expected <- tibble::tibble(
    universe = factor(1:(3^4)),
    m1_branch = factor(rep(c("x + y", "x - y", "x * y"), each = 3^3)),
    m2_branch = factor(rep(rep(c("x + y", "x - y", "x * y"), each = 3^2), 3)),
    f1_branch = factor(rep(rep(c("x > 0", "x < 0", "x == 0"), each = 3), 3^2)),
    f2_branch = factor(rep(c("x > 0", "x < 0", "x == 0"), 3^3)))
  expect_equal(nrow(mtable), 3^4)
  expect_equal(ncol(mtable), 5)
  expect_equal(
    names(mtable),
    c("universe", "m1_branch", "m2_branch", "f1_branch", "f2_branch"))
  expect_identical(mtable, mtable_expected)
})

test_that("Universe is a categorical variable in the mutiverse table.", {
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
  mtable <- summary(mv)
  expect_true(is.factor(mtable$universe))
})

test_that("summary.lm_mverse() outputs coefficient estimates with 95% confidence intervals by default.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, list(
    tibble::tibble(term = "(None)", estimate = NA, std.error = NA,
               statistic = NA, p.value = NA, conf.low = NA, conf.high = NA),
    lm(y ~ 1, data = mydf) %>% broom::tidy(conf.int = TRUE),
    lm(y ~ ., data = mydf) %>% broom::tidy(conf.int = TRUE),
    lm(y ~ x1 * x2, data = mydf) %>% broom::tidy(conf.int = TRUE)))
  smverse <- summary(mv) %>% dplyr::select(-c(universe, model_spec_branch))
  expect_identical(smverse, smanu)
})

test_that("summary.lm_mverse(., conf.int = FALSE) outputs coefficient estimates without any confidence intervals.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, list(
    tibble::tibble(term = "(None)", estimate = NA, std.error = NA,
           statistic = NA, p.value = NA),
    lm(y ~ 1, data = mydf) %>% broom::tidy(),
    lm(y ~ ., data = mydf) %>% broom::tidy(),
    lm(y ~ x1 * x2, data = mydf) %>% broom::tidy()))
  smverse <- summary(mv, conf.int = FALSE) %>% dplyr::select(-c(universe, model_spec_branch))
  expect_identical(smverse, smanu)
})

test_that("summary.lm_mverse(output = 'df') outputs degrees of freedom.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, list(
    tibble::tibble(p = NA, n.minus.p = NA, p.star = NA),
    as.data.frame(t(summary(lm(y ~ 1, data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3),
    as.data.frame(t(summary(lm(y ~ ., data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3),
    as.data.frame(t(summary(lm(y ~ x1 * x2, data = mydf))$df)) %>%
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3)))
  smverse2 <- summary(mv, output = "df") %>% dplyr::select(-c(universe, model_spec_branch))
  expect_identical(smverse2, smanu)
})

test_that("summary.lm_mverse(output = 'r') outputs R squared values.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, list(
    tibble::tibble(r.squared = NA, adj.r.squared = NA),
    as.data.frame(t(c(summary(lm(y ~ 1, data = mydf))$r.squared,
                      summary(lm(y ~ 1, data = mydf))$adj.r.squared))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(summary(lm(y ~ ., data = mydf))$r.squared,
                      summary(lm(y ~ ., data = mydf))$adj.r.squared))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(summary(lm(y ~ x1 * x2, data = mydf))$r.squared,
                      summary(lm(y ~ x1 * x2, data = mydf))$adj.r.squared))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2)))
  smverse <- summary(mv, output = "r") %>% dplyr::select(-c(universe, model_spec_branch))
  smverse2 <- summary(mv, output = "r.squared") %>% dplyr::select(-c(universe, model_spec_branch))
  expect_identical(smverse, smanu)
  expect_identical(smverse2, smanu)
})

test_that("summary.lm_mverse(output = 'f') outputs F statistics.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  smanu <- do.call(rbind, list(
    tibble::tibble(fstatistic = NA, numdf.f = NA, dendf.f = NA),
    tibble::tibble(fstatistic = NA, numdf.f = NA, dendf.f = NA),
    as.data.frame(t(summary(lm(y ~ ., data = mydf))$fstatistic)) %>%
      dplyr::rename(fstatistic = value, numdf.f = numdf, dendf.f = dendf),
    as.data.frame(t(summary(lm(y ~ x1 * x2, data = mydf))$fstatistic)) %>%
      dplyr::rename(fstatistic = value, numdf.f = numdf, dendf.f = dendf)))
  smverse <- summary(mv, output = "f") %>% dplyr::select(-c(universe, model_spec_branch))
  smverse2 <- summary(mv, output = "fstatistic") %>% dplyr::select(-c(universe, model_spec_branch))
  expect_identical(smverse, smanu)
  expect_identical(smverse2, smanu)
})

test_that("summary.lm_mverse(output = 'x') throws an invalid output argument error.", {
  n <- 10
  mydf <- data.frame(x1 = 1:n, x2 = sample(1:n)) %>%
    dplyr::mutate(y = rnorm(n, x1 + x2))
  model_spec <- formula_branch(y ~ 0, y ~ 1, y ~ ., y ~ x1 * x2)
  mv <- mverse(mydf) %>%
    add_formula_branch(model_spec) %>%
    lm_mverse()
  expect_error(summary(mv, output = "x"),
               "Invalid output argument.")
})
