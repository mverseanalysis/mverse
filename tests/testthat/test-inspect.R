context("Inspect Multiverse")

test_that("summary() prints the multiverse table for a mverse object.", {
  mydf <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")
    ) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2")
    )
  mtable <- summary(mv)
  mtable_expected <- tibble::tibble(
    universe = factor(1:(3^4)),
    m1_branch = factor(rep(c("x + y", "x - y", "x * y"), each = 3^3)),
    m2_branch = factor(rep(rep(c("x + y", "x - y", "x * y"), each = 3^2), 3)),
    f1_branch = factor(rep(rep(c("x > 0", "x < 0", "x == 0"), each = 3), 3^2)),
    f2_branch = factor(rep(c("x > 0", "x < 0", "x == 0"), 3^3))
  )
  expect_equal(nrow(mtable), 3^4)
  expect_equal(ncol(mtable), 5)
  expect_equal(
    names(mtable),
    c("universe", "m1_branch", "m2_branch", "f1_branch", "f2_branch")
  )
  expect_identical(mtable, mtable_expected)
})

test_that("summary() doesn't truncate long branch options.", {
  mydf <- data.frame(col1 = c(1, 2, 3), col2 = 4:6, col3 = 7:9)
  mbranch <- mutate_branch(
    dplyr::if_else(col1 > 1, "a", dplyr::if_else(col1 == 1, "b", "c"))
  )
  fbranch <- formula_branch(
    cbind(col1, col2 - col1) ~ col3 + col3^2 +
      col3^3 + col3^4 + exp(col3 + col3^2),
    cbind(col1, col2 - col1) ~ col3 + col3^2 +
      col3^3 + col3^4 + exp(col3) + exp(col3^2)
  )
  mv <- mverse(mydf)
  add_mutate_branch(mv, mbranch)
  add_formula_branch(mv, fbranch)
  expect_true(any(stringr::str_detect(
    sapply(unlist(summary(mv)), as.character),
    "dplyr::if_else\\(col1 > 1,"
  )))
  expect_true(any(
    stringr::str_detect(
      sapply(unlist(summary(mv)), as.character),
      "cbind\\(col1, col2 - col1\\)"
    )
  ))
})

test_that("Universe is a categorical variable in the mutiverse table.", {
  mydf <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )
  mv <- mverse(mydf)
  mv %>%
    add_mutate_branch(
      mutate_branch(x + y, x - y, x * y, name = "m1"),
      mutate_branch(x + y, x - y, x * y, name = "m2")
    ) %>%
    add_filter_branch(
      filter_branch(x > 0, x < 0, x == 0, name = "f1"),
      filter_branch(x > 0, x < 0, x == 0, name = "f2")
    )
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
    tibble::tibble(
      term = "(None)", estimate = NA, std.error = NA,
      statistic = NA, p.value = NA, conf.low = NA, conf.high = NA
    ),
    lm(y ~ 1, data = mydf) %>% broom::tidy(conf.int = TRUE),
    lm(y ~ ., data = mydf) %>% broom::tidy(conf.int = TRUE),
    lm(y ~ x1 * x2, data = mydf) %>% broom::tidy(conf.int = TRUE)
  ))
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
    tibble::tibble(
      term = "(None)", estimate = NA, std.error = NA,
      statistic = NA, p.value = NA
    ),
    lm(y ~ 1, data = mydf) %>% broom::tidy(),
    lm(y ~ ., data = mydf) %>% broom::tidy(),
    lm(y ~ x1 * x2, data = mydf) %>% broom::tidy()
  ))
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
      dplyr::rename(p = V1, n.minus.p = V2, p.star = V3)
  ))
  smverse2 <- summary(mv, output = "df") %>%
    dplyr::select(-c(universe, model_spec_branch))
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
    as.data.frame(t(c(
      summary(lm(y ~ 1, data = mydf))$r.squared,
      summary(lm(y ~ 1, data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(
      summary(lm(y ~ ., data = mydf))$r.squared,
      summary(lm(y ~ ., data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2),
    as.data.frame(t(c(
      summary(lm(y ~ x1 * x2, data = mydf))$r.squared,
      summary(lm(y ~ x1 * x2, data = mydf))$adj.r.squared
    ))) %>%
      dplyr::rename(r.squared = V1, adj.r.squared = V2)
  ))
  smverse <- summary(mv, output = "r") %>%
    dplyr::select(-c(universe, model_spec_branch))
  smverse2 <- summary(mv, output = "r.squared") %>%
    dplyr::select(-c(universe, model_spec_branch))
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
      dplyr::rename(fstatistic = value, numdf.f = numdf, dendf.f = dendf)
  ))
  smverse <- summary(mv, output = "f") %>%
    dplyr::select(-c(universe, model_spec_branch))
  smverse2 <- summary(mv, output = "fstatistic") %>%
    dplyr::select(-c(universe, model_spec_branch))
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
  expect_error(
    summary(mv, output = "x"),
    "Invalid output argument."
  )
})

test_that("multiverse_tree() expects at least one branch.", {
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf)
  expect_error(multiverse_tree(mv), "No branch to display in a tree.")
})

test_that("multiverse_tree() outputs a ggplot object.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  expect_is(mtree, "ggplot")
})

test_that("multiverse_tree() draws a graph with nodes and edges.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  expect_is(mtree$layers[[1]]$geom, "GeomEdgePath")
  expect_is(mtree$layers[[2]]$geom, "GeomPoint")
})

test_that("multiverse_tree() draws a graph with correct data.", {
  z <- mutate_branch(x, y, name = "z")
  w <- mutate_branch(x + y, x - y, name = "w")
  mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  mv <- mverse(mydf) %>%
    add_mutate_branch(z, w)
  mtree <- multiverse_tree(mv)
  expect_equal(nrow(mtree$data), 7)
  expect_equal(
    mtree$data$name,
    c("Data", "x", "y", "x_x + y", "x_x - y", "y_x + y", "y_x - y")
  )
})

test_that("multiverse_tree() runs after fitting a lm model." , {
  mydf <- data.frame(x = sample.int(25), y = sample.int(25), u = sample.int(25))
  w <- mutate_branch(x + y + u, x - y + u)
  z <- mutate_branch(x + y < mean(w), x + y > mean(w))
  frml <- formula_branch(w ~ 0 + x + y)
  mv <- mverse(mydf) %>%
    add_mutate_branch(w) %>%
    add_formula_branch(frml)
  mtree <- multiverse_tree(mv)
  mv <- mv %>%
    lm_mverse()
  mtree_lm <- multiverse_tree(mv)
  expect_true(ggplot2::is.ggplot(mtree))
  expect_equal(mtree$data$.ggraph.index, mtree_lm$data$.ggraph.index)
  expect_equal(mtree$data$.ggraph.orig_index, mtree_lm$data$.ggraph.orig_index)
})

test_that("multiverse_tree() runs after fitting a glm model." , {
  mydf <- data.frame(x = rnorm(100), y = sample.int(100) - 50)
  p1 <- mutate_branch(1 / (1 + exp(-(x + y / 100))))
  p2 <- mutate_branch(1 / (1 + exp(-(x - y / 100))))
  z <- mutate_branch(rbinom(100, 1, p1), rbinom(100, 1, p2))
  frml <- formula_branch(z ~ x + y)
  fml <- family_branch(binomial)
  mv <- mverse(mydf) %>%
    add_mutate_branch(p1, p2, z) %>%
    add_formula_branch(frml) %>%
    add_family_branch(fml)
  mtree <- multiverse_tree(mv)
  mv <- mv %>%
    glm_mverse()
  mtree_glm <- multiverse_tree(mv)
  expect_true(ggplot2::is.ggplot(mtree))
  expect_equal(mtree$data$.ggraph.index, mtree_glm$data$.ggraph.index)
  expect_equal(mtree$data$.ggraph.orig_index, mtree_glm$data$.ggraph.orig_index)
})
