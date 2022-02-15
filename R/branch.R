name <- function(br, name = NULL) {
  UseMethod("name")
}

name.branch <- function(br, name = NULL) {
  stopifnot(inherits(br, "branch"))
  if(is.null(name))
    return(br$name)
  stopifnot(is.character(name))
  br$name <- name
  br
}

parse <- function(br) {
  UseMethod('parse')
}

parse.branch <- function(br) {
  # initiate a branch
  head_str <- paste0(
    "branch(", br$name, "_branch,")
  # construct individual branch definitions
  has_cond <- "conds" %in% names(br)
  body_str <- paste0(
    mapply(
      function(i, x) paste0(
        "'", br$name, '_', i, "'",
        ifelse(has_cond, br$conds[i], ""),
        "~", rlang::quo_text(x)),
      1:length(br$opts), br$opts),
    collapse=',')
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ')'))
}

parse.formula_branch <- function(br) {
  # initiate a branch
  head_str <- paste0(
    "branch(", br$name, "_branch,")
  # construct individual formula
  has_cond <- "conds" %in% names(br)
  body_str <- paste0(
    mapply(
      function(i, x) paste0(
        "'", br$name, '_', i, "'",
        ifelse(has_cond, br$conds[i], ""),
        "~ formula(", rlang::quo_name(x), ")"),
      1:length(br$opts), br$opts),
    collapse=',')
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ')'))
}

check_branch_name <- function(br) {
  check_str <- paste0('^', class(br)[1], '(.+)$')
  if(grepl(check_str, br$name)) {
    stop(paste(
      "Please specify a variable name for the branch rule:",
      br$name)
    )
  }
}

add_branch <- function(.mverse, brs, nms) {
  # name branch
  brs <- mapply(
    function(rl, nm) {
      if(is.null(rl$name)) return(name(rl, nm))
      return(rl)
    }, brs, nms, SIMPLIFY = FALSE)
  # check branch name
  e <- sapply(brs, function(s) check_branch_name(s))
  # add to list
  attr(.mverse, 'branches_list') <- append(
    attr(.mverse, 'branches_list'), brs)
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  return(.mverse)
}

#' @importFrom rlang :=
code_branch <- function(.mverse, br) {
  stopifnot(inherits(br, "branch"))
  if(inherits(br, "mutate_branch")) {
    multiverse::inside(
      .mverse,
      data <- dplyr::mutate(
        data, !! rlang::parse_expr(br$name) := !! parse(br))
    )
  } else if (inherits(br, "filter_branch")) {
    multiverse::inside(
      .mverse,
      data <- dplyr::filter(data, !! parse(br))
    )
  } else if (inherits(br, "formula_branch")) {
    multiverse::inside(
      .mverse,
      formulae <- stats::formula(!! parse(br))
    )
  } else if (inherits(br, "family_branch")) {
    multiverse::inside(
      .mverse,
      family <- !! parse(br)
    )
  }
  return(invisible())
}

reset_parameters <- function(.mverse) {
  attr(.mverse, "multiverse")[['code']] <- NULL
  attr(.mverse, "multiverse")[['parameter_set']] <- NULL
  attr(.mverse, "multiverse")[['parameters']] <- list()
  multiverse::inside(.mverse, orig <- attr(.mverse, 'source'))
  multiverse::inside(.mverse, data <- orig)
  for (br in attr(.mverse, 'branches_list')) {
    code_branch(.mverse, br)
  }
  for (br in attr(.mverse, 'branches_conditioned_list')) {
    code_branch(.mverse, br)
  }
  .mverse
}

as_option_list <- function(x) {
  opts <- sapply(
    x$opts, function(s) stringr::str_replace(rlang::expr_name(s), "^~", ""))
  if(!is.null(x$name))
    opts <- stats::setNames(opts, paste0(x$name, "_", 1:length(opts)))
  return(opts)
}

#' Print method for \code{*_branch} objects.
#' @param x a \code{branch} object.
#' @param ... ignored. for compatibility only.
#' @export
print.branch <- function(x, ...) {
  opts <- as_option_list(x)
  opts_m <- ""
  for(i in 1:length(opts))
    opts_m <- paste0(
      opts_m, "    - ",
      ifelse(is.null(x$name), "", paste0(names(opts)[i], " : ")),
      opts[i], "\n")
  conds_m <- ""
  if("conds" %in% names(x)) {
    conds <- x$conds
    conds_m <- "  Conditions\n"
    for(j in 1:length(conds))
      if(nchar(conds[j]) > 0)
        conds_m <- paste0(
          conds_m, "    - ", names(conds)[j], " : ",
          stringr::str_replace(conds[j], "%when% ", ""), "\n")
  }
  cat(
    ifelse(is.null(x$name), "<unnamed branch>", paste0(x$name, "_branch\n")),
    "  Options\n", opts_m, conds_m, sep = ""
  )
}
