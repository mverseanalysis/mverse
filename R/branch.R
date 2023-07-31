name <- function(br, name = NULL) {
  UseMethod("name")
}

name.branch <- function(br, name = NULL) {
  stopifnot(inherits(br, "branch"))
  if (is.null(name)) {
    return(br$name)
  }
  stopifnot(is.character(name))
  br$name <- name
  br$opts <- stats::setNames(
    br$opts,
    ifelse(grepl("^_\\d+", names(br$opts)),
      paste0(
        ifelse(is.null(name), "", name),
        "_", seq_len(length(br$opts))
      ),
      names(br$opts)
    )
  )
  br
}

parse <- function(br) {
  UseMethod("parse")
}

parse.branch <- function(br) {
  # initiate a branch
  head_str <- paste0(
    "branch(", br$name, "_branch,"
  )
  # construct individual branch definitions
  has_cond <- "conds" %in% names(br)
  body_str <- paste0(
    sapply(
      names(br$opts),
      function(opt) {
        paste0(
          "'", opt, "'",
          ifelse(has_cond, br$conds[which(names(br$opts) == opt)], ""),
          "~", rlang::quo_text(br$opts[[opt]])
        )
      }
    ),
    collapse = ","
  )
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ")"))
}

parse.formula_branch <- function(br) {
  # initiate a branch
  head_str <- paste0(
    "branch(", br$name, "_branch,"
  )
  # construct individual formula
  has_cond <- "conds" %in% names(br)
  body_str <- paste0(
    sapply(
      names(br$opts),
      function(opt) {
        paste0(
          "'", opt, "'",
          ifelse(has_cond, br$conds[which(names(br$opts) == opt)], ""),
          "~ formula(", rlang::quo_text(br$opts[[opt]]), ")"
        )
      }
    ),
    collapse = ","
  )
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ")"))
}

check_branch_name <- function(br) {
  check_str <- paste0("^", class(br)[1], "(.+)$")
  if (grepl(check_str, br$name)) {
    stop(paste(
      "Please specify a variable name for the branch rule:",
      br$name
    ))
  }
}

branch <- function(opts, opts_names, name, class) {
  stopifnot(
    length(unique(opts_names)) >= length(opts_names[nchar(opts_names) > 0])
  )
  if (!length(opts) > 0) {
    stop("Error: Provide at least one rule.")
  }
  if (!(is.character(name) | is.null(name))) {
    stop('Error: "name" must be a character object.')
  }
  opts <- stats::setNames(
    opts,
    ifelse(nchar(names(opts)) > 0, names(opts),
      paste0(
        ifelse(is.null(name), "", name),
        "_", seq_len(length(opts))
      )
    )
  )
  structure(
    list(
      opts = opts,
      name = name
    ),
    class = c(class, "branch")
  )
}

add_branch <- function(.mverse, brs, nms) {
  # name branch
  brs <- mapply(
    function(rl, nm) {
      if (is.null(rl$name)) {
        return(name(rl, nm))
      }
      return(rl)
    }, brs, nms,
    SIMPLIFY = FALSE
  )
  # check branch name
  sapply(brs, function(s) invisible(check_branch_name(s)))
  # replce old and add new
  attr(.mverse, "branches_list") <- attr(.mverse, "branches_list")[
    !sapply(
      attr(.mverse, "branches_list"), function(x) x$name
    ) %in% sapply(brs, function(x) x$name)
  ]
  attr(.mverse, "branches_list") <- append(
    attr(.mverse, "branches_list"), brs
  )
  # add to mverse object
  .mverse <- reset_parameters(.mverse)
  .mverse
}

code_branch <- function(.mverse, br) {
  UseMethod("code_branch")
}

reset_parameters <- function(.mverse) {
  attr(.mverse, "multiverse")[["code"]] <- NULL
  attr(.mverse, "multiverse")[["parameter_set"]] <- NULL
  attr(.mverse, "multiverse")[["parameters"]] <- list()
  multiverse::inside(.mverse, .data_mverse <- attr(.mverse, "source"))
  for (br in attr(.mverse, "branches_list")) {
    code_branch(.mverse, br)
  }
  for (br in attr(.mverse, "branches_conditioned_list")) {
    code_branch(.mverse, br)
  }
  .mverse
}

as_option_list <- function(x) {
  opts <- sapply(
    x$opts, function(s) stringr::str_replace(rlang::expr_text(s), "^~", "")
  )
  opts
}

#' Print method for \code{*_branch} objects.
#' @param x a \code{branch} object.
#' @param ... ignored. for compatibility only.
#' @return No return value, called for printing only.
#' @export
print.branch <- function(x, ...) {
  opts <- as_option_list(x)
  opts_m <- ""
  for (i in seq_len(length(opts))) {
    opts_m <- paste0(
      opts_m, "    - ",
      ifelse(is.null(x$name), "", paste0(names(opts)[i], " : ")),
      opts[i], "\n"
    )
  }
  conds_m <- ""
  if ("conds" %in% names(x)) {
    conds <- x$conds
    conds_m <- "  Conditions\n"
    for (j in seq_len(length(conds))) {
      if (nchar(conds[j]) > 0) {
        conds_m <- paste0(
          conds_m, "    - ", names(conds)[j], " : ",
          stringr::str_replace(conds[j], "%when% ", ""), "\n"
        )
      }
    }
  }
  cat(
    ifelse(is.null(x$name), "<unnamed branch>", paste0(x$name, "_branch\n")),
    "  Options\n", opts_m, conds_m,
    sep = ""
  )
  invisible()
}
