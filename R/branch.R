name <- function(brnch, name = NULL) {
  UseMethod("name")
}

name.branch <- function(brnch, name = NULL) {
  stopifnot(inherits(brnch, "branch"))
  if(is.null(name))
    return(brnch$name)
  stopifnot(is.character(name))
  brnch$name <- name
  brnch
}

parse <- function(brnch) {
  UseMethod('parse')
}

parse.branch <- function(brnch) {
  stopifnot(inherits(brnch, "branch"))
  # initiate a branch
  head_str <- paste0(
    "branch(", brnch$name, "_branch,")
  # construct individual branch definitions
  idx <- 1:length(brnch$rules)
  body_str <- paste0(
    mapply(
      function(i, x) paste0(
        "'", brnch$name, '_', i, "'~", rlang::quo_name(x)),
      idx, brnch$rules),
    collapse=',')
  # parse as an expression
  rlang::parse_expr(paste0(head_str, body_str, ')'))
}

reset_parameters <- function(.mverse) {
  attr(.mverse, "multiverse")[['code']] <- NULL
  multiverse::inside(.mverse, orig <- attr(.mverse, 'source'))
  multiverse::inside(.mverse, data <- orig)
  for (br in attr(.mverse, 'manipulate_branches')) {
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
    }
  }
  .mverse
}
