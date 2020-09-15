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
  rlang::parse_expr(
    paste0(head_str, body_str, ')'))
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

print_branch_all <- function(.mverse) {
  brnches <- attr(.mverse, 'manipulate_branches')
  sapply(brnches, print_branch_single)
}

print_branch_single <- function(vb) {
  rules <- character(length(vb$rules))
  for(i in 1:length(vb$rules)) {
    rules[i] <- rlang::quo_name(vb$rules[[i]])
  }
  out <- list(rules)
  names(out) <- vb$name
  out
}

print_branches_table <- function(vbprint, mtable) {
  for(nm in names(vbprint)) {
    replace_this<- paste0(nm, '_', 1:length(vbprint[[nm]]))
    brnch <- paste0(nm,'_branch')
    mtable[[brnch]] <- mtable[[brnch]] %>%
      dplyr::recode(!!! stats::setNames(vbprint[[nm]], replace_this))
  }
  mtable
}
