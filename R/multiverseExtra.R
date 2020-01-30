#' Create a new multiverseExtra object
#'
#' Constructs a new multiverseExtra object which extends
#' \code{multiverse::multiverse} object.
#'
#' @examples
#' \dontrun{
#' M <- multiverseExtra(df)
#' }
#' @param data Source datafame.
#' @return A multiverseExtra object with the source dataframe attached.
#' @importFrom  multiverse multiverse
#' @name multiverseExtra
multiverseExtra <- function(data) {
  data_ <- rlang::enquo(data)
  .multiverse <- multiverse(class="multiverseExtra")
  attr(.multiverse, 'source') <- data
  attr(.multiverse, 'variable_branches') <- list()
  attr(.multiverse, 'model_branches') <- list()
  attr(.multiverse, 'branch_asserts') <- list()
  .multiverse$orig <- ~ attr(.multiverse, 'source')
  .multiverse
}

#' @rdname add_variable_branch
add_variable_branch.multiverseExtra <- function(.multiverse, ...) {
  varnames <- sapply(
    rlang::enquos(...),
    rlang::quo_name)
  branch_rules <- list(...)
  # name variable branches
  branch_rules <- mapply(
    function(rl, nm) {
      if(is.null(rl$name))
        return(name(rl, nm))
      return(rl)
    },
    branch_rules, varnames, SIMPLIFY = FALSE)
  # enforce variable name
  e <- sapply(
    branch_rules,
    function(x) {
      if(grepl('^variable_branch(.+)$', x$name)) {
        stop(paste(
          "Please specify a variable name for the branch rule:",
          x$name))}})
  # add to list
  attr(.multiverse, 'variable_branches') <- append(
    attr(.multiverse, 'variable_branches'),
    branch_rules)
  # add to multiverse object
  inside(.multiverse, data <- orig)
  for (br in attr(.multiverse, 'variable_branches')) {
    inside(
      .multiverse,
      data <- dplyr::mutate(
        data, !! rlang::parse_expr(br$name) := !! parse(br)))}
  .multiverse
}
