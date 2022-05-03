#' Create a new branch condition.
#'
#' A branch condition conditions option \code{x} to
#' depend on option \code{y}. When the branch condition
#' is added to a \code{mverse} object, option \code{x}
#' is executed only when \code{y} is. Use \code{reject = TRUE},
#' to negate the condition.
#'
#' @examples
#' # Example branches.
#' y <- mutate_branch(alldeaths, log(alldeaths + 1))
#' model <- formula_branch(y ~ femininity * strength, y ~ femininity + strength)
#' # Define a new branch condition.
#' match_poisson <- branch_condition(alldeaths, poisson)
#' # Define a branch condition that reject an option dependent on another.
#' match_log_lin <- branch_condition(log(alldeaths + 1), poisson, reject = TRUE)
#'
#' @param x option 1
#' @param y option 2
#' @param reject if TRUE, the condition rejects universes
#'   with option 1 and option 2
#' @return A \code{branch_condition} object.
#' @name branch_condition
#' @family {methods for working with a branch condition}
#' @export
branch_condition <- function(x, y, reject = FALSE) {
  x <- stringr::str_replace(rlang::expr_text(rlang::enquo(x)), "^~", "")
  y <- stringr::str_replace(rlang::expr_text(rlang::enquo(y)), "^~", "")
  if (any(stringr::str_starts(c(x, y), "\""))) {
    stop(
      "You must provide the options as expressions not strings.",
      "\ni.e.,",
      '\n(x) Incorrect usage: branch_condition("x", "y")',
      "\n(o) Correct usage: branch_condition(x, y)"
    )
  }
  structure(
    list(x = x, y = y, reject = reject),
    class = c("branch_condition")
  )
}

#' @rdname add_branch_condition
#' @export
add_branch_condition <- function(.mverse, ...) {
  UseMethod("add_branch_condition")
}

#' Add branch conditions to a \code{mverse} object.
#'
#' This method adds one or more branch conditions to
#' an existing \code{mverse} object. Branch conditions
#' are used to specify an option in one branch dependent
#' on an option in another branch.
#'
#' @examples
#' # Define branches and add them to an \code{mverse} object.
#' y <- mutate_branch(alldeaths, log(alldeaths + 1))
#' distribution <- family_branch(poisson, gaussian)
#' # You can match branching options by providing the options
#' # the way provide them when defining branches.
#' match_poisson <- branch_condition(alldeaths, poisson)
#' mv <- mverse(hurricane) %>%
#'   add_mutate_branch(y) %>%
#'   add_family_branch(distribution) %>%
#'   add_branch_condition(match_poisson)
#' summary(mv)
#' # You can also condition to reject a pair of options by
#' # setting reject = TRUE.
#' match_log_lin <- branch_condition(log(alldeaths + 1), poisson, reject = TRUE)
#' mv <- add_branch_condition(mv, match_log_lin)
#' summary(mv)
#' @param .mverse a \code{mverse} object.
#' @param ... branch conditions.
#' @return a \code{mverse} object.
#' @name add_branch_condition
#' @family {methods for working with a branch condition}
#' @export
add_branch_condition.mverse <- function(.mverse, ...) {
  conds <- list(...)
  # add condition to branches
  sapply(conds, add_branch_condition_single, .mverse)
  # add to list
  attr(.mverse, "conditions_list") <- append(
    attr(.mverse, "conditions_list"), conds
  )
  # reparse branches
  .mverse <- reset_parameters(.mverse)
  invisible(.mverse)
}

add_branch_condition_single <- function(cond, .mverse) {
  stopifnot(inherits(cond, "branch_condition"))
  brs <- attr(.mverse, "branches_list")
  for (i in seq_len(length(brs))) {
    if (any(as_option_list(brs[[i]]) == cond$x)) {
      br_x <- brs[[i]]
      br_x_i <- i
    } else if (any(as_option_list(brs[[i]]) == cond$y)) {
      br_y <- brs[[i]]
    }
  }
  brs_c <- attr(.mverse, "branches_conditioned_list")
  if (length(brs_c) > 0) {
    for (i in seq_len(length(brs_c))) {
      if (any(as_option_list(brs_c[[i]]) == cond$x)) {
        if (which(as_option_list(brs_c[[i]]) == cond$x) %in%
            which(nchar(brs_c[[i]]$conds) > 0)) {
          stop(
            "Option ", cond$x,
            " is already conditioned. Try conditioning on another option."
          )
        } else {
          br_x <- brs_c[[i]]
        }
      } else if (any(as_option_list(brs_c[[i]]) == cond$y)) {
        br_y <- brs_c[[i]]
      }
    }
  }
  # switch x and y to avoid circular conditioning
  x <- cond$x
  y <- cond$y
  if (name(br_x) %in% sapply(br_y[["conds_on"]], function(s) name(s))) {
    tmp <- x
    x <- y
    y <- tmp
    tmp <- br_x
    br_x <- br_y
    br_y <- tmp
    rm(br_x_i)
  }
  if (!"conds" %in% names(br_x)) {
    br_x[["conds"]] <- character(length(br_x$opts))
    names(br_x[["conds"]]) <- names(as_option_list(br_x))
    br_x[["conds_on"]] <- list(br_y)
  } else {
    if (!name(br_y) %in% sapply(br_x[["conds_on"]], function(s) name(s))) {
      br_x[["conds_on"]][[length(br_x[["conds_on"]]) + 1]] <- br_y
    }
  }
  # add condition
  br_x[["conds"]][which(as_option_list(br_x) == x)] <-
    paste0(
      "%when% (", br_y$name, "_branch", ifelse(cond$reject, " != \"", " == \""),
      names(as_option_list(br_y))[as_option_list(br_y) == y],
      "\")"
    )
  if (name(br_x) %in% sapply(brs_c, function(s) name(s))) {
    attr(.mverse, "branches_conditioned_list")[[
    which(name(br_x) == sapply(brs_c, function(s) name(s)))]] <- br_x
  } else {
    attr(.mverse, "branches_conditioned_list")[[length(brs_c) + 1]] <- br_x
  }
  if (exists("br_x_i")) {
    attr(.mverse, "branches_list")[[br_x_i]] <- NULL
  }
  invisible()
}
