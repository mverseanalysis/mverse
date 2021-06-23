#' Data on Atlantic hurricanes in the U.S. between 1950 and 2012.
#'
#' A dataset for the study conducted by Jung et al. (2014) in
#' \href{https://doi.org/10.1073/pnas.1402786111}{Female hurricanes are deadlier than male hurricanes}.
#'
#' @format A data frame with 94 rows and 12 variables:
#' \describe{
#'   \item{Year}{Year in which the hurricane landed on U.S.}
#'   \item{Name}{Name of the hurricane.}
#'   \item{MasFem}{Femininity index of the hurricane name
#'   (1 - very masculine; 11 - very feminine).}
#'   \item{MinPressure_before}{Minimum pressure of the hurricane
#'   at the time of landfall in the U.S. (original).}
#'   \item{Minpressure_Updated_2014}{Minimum pressure of the hurricane
#'   at the time of landfall in the U.S. (updated).}
#'   \item{Gender_MF}{Gender indicator for the hurricane name based
#'   on \code{MasFem} index (1 - \code{MasFem} > 6; 0 otherwise).}
#'   \item{Category}{Hurricane category on a scale of 1 to 5,
#'   with 5 being the most severe.}
#'   \item{alldeaths}{Number of fatalities.}
#'   \item{HighestWindSpeed}{Maximum wind speed.}
#'   \item{NDAM}{Normalized damage in 2013 U.S. million dollars.}
#'   \item{Elapsed.Yrs}{Time since hurricane.}
#'   \item{Source}{Source from where the data was gathered.}
#' }
#' @details
#' The dataset was collected by Jung et al. in their study
#' \emph{Female hurricanes are deadlier than male hurricanes}.
#'
#' @source
#' Kiju Jung, Sharon Shavitt, Madhu Viswanathan, and Joseph M. Hilbe.
#' (2014). "Female hurricanes are deadlier than male hurricanes."
#' \emph{Proceedings of the National Academy of Sciences}, 111(24), 8782-8787.
#' \url{https://doi.org/10.1073/pnas.1402786111}
#'
"hurricane"
