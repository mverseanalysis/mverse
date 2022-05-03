#' Read and write soccer data.
soccer <- read.csv("data-raw/CrowdstormingDataJuly1st.csv")
drops <- c("photoID", "Alpha_3")
soccer <- soccer[, !(names(soccer) %in% drops)]
soccer$player <- stringi::stri_trans_general(soccer$player, "latin-ascii")
soccer$club <- stringi::stri_trans_general(soccer$club, "latin-ascii")
usethis::use_data(soccer, overwrite = TRUE)
tools::checkRdaFiles("data/soccer.rda")
tools::resaveRdaFiles("data/soccer.rda", compress = "bzip2")
#' Read and write hurricane data.
data("hurricane", package = "multiverse")
usethis::use_data(hurricane, overwrite = TRUE)
tools::checkRdaFiles("data/hurricane.rda")
tools::resaveRdaFiles("data/hurricane.rda", compress = "bzip2")
