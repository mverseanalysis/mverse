#' Read and write soccer data.
soccer <- read.csv("data-raw/CrowdstormingDataJuly1st.csv")
drops <- c("photoID", "Alpha_3")
soccer <- soccer[, !(names(soccer) %in% drops)]
soccer$player <- stringi::stri_trans_general(soccer$player, "latin-ascii")
soccer$club <- stringi::stri_trans_general(soccer$club, "latin-ascii")
usethis::use_data(soccer, compress = "gzip", overwrite = TRUE)

#' Read and write hurricane data.
data("hurricane", package = "multiverse")
usethis::use_data(hurricane, compress = "gzip", overwrite = TRUE)
