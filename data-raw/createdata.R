#' Read and write soccer data.
soccer <- read.csv('data-raw/CrowdstormingDataJuly1st.csv')
drops <- c('photoID', 'Alpha_3')
soccer <- soccer[, !(names(soccer) %in% drops)]
soccer$player <- stringi::stri_trans_general(soccer$player, "latin-ascii")
soccer$club <- stringi::stri_trans_general(soccer$club, "latin-ascii")
save(soccer, file='data/soccer.RData')
