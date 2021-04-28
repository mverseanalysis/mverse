library(dplyr)
#' Read and write soccer data.
soccer <- read.csv('data-raw/CrowdstormingDataJuly1st.csv')
drops <- c('photoID', 'Alpha_3')
soccer <- soccer[, !(names(soccer) %in% drops)]
soccer$player <- stringi::stri_trans_general(soccer$player, "latin-ascii")
soccer$club <- stringi::stri_trans_general(soccer$club, "latin-ascii")
save(soccer, file='data/soccer.RData')
#' Read and write Bechdel test data.
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
movies <- tuesdata$movies %>%
  select(
    year, title, imdb, imdb_id, test, clean_test, binary,
    budget, domgross, intgross, budget_2013, domgross_2013, intgross_2013,
    language, country, rated, genre, awards,
    director, writer, actors, plot, runtime, poster,
    metascore, imdb_rating, imdb_votes, released, type
    ) %>%
  mutate(clean_test = factor(
    clean_test, c("nowomen", "notalk", "men", "dubious", "ok")))
save(movies, file = 'data/movies.RData')
