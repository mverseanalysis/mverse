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
# from jung
hurricane_jung <- readxl::read_excel("data-raw/pnas.1402786111.sd01.xlsx")
hurricane_jung <- hurricane_jung[!is.na(hurricane_jung$Name), ]
hurricane_jung$Year <- as.numeric(hurricane_jung$Year)
hurricane_jung <- dplyr::select(
  hurricane_jung, -tidyr::starts_with("Z")
)
# from simonsohn
hurricane_simonsohn <- haven::read_dta("https://osf.io/w3br2/download")
hurricane_katrina_audrey <- dplyr::filter(
  hurricane_simonsohn, name %in% c("Katrina", "Audrey")
)
# updated min pressures for Katrina and Audrey from
# https://www.aoml.noaa.gov/hrd/hurdat/All_U.S._Hurricanes.html
hurricane_katrina_audrey[["min_2014"]] <- c(946, 920)
hurricane_katrina_audrey <- dplyr::select(
  hurricane_katrina_audrey, year, name, masfem, min, min_2014,
  gender_mf, category, alldeaths, ndam, elapsedyrs, source
)
# match names
names(hurricane_katrina_audrey) <- names(hurricane_jung)
hurricane <- dplyr::bind_rows(hurricane_jung, hurricane_katrina_audrey)
names(hurricane)[c(5, 10)] <- c("Minpressure_Updated_2014", "Elapsed.Yrs")
# add additional columns from simohnson
hurricane <- dplyr::inner_join(
  hurricane, dplyr::select(
    hurricane_simonsohn, year, name, wind, masfem_mturk, ndam15
  ),
  by = c("Name" = "name", "Year" = "year")
)
names(hurricane)[12:14] <- c("HighestWindSpeed", "MasFem_MTUrk", "NDAM15")
usethis::use_data(hurricane, overwrite = TRUE)
tools::checkRdaFiles("data/hurricane.rda")
tools::resaveRdaFiles("data/hurricane.rda", compress = "bzip2")
