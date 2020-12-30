
######################################################################################################
#                                            NH_hist_io                                              #
######################################################################################################

#
# Reads in .DBF files from Habitat Study 1992 - 1996
# Saves the raw data to .rds for easier access
#

hs_hist_io <- function(file_path = "./data/138nhs/historic_db") {

  require(foreign)
  require(tidyverse)

  files <- list.files(path = file_path, pattern = '.DBF$', full.names = TRUE)
  names <- list.files(path = file_path, pattern = '.DBF$', full.names = FALSE)

  dbf <- map(files, read.dbf)
  names(dbf) <- names

  fish_tmp <- dbf[ grepl("[2,3,4,5,6,7]FISH.DBF$", names(dbf)) ]
  sein_tmp <- dbf[ grepl("[2,3,4,5,6,7]SEIN.DBF", names(dbf)) ]
  phys_tmp <- dbf[ grepl("[2,3,4,5,6,7]PHYS.DBF", names(dbf)) ]

  # Create function to mutate factors to charactors and apply to df lists
  func <- function(x) modify_if(x, is.factor, as.character)

  # Apply fx and transform into tibble
  fish <- as_tibble(map_df(fish_tmp, func))
  sein <- as_tibble(map_df(sein_tmp, func))
  phys <- as_tibble(map_df(phys_tmp, func))

  # Create a list contaning all 3 data setts
  hs_data <- list(fish, sein, phys)
  names(hs_data) <- c("fish", "sein", "phys")

  # Save the list to .rds
  #write_rds(hs_data, "./01_data/HistDB/hist_NH_raw.RDS")

  hs_data
}

#NH_hist_io()




