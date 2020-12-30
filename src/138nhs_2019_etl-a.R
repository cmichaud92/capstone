
##################################################################################################################
#                                   NURSERY HABITAT 2019 data import and restructure                             #
##################################################################################################################

library(tidyverse)
library(lubridate)
library(stringr)
library(UCRBtools)
#library(waterData)


# Source spatial dataset
source("./src/helper03_138nhs_spatial-qaqc.R")


#-----------------------------------------------------------------------------------------------------------------
# Data data import and restructure
#-----------------------------------------------------------------------------------------------------------------

# Import the 2019 dataset (DataPlus collection application) and convert all colnames to lower case using tolwr
data <- dbf_io(file_path_in = "./data/138nhs/2019") %>%     # can store in separate pass dirs...
  map(rename_all, tolower)

#----------------------------------------------------
# site table
#----------------------------------------------------

# Extract site data, create site_id col and remove location fields
  site <- bind_rows(data[grep("site", names(data))])%>%
      mutate(date = mdy(date),
           year = year(date),
           project = "138nhs",
           site_id = paste(paste(project, year(date), str_pad(site_num, width = 3, side = "left", pad = "0"), sep = "_")),
           reach = ifelse(river == "CO" & rmi < 50, "LITTLE", reach),
           pass = 5,
           hab_geom = case_when(grepl("(SC)|(SR)", hab_geom) ~ "SC",
                                grepl("(SH)", hab_geom) ~ "SE",
                                TRUE ~ hab_geom),
           hab_aspect = as.integer(hab_aspect)) %>%
    select(-c(ilat, ilon)) %>%
    select(project,
           year,
           date,
           river,
           reach,
           site_id,
           pass,
           rmi:notes_site,
           key_a) %>%
    rename(hab_length = hab_lengt) %>%
    left_join(select(sf_hs, site_id = site_number,
                     hab_area, utm_x, utm_y, epsg)) %>%
    select(-geometry)

 # site$geometry <- NULL

#-----------------------------------------------------
# Trans table
#-----------------------------------------------------

# Modify the depth table and join to the transect table
  depth <- bind_rows(data[grep("depth", names(data))]) #%>%

 # depth <- hs_bindrows(data = data, table = "depth")
  depth$loc_depth <- tolower(depth$loc_depth)

# Must spread the depth table prior to join
  depth <- depth %>%
    select(-key_aaa) %>%
    gather(variable, value, (depth:sub2)) %>%
    unite(temp, loc_depth, variable) %>%
    spread(temp, value, convert = TRUE)

# Join transect and depth data, combine notes, remove old note cols
  trans <- bind_rows(data[grep("trans", names(data))]) %>%
    full_join(depth) %>%
    left_join(select(site, key_a, site_id)) %>%
    mutate(notes_trans = paste(notes_hab, notes_dep, sep = " | "),
           trans_id = paste(site_id, str_pad(location, width = 2, side = "left", pad = "0"), sep = ".")) %>%
    select(-c(notes_hab, notes_dep)) %>%
    select(site_id, trans_id,
           location:d1_depth,
           d1_tot_dep,
           d1_sub_1 = d1_sub1,
           d1_sub_2 = d1_sub2,
           d2_depth,
           d2_tot_dep,
           d2_sub_1 = d2_sub1,
           d2_sub_2 = d2_sub2,
           dmax_depth,
           dmax_tot_dep,
           dmax_sub_1 = dmax_sub1,
           dmax_sub_2 = dmax_sub2,
           notes_trans,
           key_a, key_aa)

#-------------------------------------------------------
# Haul table
#-------------------------------------------------------

# Extract the haul data, add the site_id, date and create haul_id
  haul <- bind_rows(data[grep("haul", names(data))]) %>%
    full_join(select(site, key_a, site_id, date)) %>%
    mutate(haul_id = paste(site_id, str_pad(haul_num, width = 2, side = "left", pad = "0"), sep = "."),
           datetime = as.POSIXct(paste(ymd(date), time))) %>%
    select(-c(haul_num, date, time)) %>%
    rename(haul_length = haul_lengt)

  table(haul$method)
  anyNA(haul$method)
  haul[55,4:5] <- NA
#-------------------------------------------------------
# Length table
#-------------------------------------------------------

# Extract the length data, add the site_id and haul_id vars
#   2019 no PIT tagged fish encounters
#   correct known data entry issues
  length <- bind_rows(data[grep("length", names(data))]) %>%
    left_join(select(haul, key_a, key_ab, site_id, haul_id)) %>%

    select(site_id, haul_id,
           species:notes_len,
           key_a:key_abb) %>%

    mutate(species = ifelse(species == "OT", "UI", species),
           species = ifelse(notes_len %in% "SHINER", "UM", species)) %>%

    select(-key_abb)

  #table(length$species)
  #sum(is.na(length$species))

#---------------------------------------------------------
# Count table
#---------------------------------------------------------

# Build the count table
#   Scrape count data from the length table
  count_tmp <- length %>%
    group_by(key_a, key_ab, site_id, haul_id, species) %>%
    summarise(fish_count = n())

# Extract the count data and bind the data scraped from the length table
#   Correct known data entry issues
  count <- bind_rows(data[grep("count", names(data))]) %>%
    left_join(select(haul, key_a, key_ab, site_id, haul_id)) %>%
    bind_rows(count_tmp) %>%
    mutate(species = ifelse(species == "OT", "UI", species)) %>%
    group_by(key_a, key_ab, site_id, haul_id,  species) %>%
    summarise(fish_count = sum(fish_count)) %>%
#    mutate(species = ifelse(species == "OT", "UI", species)) %>%
    select(site_id, haul_id,
           species, fish_count,
#           notes_count = notes_ct,
           key_a, key_ab)

  count %>%
  group_by(species) %>%
  summarise(fish_count = sum(fish_count))
  sum(count$fish_count)

#---------------------------------------------------------
# Final structured dataset list
#---------------------------------------------------------

# Bind tables into a named list
  clean_hs_19 <- list(site, trans, haul, count, length) %>%
    map(modify_if, is.POSIXct, as.character) %>%
    map(modify_if, is.Date, as.character) %>%
    map(ungroup) %>%
    set_names(c("site", "trans", "haul", "count", "length"))

  write_rds(clean_hs_19, "./output/138nhs_2019_clean.Rds")


  ## END

