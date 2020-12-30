
#######################################################################################################################
#                                   NURSERY HABITAT 2018 data import and restructure                                  #
#######################################################################################################################

library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)


# Source spatial dataset
source("./src/helper03_138nhs_spatial-qaqc.R")

#-----------------------------------------------------------------------------------------------------------------
# Data data import and restructure
#-----------------------------------------------------------------------------------------------------------------

# Import the 2018 dataset (Excel collection template) and convert all colnames to lower case using tolwr

dat <- list()
path <-  "./data/138nhs/2018/hs_2018_complete.xlsx"
dat$hab <- read_xlsx(path, sheet = 1)
dat$meas <- read_xlsx(path, sheet = 2)
dat$haul <- read_xlsx(path, sheet = 3,
                  col_types = c(rep("guess", 7), rep("numeric", 21), "text"))
dat$length <- read_xlsx(path, sheet = 4)

# ----- correct sample number issues -----
make_sample_num <- function(x) {
  paste("138nhs", "2018", str_sub(x, -3, -1), sep = "_")
}
sample_num2 <- function(x) modify_at(x, 1, make_sample_num)

data <- dat %>%
  map(sample_num2) %>%
  map(rename_all, tolower)


#-----------------------------------------------------------------------
# site table
#-----------------------------------------------------------------------

# Extract site data, remove unnecessary cols and rename requisite cols
#   Correct known data entry issues
site <- data$hab %>%
  rename(site_number = sample_num,
         cover_typ = cover_type,
         cover_pct = `%coverage`,
         mc_secchi = mc_secci,
         hab_1 = prim_hab,
         hab_geom = hab_type,
         notes_site = comments) %>%
  mutate(cover_typ = case_when(cover_typ == "ALGAE" ~ "ALG",
                               cover_typ == "VEG" ~ "OHV",
                               cover_typ == "WOOD" ~ "DBR",
                               cover_typ == "NONE" ~ "NO",
                               TRUE ~ "NO"),
         cover_pct = ifelse(is.na(cover_pct), 0, cover_pct),
         hab_length = round(hab_length),
         date = ymd(date),
         year = year(date),
         project = "138nhs") %>%
  select(-c(waypoint))


source("./src/helper01_138nhs_2018_hab-2.R")                                # This corrects errors in recorded hab_1 and hab_2 values

site <- site %>%
  select(-c(hab_1, hab_geom)) %>%
  left_join(habs, by = "site_number") %>%
  left_join(select(sf_hs, site_number, hab_area, utm_x, utm_y, epsg)) %>%  #adds centroid coords in utm
  select(-geometry) #%>%
#  left_join(water)

site$hab_1[site$hab_1 == "MA"] <- "MC"
site$hab_geom[site$hab_geom == "BA"] <- NA
site$hab_geom[site$hab_geom == "EM"] <- NA

table(site$cover_typ)
table(site$hab_1)
table(site$hab_2)
table(site$hab_geom)
sum(is.na(site$hab_geom))
#--------------------------------------------------------------
# trans table
#--------------------------------------------------------------

# Extract transect data
  trans <- data$meas %>%
    mutate(flow = ifelse(flow == 0, "N", "Y"),
           width = round(width),
           d1_sub = case_when(d1_sub == "SAND" ~ "SASA",
                              d1_sub == "SILT" ~ "SISI",
                              d1_sub == "SA" ~ "SASA",
                              d1_sub == "SI" ~ "SISI",
                              TRUE ~ d1_sub),
           d2_sub = case_when(d2_sub == "SAND" ~ "SASA",
                              d2_sub == "SILT" ~ "SISI",
                              d2_sub == "SA" ~ "SASA",
                              d2_sub == "SI" ~ "SISI",
                              TRUE ~ d2_sub),
           sub_max = case_when(sub_max == "SAND" ~ "SASA",
                              sub_max == "SILT" ~ "SISI",
                              sub_max == "SA" ~ "SASA",
                              sub_max == "SI" ~ "SISI",
                              TRUE ~ sub_max),
           d1_sub_1 = str_sub(d1_sub, 1, 2),
           d1_sub_2 = str_sub(d1_sub, 3, 4),
           d2_sub_1 = str_sub(d2_sub, 1, 2),
           d2_sub_2 = str_sub(d2_sub, 3, 4),
           dmax_sub_1 = str_sub(sub_max, 1, 2),
           dmax_sub_2 = str_sub(sub_max, 3, 4),
           trans_number = paste(sample_num, str_pad(location, width = 2, side = "left", pad = "0"), sep = ".")) %>%
          # d1_sub_2 = ifelse(d1_sub_2 == "ND", "SA", d1_sub_2),
          # d1_sub_2 = ifelse(d1_sub_2 == "LT", "SI", d1_sub_2),
          # d1_sub_2 = ifelse(d1_sub_2 == "", d1_sub_1, d2_sub_2),
          # d1_sub_2 = ifelse(d1_sub_2 == "CI", "CO", d1_sub_2)

    rename(notes_trans = comments,
           secchi = secci,
           site_number = sample_num,
           d1_depth = d1,
           d1_tot_dep = tot_d1,
           d2_depth = d2,
           d2_tot_dep = tot_d2,
           dmax_depth = dmax,
           dmax_tot_dep = tot_dmax) %>%
  select(-c(d1_sub, d2_sub, sub_max))

trans$d1_sub_2[trans$d1_sub_2 == "CO"] <- "RU"
trans$d2_sub_2[trans$d2_sub_2 %in% c("CO", "CI")] <- "RU"
trans$dmax_sub_2[trans$dmax_sub_2 == "CO"] <- "RU"
trans$dmax_sub_2[trans$dmax_sub_2 == "GI"] <- "GR"

table(trans$dmax_sub_2)

widths <- trans %>%
  group_by(site_number) %>%
  summarise(hab_width = round(mean(width, na.rm = TRUE)),
            .groups = "drop")
#--------------------------------------------------------------------
# haul table
#--------------------------------------------------------------------

# Extract haul data, remove fish matrix
haul_tmp <- data$haul %>%
    rename(site_number = sample_num,
           notes_haul = comments) %>%
  left_join(select(site, site_number, date)) %>%


    mutate(datetime = as.POSIXct(paste(ymd(date), strftime(time, format = "%H:%M:%S", tz = "UTC"))),
           haul_number = paste(site_number, haul_num, sep = "."),
           method = toupper(method))

  haul <- haul_tmp %>%
    select(site_number, haul_number,
           datetime, method,
           haul_length, haul_width,
           notes_haul)
table(haul$method)
#---------------------------------------------------------------------
# length table
#---------------------------------------------------------------------

# Natives
  natives <- c("CS", "RZ", "BT", "HB", "SD", "FM", "BH", "SU", "CH")
# Extract length data, create haul_number var
  length <- data$length %>%
    rename(site_number = sample_num,
           length = tl) %>%
    mutate(haul_number = paste(site_number, haul_num, sep = "."),
           disp = ifelse(species %in% natives, "RA", "DE")) %>%
    rename(notes_len = comments) %>%
    select(-haul_num)

#---------------------------------------------------------------------
# count table
#---------------------------------------------------------------------

# Extract count data from the fish matrix
  ct_tmp <- length %>%
    group_by(site_number, haul_number, species) %>%
    summarise(fish_count = n(),
              .groups = "drop")
  count <- haul_tmp %>%
    select(site_number, haul_number, cs:ui) %>%
    gather(key = "species", value = "fish_count", (cs:ui), na.rm = TRUE) %>%
    bind_rows(ct_tmp) %>%
    mutate_at("species", toupper) %>%
    group_by(site_number, haul_number, species) %>%
    summarise(fish_count = sum(fish_count, na.rm = TRUE),
              .groups = "drop")

#---------------------------------------------------------------------
# Final data set: modified for db schema
#---------------------------------------------------------------------
  fnl_site <- site %>%
    left_join(widths, by = "site_number") %>%
    select(id_site = site_number,
           rmi_bel = rmi,
           cd_rvr = river,
           cd_rch = reach,
           dt_site = date,
           mc_secchi:year,
           cd_study = project,
           hab1 = hab_1,
           hab2 = hab_2,
           hab_geom,
           hab_width,
           loc_x = utm_x,
           loc_y = utm_y,
           epsg
           )

  fnl_trans <- trans %>%
    rename(id_site = site_number,
           id_trans = trans_number,
           d1_totdep = d1_tot_dep,
           d1_sub1 = d1_sub_1,
           d1_sub2 = d1_sub_2,
           d2_totdep = d2_tot_dep,
           d2_sub1 = d2_sub_1,
           d2_sub2 = d2_sub_2,
           dmax_totdep = dmax_tot_dep,
           dmax_sub1 = dmax_sub_1,
           dmax_sub2 = dmax_sub_2)

  fnl_haul <- haul %>%
    rename(id_site = site_number,
           id_haul = haul_number,
           tm_start = datetime,
           haul_notes = notes_haul)

  fnl_fish <- length %>%
    select(id_haul = haul_number,
           cd_spp = species,
           tot_length = length,
           disp,
           fish_notes = notes_len)

  fnl_count <- count %>%
    rename(id_haul = haul_number,
           cd_spp = species,
           n_fish = fish_count)


#--------------------------------------
# Save qc'd and upload ready to .Rds
#--------------------------------------
fnl_names <- grep("^fnl_",names(.GlobalEnv),value=TRUE) %>%
  sort()

fnl_dat<-do.call("list",mget(fnl_names))

fnl_names <- str_remove(fnl_names, "fnl_")

names(fnl_dat) <- fnl_names

fnl_dat  %>%
  map(modify_if, is.POSIXct, as.character) %>%
  map(modify_if, is.Date, as.character) %>%
  saveRDS(file = "./output/138nhs_2018_clean.Rds")

## END

