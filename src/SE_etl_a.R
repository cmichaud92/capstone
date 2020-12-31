
##############################################################
#                         123d ETL                           #
##############################################################


#-------------------------------
# Attach packages and source QC functions
#-------------------------------

library(tidyverse)
library(lubridate)
library(stringr)
library(openxlsx)
library(sf)
library(UCRBtools)


source("./123d_etl_2020/src/fun/dp_ef_qcfx.R")

#------------------------------------------------------
# Enter user defined parameters
#------------------------------------------------------

# Set starting sample number
start_num <- 1


#-------------------------------
# Create row for meta table
#-------------------------------

meta <- tibble(
  project_code = "123d",
  year = 2020,
  principal_fname = "Chris",
  principal_lname = "Michaud",
  agency = "UDWR-M",
  data_type = "EL"
)

#-------------------------------
# Import field-qc data (rds)
#-------------------------------

data <- dbf_io(file_path_in = "./123d_etl_2020/data/dbf_batch1") %>%     # can store in separate pass dirs...
  map(rename_all, tolower)

#------------------------------
# Extract data from list
#------------------------------

# Combine like tables and...
# Remove "Z" and complete easy qc

# Site data
site_tmp <- map_df(data[grepl("site", names(data))], bind_rows) %>%
  mutate_all(na_if, "Z") %>%
  select(-site_notes)                                                    #compatability with EF_V7

# Water data
water_tmp <- map_df(data[grepl("water", names(data))], bind_rows) %>%
  mutate_at(c("cond_amb", "cond_spec", "rvr_temp", "secchi"), function(x) {ifelse(x == 0, NA, x)}) %>%
  mutate_all(na_if, "Z")

# Fish data
fish_tmp <- map_df(data[grepl("fish", names(data))], bind_rows) %>%
  mutate_at(c("ilat", "ilon", "tot_length", "st_length", "weight"), function(x) {ifelse(x == 0, NA, x)}) %>%
  mutate_all(na_if, "Z") %>%
  mutate(ray_ct = na_if(ray_ct, "N"),
         tubercles = ifelse(species %in% spp_nat, tubercles, NA),
         rep_cond = ifelse(grepl("(SPENT)", notes_fish), "SPENT", rep_cond),
         rep_cond = toupper(rep_cond))

# Pittag
pit_tmp <- map_df(data[grepl("pittag", names(data))], bind_rows) %>%
  filter(!is.na(pit_num)) %>%
  mutate_all(na_if, "Z")

## Floytag
# floy_tmp <- map_df(data[grepl("floytag", names(data))], bind_rows) %>%
#   filter(!is.na(floy_num)) %>%
#   mutate_all(na_if, "Z")

#------------------------------
# Modify data
#------------------------------

# Create sample_number and index,
# Create fnl table structures

# Site table

site <- site_tmp %>%
  mutate(startdatetime = as.POSIXct(paste(mdy(date), starttime)),         # Replace `date` and `time` with `datetime`
         enddatetime = as.POSIXct(paste(mdy(date), endtime)),
         el_sec = effort_sec + (effort_min * 60),                         # Convert effort to seconds
         project = "123d",
         year = year(startdatetime)) %>%                                  # Add year varaible

  arrange(startdatetime) %>%                                              # this orders data for indexing

  mutate(s_index = row_number(),                                          # add index for qc/site_id
         site_num_crct = s_index + (start_num - 1),
         site_id = paste(project, year(startdatetime),                    # Create sample number
                         str_pad(site_num_crct, 3, "left", "0"), sep = "_")) %>%

  rename(site_notes = notes_site) %>%

  left_join(tbl_reach, by = c("reach" = "rch_code")) %>%                   # Add rvr_abbr variable


  mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),  # Belknap correction
                                                        x + 120, x)}) %>%
  mutate(mid_rmi = (start_rmi + end_rmi) / 2) %>%
  reach(RiverCode = .$rvr_code, rmi = .$mid_rmi) %>%                       # Correct reach designation using site midpoint


  select(s_index, site_id,
         project_code = project,
         year, river = rvr_code,
         reach,
         startdatetime, enddatetime,
         start_rmi, end_rmi,
         shoreline, el_sec,
         boat, crew,
         site_notes, key_a) #%>%

s1 <- site %>%
  reach(RiverCode = .$river, rmi = .$start_rmi)

samp_n <- select(site, key_a, site_id, t_stamp = startdatetime, reach)       # Create site_id df and apply to all tables.


# Water_qual table

water <- left_join(water_tmp, samp_n, by = "key_a") %>%
  rename(water_id = key_ab,
         water_notes = notes_h2o) %>%
  arrange(t_stamp) %>%
  select(water_id, site_id,
         cond_amb, cond_spec,
         rvr_temp, secchi,
         water_notes, key_a)

# Fish table

fish_1 <- left_join(fish_tmp, samp_n, by = "key_a") %>%
  mutate(datetime = as.POSIXct(paste(as.Date(t_stamp), time))) %>%
  arrange(datetime) %>%
  mutate(f_index = row_number()) %>%
  mutate_at(vars(ends_with("rmi")), function(x) {ifelse(.$reach %in% c("DESO", "ECHO"),
                                                        x + 120, x)}) %>%
  select(f_index,
         fish_id = key_aa,
         site_id,
         rmi, datetime,
         species, tot_length,
         weight, sex,
         rep_cond, tubercles,
         ray_ct, disp,
         fish_notes = notes_fish, key_a,
         ilon, ilat)

fish_sf <- fish_1 %>%                                     # Convert long-lat to UTMs
  group_by(site_id, rmi) %>%
  summarise(ilon = mean(ilon, na.rm = TRUE),
            ilat = mean(ilat, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(ilon)) %>%
  st_as_sf(coords = c("ilon", "ilat"), crs = 4326) %>%
  st_transform(crs = 32612) %>%
  mutate(loc_x = st_coordinates(geometry)[, 1],
         loc_y = st_coordinates(geometry)[, 2],
         epsg = 32612) %>%
  st_drop_geometry() %>%
  select(site_id, rmi, loc_x, loc_y, epsg)

fish <- full_join(fish_1, fish_sf, by = c("site_id", "rmi")) %>%
  select(-c(ilat, ilon))

# Pittag table

pittag <- left_join(pit_tmp, samp_n, by = "key_a") %>%
  rename(pit_id = key_aaa,
         fish_id = key_aa) %>%
  left_join(select(fish, fish_id, datetime, species), by = c("fish_id")) %>%
  arrange(datetime) %>%
  mutate(p_index = row_number(),
         pit_num = toupper(pit_num)) %>%
  select(p_index, pit_id, fish_id, site_id,
         species,pit_type, pit_num, pit_recap,
         pit_notes, key_a)




#------------------------------
# Add data corrections here
#------------------------------

# Site table
site$crew[site$s_index == 2] <- "RM KB"
site$end_rmi[site$s_index == 15] <- 127.6
site$end_rmi[site$s_index == 73] <- 114.3
site$end_rmi[site$s_index == 75] <- 112.9
site$end_rmi[site$s_index == 78] <- 112.4
site$end_rmi[site$s_index == 79] <- 111.3

site$start_rmi[site$s_index == 75] <- 114.3
site$startdatetime[site$s_index == 75] <- as.POSIXct("2020-07-02 14:00:06")

site$boat[site$s_index %in% c(73, 75)] <- "SPARKY"

site$enddatetime[site$s_index == 20] <- site$startdatetime[site$s_index == 21]
site$enddatetime[site$s_index == 34] <- as.POSIXct("2020-05-05 15:00:35")
site$enddatetime[site$s_index == 32] <- as.POSIXct("2020-05-01 14:14:08")
site$end_rmi[site$s_index == 41] <- 109.3
site$crew[site$s_index == 51] <- "SB JC"
site$end_rmi[site$s_index == 48] <- 109.0
site <- site %>%
  filter(s_index != 51) %>%
  mutate(boat = case_when(between(s_index, 56, 60) ~ "BERT",
                          between(s_index, 61, 65) ~ "ERNIE",
                          TRUE ~ boat))
site$enddatetime[site$s_index == 52] <- as.POSIXct("2020-06-04 12:28:07")
site$enddatetime[site$s_index == 65] <- site$enddatetime[site$s_index == 64]
site$startdatetime[site$s_index == 65] <- site$startdatetime[site$s_index == 64]
site$reach[site$s_index %in% c(64, 65)] <- "LGR"

# Fish table
fish$weight[fish$f_index == 30] <- 1050
fish$disp[fish$f_index %in% c(63, 64, 65, 77, 496, 557, 560)] <- "DE"
fish <- fish %>%
  mutate(rmi = case_when(rmi == 2 ~ 114.4,
                         rmi == 3 ~ 112,
                         TRUE ~ rmi)) %>%
  filter(species %!in% c("RS", "SS", "FH"))           # Remove any smallbodied cyps


# Pit table
pittag$pit_num[pittag$p_index == 5] <- "3DD.003BFC5B4D"
pittag$pit_num[pittag$p_index == 39] <- gsub('^(.{3})(.*)$', '\\1.\\2', pittag$pit_num[pittag$p_index == 39])


#------------------------------
# QC data.tables
#------------------------------

ck_site <- site_qcfx(site_data = site)%>%
  mutate_if(is.POSIXct, force_tz, tzone = "UTC")

ck_fish <- fish_qcfx(fish_data = fish, site_data = site)%>%
  mutate_if(is.POSIXct, force_tz, tzone = "UTC")

ck_pit <- pit_qcfx(pit_data = pittag, fish_data = fish)


#--------------------------------
# Remove index columns                                      # may also need to convert POSIXct to Character here...???
#--------------------------------

site_fnl <- select(site, -c(s_index, key_a))
fish_fnl <- select(fish, -c(f_index, key_a))
pit_fnl <- select(pittag, -c(p_index, species, site_id, key_a))
water_fnl <- select(water, -key_a)

#-------------------------------------------------
# Bind cleaned data into list and export to .Rds for update scripts
#-------------------------------------------------

a <- list(meta, site_fnl, water_fnl, fish_fnl, pit_fnl) %>%           # For update script
  set_names(c("meta", "site", "water", "fish", "pit"))

write_rds(a, "./123d_etl_2020/data/clean123d_batch1.Rds")

b <- map(a, modify_if, is.POSIXct, as.character)                      # For database submission
write_rds(b, "./123d_etl_2020/data/clean123d_batch1_db.Rds")

## End
