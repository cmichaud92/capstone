
########################################################################################################################
#                                                     hs_historic_io                                                   #
########################################################################################################################

# Script intended to structure historic database data set AND QAQC to current data standards

library(tidyverse)
library(openxlsx)
library(lubridate)
library(sf)

source("./src/helper04_138nhs_1992-1997_io.R")  #imports .dbf files


#------------------------------------------------------
# Import dataset as list, map var names to lower case
#------------------------------------------------------
data <- hs_hist_io() %>%
  map(rename_all, tolower)

# import spatial_rmi data
rmi_sf <- read_sf("c:/Users/cmichaud/proj_mgt/database/sqlite/ucrb_spat.sqlite",
                  layer = "belknap_tnth") %>%
  filter(rvr_code == "CO" |
           rvr_code == "GR") %>%
  st_transform(crs = 32612) %>%
  select(-c(reach, rvr_name, rvr_id))

#------------------------------------------------------
# Site data table
#------------------------------------------------------
site_tmp <- data$phys %>%
  rename(rmi = rm,
         hab_1 = majorhab,
         hab_geom = spec_class,
         hab_length = length,
         mc_temp = mctemp,
         mc_secchi = mcturb,
         hab_aspect = aspect) %>%
  mutate(hab_2 = case_when(desc %in% c("BA", "BA/FL", "DEBRIS FAN", "DF", "BACKWATER", "BA/SHORELINE", "BA//F",
                                       "BACKWATER(FLOW)", "FLOW,BA", "FL+BA") ~ "BA",
                           desc %in% c("FLOWING CHANNEL", "FLOWING CHUTE", "FLOWING SC/CH",
                                       "FLOWING SIDE CH", "FLOWING SCOUR", "SC", "FL",
                                       "FLOWING", "SLIGHT FLOW", "FLOW") ~ "SC",
                           desc == "FLOWING TRIB." ~ "TS",
                           desc %in% c("IP", "ISOLATED POOL") ~ "IP",
                           desc %in% c("SH/EM", "BA/EMBAYMENT", "BA/EM", "EMBAYMENT", "EMBAYMENT/SHORE") ~ "EM",
                           desc %in% c("SH", "SHORELINE", "SHORELINE LOW V") ~ "SH",
                           hab_geom %in% c("CE", "BE", "ER") ~ "ED",
                           hab_geom %in% c("DEBRIS FAN", "DF") ~ "BA"),
         hab_geom = case_when(grepl("(CE)|(BE)|(ER)|(SH)", hab_geom) ~ "SE",
                              grepl("HS", hab_geom) ~ "HS",
                              grepl("(MS)|(NS)", hab_geom) ~ "MS",
                              grepl("(FT)|(TRIB.*)", hab_geom) ~ "FT",
                              grepl("SC", hab_geom) ~ "SC"),
         flow = ifelse(grepl("F", desc), "Y", "N"),
         flow = ifelse(is.na(hab_2), NA, flow),
         hab_length = round(hab_length)) %>%
  select(-flow)


# site table data corrections
  site_tmp$hab_length[site_tmp$hab_length > 900] <- NA
  site_tmp$river[site_tmp$river %in% c("1.", "11", "C0")] <- "CO"
  site_tmp$hab_1[site_tmp$hab_1 == "MS"] <- "MC"
  site_tmp$hab_aspect[site_tmp$hab_aspect == 0] <- NA
  site_tmp$rmi[site_tmp$rmi == 6] <- 26
  site_tmp$mc_temp[site_tmp$mc_temp < 1] <- NA

# correct reach designations
  site_tmp <- site_tmp %>%
    mutate(reach = case_when(river == "CO" & rmi < 50 ~ "LITTLE",
                             river == "CO" & rmi >= 50 ~ "MOAB",
                             river == "GR" ~ "MINERAL"),
           site_number = paste(samnum, reach, sep = "_"),
           year = year(date),
           project = "138nhs",
           flow = ifelse(grepl("FL", desc), "Y", "N"))

# Add UTM coordinates (from rmi)

  site_sf <- left_join(site_tmp, rmi_sf, by = c("river" = "rvr_code", "rmi")) %>%
    st_as_sf() %>%
    mutate(utm_x = st_coordinates(GEOMETRY)[, 1],
           utm_y = st_coordinates(GEOMETRY)[, 2],
           epsg = 32612) %>%
    st_drop_geometry()

  site <- site_sf %>%
    select(river, rmi, date, hab_1,
           hab_2, hab_geom, hab_length,
           hab_aspect, mc_temp, mc_secchi,
           reach, site_number, year, flow,
           project, utm_x, utm_y, epsg)


# Correct rmi in original dataset
  data$phys$rm[data$phys$rm == 6] <- 26

#any(duplicated(site$site_number))
  table(site$flow)


#_________________________________________
# site QAQC helpers
#_________________________________________
# table(site$desc)
# table(site$hab_geom)
# table(site$hab_2)
# nrow(site)
# sum(is.na(site$hab_2))
# sum(!is.na(site$hab_2))
# sum(!is.na(site$desc))
# habs <- site %>% select(hab_1, hab_2, hab_geom, desc)
# hs_hist <- names(site)
# hs_hist


#---------------------------------------------------------
# Transect data table
#---------------------------------------------------------

# Look for a better way... Have to deconstruct and gather piece by piece...
# All
tr_tmp <- site_tmp %>%
  select(site_number, mwidth:t2d2)
any(duplicated(tr_tmp$site_number))
sum(duplicated(tr_tmp$site_number))

d1 <- tr_tmp %>%
  select(site_number, md1, t1d1, t2d1) %>%
  gather(key = "location", value = "d1_depth", c(md1, t1d1, t2d1)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                                  str_detect(location, "^t1") ~ 2,
                                  str_detect(location, "^t2") ~ 3),
         d1_depth = round(d1_depth * 304.8))

d2 <- tr_tmp %>%
  select(site_number, md2, t1d2, t2d2) %>%
  gather(key = "location", value = "d2_depth", c(md2, t1d2, t2d2)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                              str_detect(location, "^t1") ~ 2,
                              str_detect(location, "^t2") ~ 3),
         d2_depth = round(d2_depth * 304.8))

dmax <- tr_tmp %>%
  select(site_number, mdmax, t1dmax, t2dmax) %>%
  gather(key = "location", value = "dmax_depth", c(mdmax, t1dmax, t2dmax)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                              str_detect(location, "^t1") ~ 2,
                              str_detect(location, "^t2") ~ 3),
         dmax_depth = round(dmax_depth * 304.8))

width <- tr_tmp %>%
  select(site_number, mwidth, t1width, t2width) %>%
  gather(key = "location", value = "width", c(mwidth:t2width)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                              str_detect(location, "^t1") ~ 2,
                              str_detect(location, "^t2") ~ 3),
         width = round(width))

temp <- tr_tmp %>%
  select(site_number, mtemps, t1temps, t2temps) %>%
  gather(key = "location", value = "temp", c(mtemps:t2temps)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                              str_detect(location, "^t1") ~ 2,
                              str_detect(location, "^t2") ~ 3),
         temp = round(temp))

secchi <- tr_tmp %>%
  select(site_number, mturb, t1turb, t2turb) %>%
  gather(key = "location", value = "secchi", c(mturb:t2turb)) %>%
  mutate(location = case_when(str_detect(location, "^m") ~ 1,
                              str_detect(location, "^t1") ~ 2,
                              str_detect(location, "^t2") ~ 3))

trans <- full_join(d1, d2, by = c("site_number", "location")) %>%
  full_join(dmax, by = c("site_number", "location")) %>%
  full_join(width, by = c("site_number", "location")) %>%
  full_join(temp, by = c("site_number", "location")) %>%
  full_join(secchi, by = c("site_number", "location")) %>%
  mutate(trans_number = paste(site_number, location, sep = "."))

# trans table data corrections
trans$temp[trans$temp == 0] <- NA
trans$width[trans$width > 150] <- NA
trans$width[trans$width < 1] <- NA

trans$dmax_depth[trans$dmax_depth < 1] <- NA

trans_fnl <- trans %>%
  semi_join(site)

#---------------------------------------------------------------------
# Calculate hab_area and add to site data
#---------------------------------------------------------------------
widths <- trans %>%
  group_by(site_number) %>%
  summarise(m_width = round(mean(width, na.rm = TRUE)),
            .groups = "drop")

site <- site %>%
  left_join(widths, by = "site_number") %>%
  mutate(hab_area = hab_length * m_width) %>%
  select(-m_width)

#---------------------------------------------------------------------
# Haul table
#---------------------------------------------------------------------

hltmp <- data$sein %>%
  rename(rmi = rm) %>%
  mutate(reach = case_when(river == "CO" & rmi < 50 ~ "LITTLE",
                           river == "CO" & rmi >= 50 ~ "MOAB",
                           river == "GR" ~ "MINERAL"),
         site_number = paste(samnum, reach, sep = "_"))

method <- hltmp %>%
  select(site_number, s1p_a, s2p_a, s3p_a, s4p_a) %>%
  gather(key = "haul_num", value = "method", c(s1p_a:s4p_a)) %>%
  mutate(haul_num = case_when(str_detect(haul_num, "^s1") ~ 1,
                              str_detect(haul_num, "^s2") ~ 2,
                              str_detect(haul_num, "^s3") ~ 3,
                              str_detect(haul_num, "^s4") ~ 4))

haul_length <- hltmp %>%
  select(site_number, s1len, s2len, s3len, s4len) %>%
  gather(key = "haul_num", value = "haul_length", c(s1len:s4len)) %>%
  mutate(haul_num = case_when(str_detect(haul_num, "^s1") ~ 1,
                              str_detect(haul_num, "^s2") ~ 2,
                              str_detect(haul_num, "^s3") ~ 3,
                              str_detect(haul_num, "^s4") ~ 4),
         haul_length = as.integer(round(haul_length)))


haul_width <- hltmp %>%
  select(site_number, s1wid, s2wid, s3wid, s4wid) %>%
  gather(key = "haul_num", value = "haul_width", c(s1wid:s4wid)) %>%
  mutate(haul_num = case_when(str_detect(haul_num, "^s1") ~ 1,
                              str_detect(haul_num, "^s2") ~ 2,
                              str_detect(haul_num, "^s3") ~ 3,
                              str_detect(haul_num, "^s4") ~ 4),
         haul_width = as.integer(round(haul_width)))

# haul_dmax <- hltmp %>%
#   select(site_number, s1dmax, s2dmax, s3dmax, s4dmax) %>%
#   gather(key = "haul_num", value = "haul_dmax", c(s1dmax:s4dmax)) %>%
#   mutate(haul_num = case_when(str_detect(haul_num, "^s1") ~ 1,
#                              str_detect(haul_num, "^s2") ~ 2,
#                              str_detect(haul_num, "^s3") ~ 3,
#                              str_detect(haul_num, "^s4") ~ 4),
#          haul_dmax = round(haul_dmax * 304.8))
#
#
# haul_cs <- hltmp %>%
#   select(site_number, s1totcs, s2totcs, s3totcs, s4totcs) %>%
#   gather(key = "haul_num", value = "haul_cs", c(s1totcs:s4totcs)) %>%
#   mutate(haul_num = case_when(str_detect(haul_num, "^s1") ~ 1,
#                              str_detect(haul_num, "^s2") ~ 2,
#                              str_detect(haul_num, "^s3") ~ 3,
#                              str_detect(haul_num, "^s4") ~ 4))


ti <- site_tmp %>%
  select(site_number, date, time) %>%
  filter(!is.na(time)) %>%
  mutate(time = as.numeric(time),
         time = str_pad(time, width = 4, side = "left", pad = "0"),
         time = sub("(.{2})(.*)", "\\1:\\2", time),
         datetime = as.POSIXct(paste(ymd(date), time))) %>%
  select(-c(date, time))


haul <- haul_length %>%
  left_join(ti) %>%
  full_join(haul_width) %>%
  full_join(method) %>%
#  full_join(haul_dmax) %>%
#  full_join(haul_cs) %>%
  filter(!is.na(site_number),
         !is.na(haul_length),
         haul_length > 0) %>%
  mutate(haul_number = paste(site_number, haul_num, sep = "_")) %>%
  filter(!duplicated(haul_number)) %>%
  select(-haul_num)

haul$haul_width[haul$haul_width > 4] <- NA
haul$haul_width[haul$haul_width < 1] <- 1
#filter(haul,(is.na(haul_width)))

haul_fnl <- haul %>%
  semi_join(site)
#----------------------------------------------------------------------
# Count table
#----------------------------------------------------------------------

count <- data$fish %>%
  select(-c(habtype, permanence)) %>%
  rename(rmi = rm,
         rt = rtc,
         fm = fms,
         bh = bhs,
         gs = grs,
         bc = bcr,
         lg = lmb,
         cc = ccf,
         bb = bbh,
         su = uks,
         um = nnc,
         ui = unknown) %>%
  gather(key = "species", value = "fish_count", cs:ui, na.rm = TRUE) %>%
  filter(fish_count > 0) %>%
  mutate(reach = case_when(river == "CO" & rmi < 50 ~ "LITTLE",
                           river == "CO" & rmi >= 50 ~ "MOAB",
                           river == "GR" ~ "MINERAL"),
         site_number = paste(samnum, reach, sep = "_"),
         species = toupper(species)) %>%
  select(site_number, species, fish_count)
# count$rs[count$rs > 5000] <- NA
# count$ss[count$ss > 5000] <- NA
# count$fh[count$fh > 5000] <- NA

count_fnl <- count %>%
  semi_join(haul_fnl)
#---------------------------------------------------------------------
# Package the data into a named list
#---------------------------------------------------------------------
site <- site %>%
  rename(site_id = site_number)

trans_fnl <- trans_fnl %>%
  rename(site_id = site_number,
         trans_id = trans_number)

haul_fnl <- haul_fnl %>%
  rename(site_id = site_number,
         haul_id = haul_number)

count_fnl <- count_fnl %>%
  rename(site_id = site_number)

clean_hs_hist <- list(site, trans_fnl, haul_fnl, count_fnl)%>%
  map(modify_if, is.POSIXct, as.character) %>%
  map(modify_if, is.Date, as.character) %>%
  map(ungroup) %>%
  set_names(c("site", "trans", "haul", "count"))

write_rds(clean_hs_hist, "./output/138nhs_clean_hist.Rds")

