
##################################################################################################################
#                                                hs_spatial_io                                                   #
##################################################################################################################

# Imports raw shapefile downloaded from arconline as sf object
# Saves as .rds

library(sf)
library(mapview)
library(dplyr)
library(stringr)
library(readr)

# Import the data as sf object
  sf_data <- st_read(dsn = "./data/138nhs/spatial", layer = "nh_hab_fnl")

# Convert col names to lowercase
  names(sf_data) <- tolower(names(sf_data))

# glimpse data and remove unnecessary cols
 # glimpse(sf_data)
  sf_data <- sf_data %>%
    select(site_number = sample_num,
           shape_leng = shape__len,
           shape_area = shape__are,
           geometry)

# Correct 2018 sample number issues
  sf_18 <- sf_data %>%
    filter(str_detect(site_number, "^HS")) %>%
    mutate(site_number = paste("138nhs", "2018", str_sub(site_number, -3, -1), sep = "_"),
           year = 2018)

# Correct 2019 sample number issues and eliminate one row.
  sf_19 <- sf_data %>%
    filter(str_detect(site_number, "^[\\d]+(?<!a)$")) %>%
    mutate(site_number = paste("138nhs", "2019", str_pad(site_number, width = 3, side = "left", pad = "0"), sep = "_"),
           year = 2019)

# Reunite the spatial dataset
  sf_hs <- rbind(sf_18, sf_19)
  # plot(sf_hs["year"])
  # mapview(sf_hs["year"])
# Extract crs
  st_crs(sf_hs)  # Projection is in web mercator.

# Convert to UTM zone 12, WGS84 for storage (epsg)
  sf_hs <- st_transform(sf_hs, crs = 32612) %>%
    mutate(hab_area = round(st_area(sf_hs$geometry)))

# Calculate polygon centroids, add to sf_hs for mapping purposes
# Also, add epsg column
  sf_tmp <- sf_hs %>%
    st_centroid()

  sf_hs$utm_x <- st_coordinates(sf_tmp)[, 1]
  sf_hs$utm_y <- st_coordinates(sf_tmp)[, 2]
  sf_hs$epsg <- st_crs(sf_hs)$epsg

  # st_write(sf_hs, dsn = "./data/138nhs_spat.sqlite", layer = "138nhs_poly",
  #          driver = "SQLite")
  # st_drivers()

## END
