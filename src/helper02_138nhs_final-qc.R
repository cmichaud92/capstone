
library(tidyverse)
library(sf)
library(DBI)
library(mapview)

# Sonnect 
con <- dbConnect(RSQLite::SQLite(), "./data/138nhs_spat.sqlite") 
dbListTables(con)
dat <- list(read_rds("./output/138nhs_clean_2018.Rds"),
          read_rds("./output/138nhs_clean_2019.Rds"),
          read_rds("./output/138nhs_clean_hist.Rds")) %>% 
  flatten() %>% 
  map(ungroup)

site <- map_df(dat[grepl("(site)", names(dat))], bind_rows)
trans <- map_df(dat[grepl("(trans)", names(dat))], bind_rows)
haul <- map_df(dat[grepl("(haul)", names(dat))], bind_rows)
count <- map_df(dat[grepl("(count)", names(dat))], bind_rows)
length <- map_df(dat[grepl("(length)", names(dat))], bind_rows)
sf_poly <- st_read(dsn = "./data/138nhs_spat.sqlite", 
                   layer = "138nhs_poly")
sf_site <- inner_join(site, filter(count, species == "CS")) %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = .$epsg[1])

mapview(sf_site, zcol = "year", cex = "fish_count")

sf_site %>% 
  count(reach, year, wt = fish_count, name = "fish_count2") %>% 
  ggplot() +
  geom_col(aes(x = year, y = fish_count2)) +
  facet_wrap(~ reach)
class(sf_poly, )

mapview::mapview(sf_poly, zcol = "year")
count %>% 
  count(species, wt = fish_count) %>% 
  mutate(species = fct_reorder(species, n)) %>% 
  ggplot() + 
  geom_col(aes(y = species, x = n)) +
  scale_x_log10()
any(duplicated(site$site_number))

site1 <- semi_join(site, haul)
haul1 <- semi_join(haul, count)
count1 <- semi_join(count, haul)
haul2 <- semi_join(haul, site)
trans1 <- semi_join(trans, site)
