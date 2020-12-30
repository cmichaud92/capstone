
##########################################################
#               Water data for seining                   #
##########################################################

library(dplyr)
library(waterData)


#---------------------------------------------
# Fetch waterdata from USGS
#---------------------------------------------

# GR discharge 
gr_dis <- importDVs(staid = "09315000", 
                    code = "00060", 
                    stat = "00003", 
                    sdate = "1986-01-01", 
                    edate = as.Date(Sys.Date(), 
                                    format = "%Y-%m-%d")) %>%
  rename(usgs_discharge = val) 

# GR temp 
gr_temp <- importDVs(staid = "09315000", 
                     code = "00010", 
                     stat = "00011", 
                     sdate = "1986-01-01", 
                     edate = as.Date(Sys.Date(), 
                                     format = "%Y-%m-%d")) %>%
  rename(usgs_temp = val) 

# Join green temp and discharge
gr <- full_join(gr_dis, gr_temp, by = c("staid", "dates")) %>% 
  mutate(river = "GR")
  

# CO water
co_dis <- importDVs(staid =  "09180500", 
                    code = "00060", 
                    stat = "00003", 
                    sdate = "1986-01-01", 
                    edate = as.Date(Sys.Date(), 
                                    format = "%Y-%m-%d")) %>%
  rename(usgs_discharge = val)  

# CO temp
co_temp <- importDVs(staid = "09180500", 
                     code = "00010", 
                     stat = "00003", 
                     sdate = "1986-01-01", 
                     edate = as.Date(Sys.Date(), 
                                     format = "%Y-%m-%d")) %>%
  rename(usgs_temp = val)

# Join colorado temp and discharge
co <- full_join(co_dis, co_temp, by = c("staid", "dates")) %>% 
  mutate(river = "CO")
  
# Bind into complete dataset
water <- bind_rows(co, gr) %>% 
  select(-c(contains("qual"), staid)) %>% 
  rename(date = dates) %>% 
  mutate(date = as.character(date))

readr::write_rds(water, "./data/ismp_water.Rds")

## End
