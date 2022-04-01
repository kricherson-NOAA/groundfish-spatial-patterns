#Setting up data: combining all "groundfish" sectors (i.e. sectors I think entirely or mostly target groundfish)
#COMBINING OB, EM, ASHOP for now - need to be conscious that these data sources are all a bit different.
#This is run on Tantalus and saved there, can then be pulled down locally. 

#/opt/R/64-bit/R-4.0.1/bin/R

library(janitor)
library(tidyverse)
library(lubridate)

source("~/observer/Input/load_data_2021-09-30.R")

load_data(c("WCGOP_proc_full", "EM_proc", "ASHOP_proc", "FT_proc"))


#Pulling out fields we MAY want at some point - some of these probably not needed
ob_gf <- OBOrig_Proc_Full %>% 
  clean_names() %>% 
  filter(!is.na(haul_id)) %>% 
  filter(sector %in% c("Limited Entry Trawl", 
                       "Catch Shares",
                       "Midwater Hake",
                       "Midwater Rockfish",
                       "Shoreside Hake",
                       "OA Fixed Gear",
                       "Limited Entry Sablefish",
                       "LE Fixed Gear DTL",
                       "Nearshore")) %>% 
  select(-gear) %>% 
  mutate(gear = gear_2) %>% 
  mutate(sector2 = ifelse(sector %in% c("Limited Entry Trawl", "Catch Shares") &
                            gear %in% c("Bottom Trawl", "Midwater Trawl"), "LE/CS Trawl", sector)) %>% 
  mutate(sector2 = ifelse(sector == "Catch Shares" &
                            !gear %in% c("Bottom Trawl", "Midwater Trawl"), "CS Fixed Gear", sector2)) %>% 
  mutate(sector2 = ifelse(grepl("Hake", sector), "Shoreside/midwater Hake", sector2)) %>% 
  dplyr::select(year, sector, sector2, drvid, trip_id, gear, d_port, d_date, d_state, r_port, pcid, r_date, r_state, haul_id, set_date, up_date, set_lat, set_long, up_lat, up_long, avg_lat, avg_long, avg_depth, haul_duration, spid_eqv, scientific_name, mt, ret_mt, dis_mt, gfr_mt, pwht_mt, sabl_mt, tgt_mt, pcid) %>% 
  mutate(data_source = "WCGOP") %>% 
  mutate(haul_id = as.character(haul_id)) %>% 
  mutate(trip_id = as.character(trip_id)) %>% 
  mutate(drvid = as.character(drvid))


em_gf<- EMOrig_Proc %>% 
  clean_names() %>% 
  filter(!is.na(haul_id)) %>% 
  mutate(sector2 = ifelse(sector == "Midwater Rockfish EM", "Midwater Rockfish", NA),
         sector2 = ifelse(sector == "Midwater Hake EM", "Shoreside/midwater Hake", sector2),
         sector2 = ifelse(sector == "Catch Shares EM" & gear %in% c("Bottom Trawl", "Midwater Trawl") , "LE/CS Trawl", sector2)) %>% 
  mutate(sector2 = ifelse(sector == "Catch Shares EM" &
                            !gear %in% c("Bottom Trawl", "Midwater Trawl"), "CS Fixed Gear", sector2)) %>% 
  select(year, sector, sector2, drvid, trip_id = emtrip_id, gear, d_port, d_date, d_state, r_port, pcid = pacfin_port_code, r_date, r_state, haul_id, set_date, up_date, set_lat, set_long, up_lat, up_long, avg_lat, avg_long, avg_depth, haul_duration, spid_eqv = spid, scientific_name = sci_name, mt, ret_mt, dis_mt, gfr_mt, pwht_mt, sabl_mt, tgt_mt) %>% 
  mutate(data_source = "EM") %>% 
  mutate(haul_id = as.character(haul_id)) %>% 
  mutate(trip_id = as.character(trip_id)) %>% 
  mutate(drvid = as.character(drvid))


ashop_gf <- ASOrig_Proc %>% 
  clean_names() %>% 
  filter(!is.na(haul_id)) %>% 
  mutate(sector2 = ifelse(sector == "CATCHER-PROCESSOR", "At-sea hake CP", "At-sea hake MS")) %>% 
  select(year, sector, sector2, drvid,  gear, haul_id, set_date = deployment_date, up_date = retrieval_date, set_lat = latdd_start, set_long = londd_start, up_lat = latdd_end, up_long = londd_end, avg_lat, avg_long, avg_depth = depth, duration_in_min, spid_eqv, scientific_name, ret_mt, dis_mt, gfr_mt, tgt_mt) %>% 
  mutate(mt = dis_mt + ret_mt) %>% 
  mutate(haul_duration = duration_in_min/60) %>%
  select(-duration_in_min) %>% 
  mutate(data_source = "ASHOP") %>% 
  mutate(drvid = as.character(drvid)) %>% 
  mutate(r_port = "AT SEA - NO RETURN PORT")


gf <- bind_rows(ob_gf, em_gf, ashop_gf)

#we probably need the data on the haul, rather than species/group level
#There is a small but non-zero possibility that a haul or trip ID could be repeated across different data sources, so I am appending with the data source to be sure they are unique identifiers
gf_haul0 <- gf %>% 
  group_by(data_source, year, sector2, drvid, gear, r_port, r_date, trip_id, haul_id, set_date, set_lat, set_long) %>% 
  summarise(ret_mt= sum(ret_mt, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(j_set_day = yday(set_date)) %>% 
  mutate(haul_id = paste0(data_source, haul_id),
         trip_id = ifelse(data_source == "ASHOP", NA, paste0(data_source, trip_id))) %>% 
  mutate(r_port = ifelse(r_port == "WARRENTON", "ASTORIA / WARRENTON", r_port))

#Add in return port lat/long
r_ports <- read_csv("~/observer/Input/Richerson/iopac_ob_conversion_table.csv") %>% 
  clean_names() %>% 
  select(r_port_lat = pacfin_lat, r_port_long = pacfin_long, r_port) %>% 
  filter(!is.na(r_port))

gf_haul <- gf_haul0 %>% 
  left_join(r_ports, by = "r_port") %>% 
  mutate(area = ifelse (r_port_lat >= 40 + 1/6, "north", "south"),
         area = ifelse(r_port == "AT SEA - NO RETURN PORT", "at sea", area))

nrow(gf_haul) == nrow(gf_haul0)

saveRDS(gf_haul, "~/observer/Input/Richerson/groundfish_spatial/gf_haul.rds")

fields <- data.frame(column_name = names(gf_haul), 
                     column_class = unlist(lapply(gf_haul, function(column) {class(column)[1]})),
                     definition = c("Source of data (WCGOP, ASHOP, or EM)",
                                    "Year of vessel return to port",
                                    "Aggregated sector name, currently combines electronically-monitored and observed data",
                                    "Vessel ID (Coast Guard documentation number or state registration number of vessel)",
                                    "Gear name",
                                    "Port of return (NA for at-sea sectors)",
                                    "Date of return (NA for at-sea sectors)",
                                    "Unique trip identifier (NA for at-sea sectors)",
                                    "Unique haul identifier",
                                    "Set date",
                                    "Set latitude (decimal degrees)",
                                    "Set longitide (decimal degrees)",
                                    "Retained catch (metric tons)",
                                    "Julian set day",
                                    "Latitude of return port",
                                    "Longitude of return port",
                                    "Return port is north or south of 40 10"),
                     currently_used = c(F, 
                                        T, 
                                        T, 
                                        T, 
                                        F, 
                                        T, 
                                        F, 
                                        T, 
                                        T, 
                                        T, 
                                        T, 
                                        T, 
                                        T, 
                                        T, 
                                        T,
                                        T,
                                        T)
                     ) #set_date class is actually "POSIXct" "POSIXt" but since the former inherits from the latter I think this works

write_csv(fields, "~/observer/Input/Richerson/groundfish_spatial/fields_in_kr_data.csv")

#Let's also output the FT data, grouped by FT. We want to be able to. Let's also sort by north/south, as above, and include port names that match with observer data

#Add in return port lat/long
r_ports2 <- read_csv("~/observer/Input/Richerson/iopac_ob_conversion_table.csv") %>% 
  clean_names() %>% 
  select(r_port, pacfin_port_code) %>% 
  filter(!is.na(r_port))

ft <- FTOrig_Proc %>% 
  clean_names() %>% 
  filter(sector %in% c("Limited Entry Trawl", 
                       "Catch Shares",
                       "Midwater Hake",
                       "Midwater Rockfish",
                       "Shoreside Hake",
                       "OA Fixed Gear",
                       "Limited Entry Sablefish",
                       "LE Fixed Gear DTL",
                       "Nearshore")) %>% 
  mutate(sector2 = ifelse(sector %in% c("Limited Entry Trawl", "Catch Shares") &
                            gear %in% c("Bottom Trawl", "Midwater Trawl"), "LE/CS Trawl", sector)) %>% 
  mutate(sector2 = ifelse(sector == "Catch Shares" &
                            !gear %in% c("Bottom Trawl", "Midwater Trawl"), "CS Fixed Gear", sector2)) %>% 
  mutate(sector2 = ifelse(grepl("Hake", sector), "Shoreside/midwater Hake", sector2)) %>% 
  group_by(year, agency_code, sector, sector2, drvid, gear, landing_date, pacfin_port_code, latitude_port, ftid) %>% 
  summarize(mt = sum(mt),
            tgt_mt = sum(tgt_mt),
            gfr_mt = sum(gfr_mt),
            exvessel_revenue = sum(exvessel_revenue)) %>% 
  mutate(data_source = "FT") %>% 
  mutate(drvid = as.character(drvid)) %>% 
  ungroup() %>% 
  mutate(area = ifelse (latitude_port >= 40 + 1/6, "north", "south")) %>% 
  left_join(r_ports2, by = "pacfin_port_code") %>% 
  mutate(j_landing_day = yday(landing_date)) 
  

saveRDS(ft, "~/observer/Input/Richerson/groundfish_spatial/ft.rds")



  