library(dplyr)
library(lubridate)

# no date info. but we can look potentially at spatial shifts by
# individual / fleets
akfin <- readRDS("Data/subset_pfxcommercial_cleaned_allyears.rds")
akfin$year = as.numeric(akfin$year)
#akfin = dplyr::filter(akfin, area %in% c("BSAI","GOA","INSD"))
# rename variables to match kate's
fields = read.csv("fields_in_kr_data.csv")
akfin = dplyr::rename(akfin, sector2 = p_fshy,
                      drvid = p_holder,
                      r_port = port,
                      r_date = date,
                      set_lat = lat,
                      set_long = lon,
                      ret_mt = g_pounds)
akfin$data_source = "AKFIN"
akfin$set_date = akfin$r_date
akfin$gear = akfin$sector2 # gear info is in permit
akfin$trip_id = NA
akfin$haul_id = NA

akfin$month = substr(akfin$r_date,5,6)
akfin$day = substr(akfin$r_date,7,8)
akfin$j_set_day = lubridate::yday(lubridate::parse_date_time(paste0(akfin$year,akfin$month, akfin$day), "ymd"))

#akfin = akfin[,which(names(akfin) %in% fields$column_name)]

# spatial strata ("subarea"): WG, CG, WY, SE and SEI / BS and AI separate question?
akfin = dplyr::filter(akfin, subarea %in% c("WG","CG","WY","SE","SEI"))
akfin$area = akfin$subarea

# create longline sector
akfin$sector = NA
akfin$sector[which(akfin$sector2 %in% c("B 06B","B 61B","C 06B","C 61B"))] = "longline"
# create pelagic trawl sector (pollock)
akfin$sector[which(akfin$sector2 %in% c("B 06B","B 61B","C 06B","C 61B")==FALSE & akfin$gear_code==47)] = "pelagic trawl"

# separate out bottom trawl by arrowtooth (mgmt code != RPP) and rockfish (RPP)
# problem is that the RPP code didn't exist before 2007

# rock = dplyr::filter(akfin, program=="RPP", gear_code == "07", year>2007,
#                      sector2 %in% c("B 06B","B 61B","C 06B","C 61B")==FALSE)

rock = dplyr::filter(akfin, sector2 %in% c("B 06B","B 61B","C 06B","C 61B")==FALSE,
                     gear_code == "07")
rock$rockfish = ifelse(rock$specn%in% c(135:185), 1, 0)
trips = dplyr::group_by(rock, r_date, drvid) %>%
  dplyr::summarize(non_rock = sum(ret_mt[which(rockfish==0)]),
                   rock = sum(ret_mt[which(rockfish==1)]),
                   p_rock = rock / (rock+non_rock),
                   trip_type = ifelse(p_rock > 0.5,"rockfish","misc. groundfish")) %>%
  dplyr::select(-rock, -non_rock, -p_rock)
# join in trhe trip_type
akfin = dplyr::left_join(akfin, trips)
akfin$sector[which(akfin$trip_type=="rockfish")] = "rockfish"
akfin$sector[which(akfin$trip_type=="misc. groundfish")] = "misc. groundfish"

akfin$sector2 = akfin$sector
akfin = dplyr::select(akfin, -sector)
# hal/sablefish: longliners only
# C61B / B61B
# look at map of 61B and 06B to see if we can combine
# pots recently approved in gulf for example w/depradation
# drop B 26B and M 05B -- maybe not used anymore and small #s

# summary = dplyr::filter(d, sector == "longline") %>%
#   dplyr::mutate(set_long = 5*floor(set_long/5),
#                 set_lat = floor(set_lat)) %>%
#   dplyr::group_by(sector2, year, set_long, set_lat) %>%
#   dplyr::summarize(s = sum(ret_mt, na.rm=T))
#
#   pdf("plots by area.pdf")
#   ggplot(dplyr::filter(summary, sector2=="B 06B"), aes(set_long,set_lat,fill=log(s))) +
#     geom_tile() +
#     facet_wrap(~year) +
#     ggtitle("B 06B")
#   ggplot(dplyr::filter(summary, sector2=="C 06B"), aes(set_long,set_lat,fill=log(s))) +
#     geom_tile() +
#     facet_wrap(~year) +
#     ggtitle("C 06B")
#   ggplot(dplyr::filter(summary, sector2=="B 61B"), aes(set_long,set_lat,fill=log(s))) +
#     geom_tile() +
#     facet_wrap(~year) +
#     ggtitle("B 61B")
#   ggplot(dplyr::filter(summary, sector2=="C 61B"), aes(set_long,set_lat,fill=log(s))) +
#     geom_tile() +
#     facet_wrap(~year) +
#     ggtitle("C 61B")
# dev.off()

# management code for rockfish pilot program (RPP).
# ADFG_H_MGT_PROGRAM_ID contains RPP
# 2 main bottom trawl fisheries in gulf. M permits have 2 gear
# types: pelagic (pollock) and bottom trawl (rockfish RPP or arrowooth).
# Arrowtooth would be bottom trawl != RPP
# d$ADFG_H_GEAR_CODE = 7 = bottom trawl
# d$ADFG_H_GEAR_CODE = 47

# what to do pre-2007 RPP rationalization.
# group by fish landed date and vessel id --
# look at % of rockfish (K ) species vs other -- for estimates >
# 50% we'll call it a rockfish trip and keep it.



saveRDS(akfin, "Data/subset_pfxcommercial_cleaned_allyears_renamed.rds")
