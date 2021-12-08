library(dplyr)
library(lubridate)

# no date info. but we can look potentially at spatial shifts by
# individual / fleets
akfin <- readRDS("Data/subset_pfxcommercial_cleaned_allyears.rds")
akfin$year = as.numeric(akfin$year)
akfin = dplyr::filter(akfin, area %in% c("BSAI","GOA","INSD"))
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

saveRDS(akfin, "Data/subset_pfxcommercial_cleaned_allyears_renamed.rds")