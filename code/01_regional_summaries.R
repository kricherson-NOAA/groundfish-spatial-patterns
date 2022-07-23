library(dplyr)
library(ggplot2)
library(zoo)
library(Hmisc)
library(lubridate)
library(ggridges)
library(sinkr) #Has a function to calculate distance in km between two points
library(raster)
library(maps)
library(mapproj)
library(viridis)
library(tidyr)

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- c("Alaska", "WC")[2]
scale = c("region","port")[2]
n_top_ports <- 50

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

#Use only individuals fishing before/after catch shares (FALSE includes all)?
cs_sensitivity <- FALSE


#Include all vessels, or just those that fish out of one port?
include_all_vessels <- TRUE

#Which WC sectors to include?
wc_sectors <- c("LE/CS Trawl",
                "OA Fixed Gear",
                "Limited Entry Sablefish",
                "LE Fixed Gear DTL",
                "Nearshore",
                "At-sea hake CP",
                "At-sea hake MS",
                "CS Fixed Gear",
                "Shoreside/midwater Hake")[c(1,3,6,7)]

if(data == "Alaska") {
  d <- readRDS("Data/subset_pfxcommercial_cleaned_allyears_renamed.rds")
  # bring in ports
  port_locs = read.csv("Data/usharbour.csv") %>%
    dplyr::rename(r_port_long = lon, r_port_lat = lat, r_port = port) %>%
    dplyr::select(r_port, r_port_long, r_port_lat)
  d = dplyr::left_join(d, port_locs)

} else {

  d <- readRDS("Data/gf_haul.rds") %>%
    dplyr::filter(sector2 %in% wc_sectors) %>%
    # dplyr::mutate(sector2 = ifelse(grepl("At-sea hake", sector2),
    #                                "At-sea hake",
    #                                sector2)) %>%  #Combine at-sea CP and MS for now
    mutate(subarea = area) #Not sure if this is the best way to match up with AK data - placeholder for now

  ft <- readRDS("Data/ft.rds") %>%
    dplyr::filter(sector2 %in% wc_sectors) %>%
    mutate(subarea = area) #Not sure if this is the best way to match up with AK data - placeholder for now

  if(split_wc == "one_area")
  {
    d$area <- "WC"
    ft$area <- "WC"
  }

}


#create a label to append file names with whether we subset only to vessels present both before and after CS ("stayers")
if(cs_sensitivity)
{
  cs_sens_label = "stayers"
}else{
  cs_sens_label = "allvessels"
}


# Switch for making plots by port or area
if(scale=="port") {
  d$v <- d$r_port
  # if using ports, put together list of top ports by ret_mt
  port_list <- dplyr::group_by(d, r_port) %>%
    dplyr::summarize(sum_mt = sum(ret_mt)) %>% dplyr::arrange(-sum_mt)
  # remove number codes
  port_list = port_list[which(is.na(as.numeric(port_list$r_port))),]
  port_list = port_list$r_port[1:n_top_ports]

  #On the west coat, there's only 34 ports total, so remove NAs that get generated
  port_list <- port_list[!is.na(port_list)]

  ft$v <- ft$r_port

  #If we DON'T want to restrict analysis to only vessels landing in one port, let's define v as the port that vessels land in most often in a given year/sector, according to fish tickets (their "home port")

  if(include_all_vessels)
  {

    home_ports <- ft %>%
      #summarize one "landing" (ie same port, vessel, and day)
      group_by(year, sector2, drvid, landing_date, r_port) %>%
      summarise(total_mt = sum(mt)) %>%
      group_by(drvid, sector2, year, r_port) %>%
      summarise(n = n()) %>% #could also use highest weight or highest revenue....
      group_by(drvid, sector2, year) %>%
      slice(which.max(n)) %>%
      rename(home_port = r_port) %>%
      ungroup()

    #Join home port derived from to observer data, remove the very small number of observer data rows that did not have a match in the fish ticket data (<1%)
    d <- d %>%
      left_join(home_ports, by =c("drvid", "sector2", "year")) %>%
      mutate(home_port = ifelse(sector2 %in% c("At-sea hake CP", "At-sea hake MS", "At-sea hake"), r_port, home_port)) %>%
      filter(!is.na(home_port)) %>%
      dplyr::select(-v, -n) %>%
      rename(v = home_port)

    #Also change the ft data so that v = home port
    ft <- ft %>%
      left_join(home_ports, by =c("drvid", "sector2", "year")) %>%
      dplyr::select(-v, -n) %>%
      rename(v = home_port)

  }

} else {
  d$v <- d$area
  port_list = unique(d$r_port)
  ft$v <- v$area
}


# Find single port fishers if only include individuals who fish in a single port in a year
d$port_fisher = paste(d$year, d$drvid)
port_fisher = dplyr::group_by(d, year, drvid) %>%
  dplyr::summarize(n = length(unique(r_port)),
                   port_fisher = paste(year, drvid))
# break the dplyr commands here to output summary stats of ports per fisher
port_fisher$yearf <- as.factor(port_fisher$year)

#remove at-sea hake from this plot, since they will always (misleadingly) "land" in one port
port_fisher_nohake <- dplyr::group_by(d %>% filter(!grepl("hake", sector2)), year, drvid) %>%
  dplyr::summarize(n = length(unique(r_port)),
                   port_fisher = paste(year, drvid)) %>% 
  mutate(yearf = as.factor(year))

g <- ggplot(port_fisher_nohake, aes(x = n, y = yearf)) +
  geom_density_ridges(scale = 2, col=viridis(1), fill=viridis(1), alpha=0.3) +
  coord_flip() +
  coord_cartesian(xlim=c(0.7,5.5)) +
  theme_bw() + xlab("Ports per fisher") +
  ylab("Year") +
  scale_y_discrete(expand = expansion(add = c(0.1, 2)))
ggsave(g, filename = paste0("figures/Ports_per_fisher_",data,".png"),
       height = 6, width = 6)

# port_fisher is a data frame with IDs-years of single port fishers
port_fisher <- port_fisher %>%
  dplyr::filter(n == 1)

#I get an error about "$ operator is invalid for atomic vectors" when filtering to data to port_fisher %in% port_fisher$port_fisher, this is a workaround -KR
single_port_fishers <- port_fisher$port_fisher

#do the same for WC fish ticket data
if (data == "WC")
{
  ft$port_fisher = paste(ft$year, ft$drvid)
  port_fisher_ft = dplyr::group_by(ft, year, drvid) %>%
    dplyr::summarize(n = length(unique(pacfin_port_code)),
                     port_fisher_ft = paste(year, drvid)) %>%
    dplyr::filter(n == 1)

  single_port_fishers_ft <- port_fisher_ft$port_fisher_ft

} else {
  # drop ports associated with floating processors
  d = dplyr::filter(d, v %in% c("UNK","FLP","IFP","FLD")==FALSE)
  d$keep = 0
  # keep combos with 15+ years of data
  d$keep[which(d$sector2=="longline" & d$r_port %in% c("AKU","ALI",
                                                        "COR","CRG","DUT",
                                                        "HNH","HOM","JNU",
                                                        "KCO","KOD","KTN",
                                                        "PBG","PEL","SEW","SIT","VAL",
                                                        "WRN","YAK"))] = 1
  # rockfish in kodiak
  d$keep[which(d$sector2 == "rockfish" & d$r_port == "KOD")] = 1
  # groundfish, pelagic trawl in: KCO, KOD, SPT
  d$keep[which(d$sector2 == "pelagic trawl" & d$r_port %in% c("KCO","KOD","SPT"))] = 1
  d$keep[which(d$sector2 == "misc. groundfish" & d$r_port %in% c("KCO","KOD","SPT"))] = 1
  d = dplyr::filter(d,keep==1)
}

# filter all records to only use single port fishers?
if(include_all_vessels)
{
  d = dplyr::filter(d, r_port %in% port_list)

}else{

  d = dplyr::filter(d, port_fisher %in% single_port_fishers,
                    r_port %in% port_list)
}

# For Supplementary analysis for paper, we're also interested in
# a sensitivity looking at catch share effects for only the fishers who
# were active pre / post catch shares. This is a little tricky because the
# time of catch shares varies by sector -- so not sure of best way to code
if(cs_sensitivity == TRUE) {
  if (data == "Alaska") {
    # filter only folks active for 4+ years before/after catch shares.
    # 4 years seems arbitrary, but it's the minimum we can use for AK longline -
    # data starts in 1991 and CS happens in 1995
    grp_1 <- dplyr::filter(d, sector2 == "rockfish") %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 2005)),
                       n_post = length(which(unique(year) >= 2005))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)

    grp_2 <- dplyr::filter(d, sector2 == "longline") %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 1995)),
                       n_post = length(which(unique(year) >= 1995))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)
  } else{
    # filter only folks active for 5+ years before/after catch shares - note this takes our sample size from 232 vessels to 60
    grp_1 <- dplyr::filter(d, sector2 == "LE/CS Trawl") %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 2011)),
                       n_post = length(which(unique(year) >= 2011))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)

    grp_2 <- dplyr::filter(d, grepl("At-sea hake", sector2)) %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 2011)),
                       n_post = length(which(unique(year) >= 2011))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)
  }
  d <- dplyr::filter(d, drvid %in% c(grp_1$drvid, grp_2$drvid))
}

# plot ports available by sector (and region?)
port_df <- dplyr::group_by(d, sector2, subarea, year) %>%
  dplyr::summarize(n_port = length(unique(r_port)))
saveRDS(port_df, paste0("data/",data,"_",scale,"_ports",".rds"))


# do coarse summaries by area and year
area_cog <-
  dplyr::group_by(d, v, year) %>%
  dplyr::summarize(sum_mt = sum(ret_mt,na.rm=T),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2),
                   individuals="Ind. ignored",
                   total_cv = total_sd / mean(lat_sd^2 + long_sd^2))

# do same -- but vessel/individual averages
area_cog_ind <- dplyr::group_by(d, v, drvid, year) %>%
  dplyr::summarize(n = n(),
                   sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2)) %>%
  dplyr::filter(!is.na(lat_mean+lon_mean+lat_sd+long_sd)) %>%
  dplyr::group_by(v, year) %>%
  dplyr::summarize(sum_mt = mean(sum_mt),
                     lat_mean = mean(lat_mean),
                   lon_mean = mean(lon_mean),
                   lat_sd = mean(lat_sd),
                   long_sd = mean(long_sd),
                   total_sd = mean(total_sd),
                   total_cv = sd(total_sd) / total_sd,
                   individuals="Ind. averaged")

# instead of just stratifying by area, do area + permit (sector2)
area_permit_cog <- dplyr::group_by(d, v, sector2, year) %>%
  dplyr::summarize(n = n(),
                   sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2),
                   individuals="Ind. ignored",
                   subarea = subarea[1],
                   total_cv = total_sd/mean(lat_sd^2 + long_sd^2))

# do same -- but vessel/individual averages
area_permit_cog_ind <- dplyr::group_by(d, v, sector2, drvid, year) %>%
  dplyr::summarize(n = n(),
                   subarea = subarea[1],
                   sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2)) %>%
  dplyr::filter(!is.na(lat_mean+lon_mean+lat_sd+long_sd)) %>%
  #dplyr::group_by(area_permit_cog_ind,v, year, sector2) %>%
  dplyr::group_by(v, year, sector2) %>% #KR edit: commented out line above and added this line b/c otherwise I get an error
  dplyr::summarize(n_fishtickets = sum(n),
                   n = n(),
                   sum_mt = mean(sum_mt),
                   lat_mean = mean(lat_mean),
                   lon_mean = mean(lon_mean),
                   lat_sd = mean(lat_sd),
                   long_sd = mean(long_sd),
                   total_cv = sd(total_sd)/mean(total_sd),
                   total_sd = mean(total_sd),
                   individuals="Ind. averaged",
                   subarea = subarea[1])

#Calculating distance from return port. For WC, this is the haul distance from port
#First, taken over all observations
haul_dist <-
  dplyr::group_by(d, v, sector2, year, haul_id) %>%
  dplyr::summarise(port_dist = sinkr::earthDist(set_long, set_lat, r_port_long, r_port_lat),
                   subarea = subarea[1],
                   mt = sum(ret_mt)) %>%
  dplyr::group_by(v, sector2, year) %>%
  dplyr::summarise(n = n(),
                   mean_haul_dist = wtd.mean(port_dist, mt, na.rm = T),
                   total_cv = sd(port_dist, na.rm = T)/mean_haul_dist,
                   subarea = subarea[1])

#Second, taken over individuals/vessels, then across port/sector
haul_dist_ind <- dplyr::group_by(d, v, sector2, year, drvid, haul_id) %>%
  dplyr::summarise(port_dist = sinkr::earthDist(set_long, set_lat, r_port_long, r_port_lat),
                   subarea = subarea[1],
                   mt = sum(ret_mt)) %>%
  dplyr::group_by(v, sector2, year, drvid) %>%
  dplyr::summarise(mean_haul_dist = wtd.mean(port_dist, mt, na.rm = T),
                   subarea = subarea[1]) %>%
  dplyr::group_by(v, sector2, year) %>%
  dplyr::summarise(n = n(),
                   mean_haul_dist = mean(mean_haul_dist, na.rm = T),
                   total_cv = sd(mean_haul_dist, na.rm = T)/mean_haul_dist,
                   subarea = subarea[1])

# also add calculations for the effective number of days fished. This
# follows from the portfolio work we've done previously (Anderson et al. PNAS,
# Ward et al. J Appl Ecol) -- the formula is Effective[x] = sum(p_i^2),
# where p_i is the proportion of landings for any individual fisher
# this is calculated separately by person and year, and then averaged across people
season_eff <- dplyr::group_by(d, year, v, sector2,j_set_day) %>%
  dplyr::summarise(ret = sum(ret_mt), sector2=sector2[1],
                   v = r_port[1],
                   subarea = subarea[1]) %>%# calculate sum by person-day-year
  dplyr::group_by(year, v, sector2) %>%  # sum across days
  dplyr::mutate(ret_yr = sum(ret), p = ret/ret_yr) %>%# normalize
  dplyr::group_by(year, v, sector2,subarea) %>%
  dplyr::summarize(n = n(), effective_days = 1/(sum(p^2)))

season_eff_ind <- dplyr::group_by(d, year, drvid, j_set_day) %>%
  dplyr::summarise(ret = sum(ret_mt), sector2=sector2[1],
                   v = r_port[1],
                   subarea = subarea[1]) %>%# calculate sum by person-day-year
  dplyr::group_by(year, drvid) %>%  # sum across days
  dplyr::mutate(ret_yr = sum(ret), p = ret/ret_yr,
                effective_days = 1/(sum(p^2))) %>% # normalize
  dplyr::group_by(year, v, sector2,subarea) %>%
  dplyr::summarize(n = n(), effective_days = mean(effective_days))

#For the shoreside west coast sectors, I think we should use fish ticket data to calculate effective days fished (or days landed). I'm not confident that partial observer coverage might not lead to biases in results (selection is *supposed* to be spatially representative, but I'm not sure about *temporally* representative)
if(data == "WC")
{
  # filter all records to only use single port fishers?
  if(include_all_vessels)
  {

    ft = dplyr::filter(ft, r_port %in% port_list)

  }else{

    ft = dplyr::filter(ft, port_fisher %in% single_port_fishers,
                       r_port %in% port_list)
  }
  
  if(cs_sensitivity)
  {
    # filter only folks active for 5+ years before/after catch shares
    ft_grp_1 <- dplyr::filter(ft, sector2 == "LE/CS Trawl") %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 2011)),
                       n_post = length(which(unique(year) >= 2011))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)
    
    ft_grp_2 <- dplyr::filter(ft, grepl("At-sea hake", sector2)) %>%
      dplyr::group_by(drvid) %>%
      dplyr::summarize(n_pre = length(which(unique(year) < 2011)),
                       n_post = length(which(unique(year) >= 2011))) %>%
      dplyr::filter(n_pre >= 4, n_post >= 4)
    
    ft <- dplyr::filter(ft, drvid %in% c(ft_grp_1$drvid, ft_grp_2$drvid))
    
  }


  season_eff_ft <- dplyr::group_by(ft, year, v, sector2, j_landing_day) %>%
    dplyr::summarise(ret = sum(mt), sector2=sector2[1],
                     v = r_port[1],
                     subarea = subarea[1]) %>%# calculate sum by person-day-year
    dplyr::group_by(year, v, sector2) %>%  # sum across days
    dplyr::mutate(ret_yr = sum(ret), p = ret/ret_yr) %>%# normalize
    dplyr::group_by(year, v, sector2,subarea) %>%
    dplyr::summarize(n = n(), effective_days = 1/(sum(p^2)))

  season_eff_ind_ft <- dplyr::group_by(ft, year, drvid, j_landing_day) %>%
    dplyr::summarise(ret = sum(mt), sector2=sector2[1],
                     v = r_port[1],
                     subarea = subarea[1]) %>%# calculate sum by person-day-year
    dplyr::group_by(year, drvid) %>%  # sum across days
    dplyr::mutate(ret_yr = sum(ret), p = ret/ret_yr,
                  effective_days = 1/(sum(p^2))) %>% # normalize
    dplyr::group_by(year, v, sector2,subarea) %>%
    dplyr::summarize(n = n(), effective_days = mean(effective_days))

  #Add on at-sea effective days fished, then save this for further analysis
  season_eff <- filter(season_eff, grepl("At-sea hake", sector2)) %>%
    bind_rows(season_eff_ft)

  season_eff_ind <- filter(season_eff_ind, grepl("At-sea hake", sector2)) %>%
    bind_rows(season_eff_ind_ft)
}

# saving the above dataframes as objects to run models on -- EW
saveRDS(area_permit_cog, paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog",".rds"))
saveRDS(area_permit_cog_ind, paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog-ind",".rds"))
saveRDS(haul_dist, paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist",".rds"))
saveRDS(haul_dist_ind, paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist-ind",".rds"))
saveRDS(season_eff, paste0("data/",data,"_",cs_sens_label,"_",scale,"_days",".rds"))
saveRDS(season_eff_ind, paste0("data/",data,"_",cs_sens_label,"_",scale,"_days-ind",".rds"))

p1 <- ggplot(area_cog, aes(lon_mean, lat_mean,col=year)) +
  geom_point(alpha=0.6) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ v, scale="free") +
  ggtitle("Individuals ignored") +
  scale_color_viridis_c(end=0.8)

p2 <- ggplot(area_cog_ind, aes(lon_mean, lat_mean,col=year)) +
  geom_point(alpha=0.6) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ v, scale="free") +
  ggtitle("Individual averages") +
  scale_color_viridis_c(end=0.8)

p3 <- gridExtra::grid.arrange(p1,p2)

# same by permit -- ignoring indviduals
p4 <- ggplot(area_permit_cog, aes(lon_mean, lat_mean,col=year)) +
  geom_point(alpha=0.6) +
  xlab("Longitude") + ylab("Latitude") +
  facet_grid(sector2 ~ v, scale ="free") +
  ggtitle("Individuals ignored") +
  scale_color_viridis_c(end=0.8)

p5 <- ggplot(area_permit_cog_ind, aes(lon_mean, lat_mean,col=year)) +
  geom_point(alpha=0.6) +
  xlab("Longitude") + ylab("Latitude") +
  facet_grid(sector2 ~ v, scale="free") +
  ggtitle("Individual averages") +
  scale_color_viridis_c(end=0.8)

# trends in variability
p6 <- ggplot(area_cog, aes(year, total_sd)) +
  geom_line() +
  xlab("Year") +
  ylab("Sqrt inertia") +
  ggtitle("Individuals ignored") +
  facet_wrap(~v, scale="free_y")

p7 <- ggplot(area_cog_ind, aes(year, total_sd)) +
  geom_line() +
  xlab("Year") +
  ylab("Sqrt inertia") +
  ggtitle("Individual averages") +
  facet_wrap(~v, scale="free_y") #

p8 <- ggplot(area_permit_cog, aes(year, total_sd)) +
  geom_line() +
  xlab("Year") +
  ylab("Sqrt inertia") +
  ggtitle("Individuals ignored") +
  facet_grid(sector2~v, scale="free_y")

p9 <- ggplot(area_permit_cog_ind, aes(year, total_sd)) +
  geom_line() +
  xlab("Year") +
  ylab("Sqrt inertia") +
  ggtitle("Individual averages") +
  facet_grid(sector2~v, scale="free_y")


# also summarize seasonal trends. probably several as to do this -- this
# way generates random samples of calendar day, by year

# we could use geom_density_ridges and pass in raw data - but then every
# fish ticket date would weight equally. Here, I'll generate a random sample
# maintaining baseic data structure (e.g. variable effort by year) but by
# weighting by retained catch, we're giving more weight to those
d$id = seq(1,nrow(d))
set.seed(123)
d_sample = sample(d$id, size=nrow(d), replace=T, prob = d$ret_mt)

p10 <- ggplot(dplyr::filter(d[d_sample,], r_port %in% port_list), aes(j_set_day, year, group=year)) +
  geom_density_ridges(fill=viridis(1), alpha=0.3, col=viridis(1)) +
  xlab("Calendar day") +
  ylab("Year") +
  theme_bw() +
  coord_cartesian(xlim=c(0,366))

ggsave(p10, filename = paste0("figures/Calendar_day_ridges_",data,"_",cs_sens_label,".png"),
       height = 6, width = 6)

#Also plot by sector
p11 <- ggplot(dplyr::filter(d[d_sample,], r_port %in% port_list), aes(j_set_day, year, group=year)) +
  geom_density_ridges(fill=viridis(1), alpha=0.3, col=viridis(1)) +
  xlab("Calendar day") +
  ylab("Year") +
  #ggtitle("Distribution of landings")+
  facet_wrap(~sector2, scales = "free")+
  theme_bw() +
  coord_cartesian(xlim=c(0,366))

ggsave(p11, filename = paste0("figures/Calendar_day_ridges_sector_",data,"_",cs_sens_label,".png"),
       height = 6, width = 6)


plot_list <- list()
sub = dplyr::filter(d[d_sample,], r_port %in% port_list)
for(i in 1:length(unique(sub$v))) {
  plot_list[[i]] <- ggplot(dplyr::filter(sub, v == unique(sub$v)[i]), aes(j_set_day, year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle(paste0("Distribution of landings: ",unique(sub$v)[i]))
}

#For WC, let's also plot fish ticket landings since partial observer coverage may make seasonal patterns harder to discern
if (data == "WC")
{
  ft$id = seq(1,nrow(ft))
  ft_sample = sample(ft$id, size=nrow(ft), replace=T, prob = ft$mt)

  #Note: not filtering by port for now
  p10_ft <- ggplot(ft[ft_sample,], aes(yday(landing_date), year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle("Distribution of landings (fish tickets)")


  p11_ft <- ggplot(ft[ft_sample,], aes(yday(landing_date), year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle("Distribution of landings (fish tickets)")+
    facet_grid(area~sector2, scales = "free")

  plot_list_ft <- list()
  sub = ft[ft_sample,] %>%
    dplyr::mutate(j_landing_date = yday(landing_date))

  for(i in 1:length(unique(sub$area))) {
    plot_list_ft[[i]] <- ggplot(dplyr::filter(sub, area == unique(sub$area)[i]), aes(j_landing_date, year, group=year)) +
      geom_density_ridges() +
      xlab("Calendar day") +
      ylab("Year") +
      ggtitle(paste0("Distribution of landings (fish tickets): ",unique(sub$area)[i]))
  }
}



# aggregate by year and day
# write.csv(dplyr::group_by(dplyr::filter(sub, v == unique(sub$v)[i]), j_set_day, year) %>%
#   dplyr::summarise(tot = sum(ret_mt)), "demo.csv")

#Distance to port plots
p12 <- ggplot(dplyr::filter(haul_dist, v != "at sea"), aes(year, mean_haul_dist, color = sector2)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean haul dist to port (km)") +
  ggtitle("Individuals ignored") +
  facet_grid(sector2~v, scale="free_y")

p13 <- ggplot(dplyr::filter(haul_dist_ind, v != "at sea"), aes(year, mean_ind_haul_dist, color = sector2)) +
  geom_line() +
  xlab("Year") +
  ylab("Mean haul dist to port (km)") +
  ggtitle("Individual averages") +
  facet_grid(sector2~v, scale="free_y")


p14 <- ggplot(season_all_ports, aes(year,eff_days)) +
  geom_line() +
  facet_wrap(~sector2, scale="free_y") +
  ylab("Effective days fished") +
  xlab("Year") + theme_bw()

# do same, calculating seasonal trends by port for top ports
season_by_ports <-
  # dplyr::filter(season_eff, port %in% port_list) %>%
  dplyr::group_by(season_eff, year, drvid) %>%
  dplyr::summarize(sector2 = sector2[1],
                   ret_yr = ret_yr[1],
                   sum_p2 = sum(norm_ret^2),
                   r_port = port[1])

season_by_ports = dplyr::group_by(season_by_ports, year, sector2, r_port) %>%
  dplyr::summarize(eff_days = wtd.mean(1/sum_p2, ret_yr)) %>%
  dplyr::filter(r_port %in% port_list)

p15 <- ggplot(season_by_ports, aes(year,eff_days, group=r_port, col=r_port)) +
  geom_line() +
  facet_wrap(~sector2, scale="free_y") +
  ylab("Effective days fished") +
  xlab("Year") + theme_bw()+
  {if(data == "WC") theme(legend.title = element_text( size=2), legend.text=element_text(size=2))}

#For WC data, repeat effective days fished analysis on fish ticket data (and at-sea hake data)
if (data == "WC")
{
  #Still needs a little data prep
  ft$j_set_day <- lubridate::yday(ft$landing_date)
  ft$ret_mt <- ft$mt
  ft$r_port <- ft$pacfin_port_code

  if(include_all_vessels)
  {
    season_eff_ft <- ft %>%
      dplyr::group_by(year, drvid, j_set_day) %>%
      dplyr::summarise(ret = sum(ret_mt), sector2=sector2[1],
                       port = r_port[1]) %>% # calculate sum by person-day-year
      dplyr::group_by(year, drvid) %>%  # sum across days
      dplyr::mutate(ret_yr = sum(ret), norm_ret = ret/ret_yr) # normalize
  }else{
    season_eff_ft <- dplyr::filter(ft, port_fisher %in%  single_port_fishers_ft) %>%
      dplyr::group_by(year, drvid, j_set_day) %>%
      dplyr::summarise(ret = sum(ret_mt), sector2=sector2[1],
                       port = r_port[1]) %>% # calculate sum by person-day-year
      dplyr::group_by(year, drvid) %>%  # sum across days
      dplyr::mutate(ret_yr = sum(ret), norm_ret = ret/ret_yr) # normalize
  }

  # calculate weighted avg across fishers by retained catch --
  season_all_ports_ft <- dplyr::group_by(season_eff_ft, year, drvid) %>%
    dplyr::summarize(sector2 = sector2[1],
                     ret_yr = ret_yr[1],
                     sum_p2 = sum(norm_ret^2)) %>%
    dplyr::group_by(year, sector2) %>%
    dplyr::summarize(eff_days = wtd.mean(1/sum_p2, ret_yr))

  p14_ft <- ggplot(season_all_ports_ft, aes(year,eff_days)) +
    geom_line() +
    facet_wrap(~sector2, scale="free_y") +
    ylab("Effective days fished (fish tickets)") +
    xlab("Year") + theme_bw()

  #At sea hake doesn't have FTs, but it does have full observer coverage, so make a plot here
  p14_hake <- ggplot(season_all_ports %>% filter(sector2 == "At-sea hake"), aes(year,eff_days)) +
    geom_line() +
    facet_wrap(~sector2, scale="free_y") +
    ylab("Effective days fished") +
    xlab("Year") + theme_bw()

  # do same, calculating seasonal trends by port for top ports
  season_by_ports_ft <-
    # dplyr::filter(season_eff, port %in% port_list) %>%
    dplyr::group_by(season_eff_ft, year, drvid) %>%
    dplyr::summarize(sector2 = sector2[1],
                     ret_yr = ret_yr[1],
                     sum_p2 = sum(norm_ret^2),
                     r_port = port[1])

  season_by_ports_ft = dplyr::group_by(season_by_ports_ft, year, sector2, r_port) %>%
    dplyr::summarize(eff_days = wtd.mean(1/sum_p2, ret_yr)) #%>%
    #dplyr::filter(r_port %in% port_list)

  p15_ft <- ggplot(season_by_ports_ft, aes(year,eff_days, group=r_port, col=r_port)) +
    geom_line() +
    facet_wrap(~sector2, scale="free_y") +
    ylab("Effective days fished (fish tickets)") +
    xlab("Year") + theme_bw()+
    {if(data == "WC") theme(legend.title = element_text( size=2), legend.text=element_text(size=2))}
}

### Create spatial anomaly maps
#These plot the anomaly in each spatial pixel within each year

#Create raster grid. Resolution is currently arbitrary
ras <- raster(xmn=min(d$set_long), xmx=max(d$set_long), ymn=min(d$set_lat), ymx=max(d$set_lat), res=0.25, crs="+proj=longlat")# +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Create empty data frame to store results
d_grid <- data.frame(year= numeric(),
                     sector2 = character(),
                     set_lat = numeric(),
                     set_long = numeric(),
                     var = character(),
                     stringsAsFactors=FALSE)

#Loop over years, sectors to grid the data using the raster above
for(y in 1:(length(unique(d$year))))
{

  for(s in 1:length(unique(d$sector2)))
  {
    dat <- filter(d, year == unique(d$year)[y] & sector2 == unique(d$sector2)[s])

    if(nrow(dat > 0))
    {
      vessels_ras<-rasterize(cbind(dat$set_long, dat$set_lat), ras, dat$drvid, fun=n_distinct)
      retained_ras<-rasterize(cbind(dat$set_long, dat$set_lat), ras, dat$ret_mt, fun=sum)
      #hauls_ras<-rasterize(cbind(dat$set_long, dat$set_lat), ras, dat$haul_id, fun=n_distinct)

      vessels_df <- rasterToPoints(vessels_ras) %>%
        as.data.frame() %>%
        mutate(var = "n_vessels")

      retained_df <- rasterToPoints(retained_ras) %>%
        as.data.frame() %>%
        mutate(var = "ret_mt")

      # hauls_df <- rasterToPoints(hauls_ras) %>%
      #   as.data.frame() %>%
      #   mutate(var = "n_hauls")

      df <- bind_rows(vessels_df, retained_df) %>%
        mutate(year = dat$year[1],
               sector2 = dat$sector2[1]) %>%
        rename(set_long = x, set_lat = y, value = layer)

      d_grid <- bind_rows(d_grid, df)
    }


  }

}

#Fill in 0s for cells where there was no observed effort in a given year/sector, then calculate anomalies
d_grid <- d_grid %>%
  complete(year, var, nesting(sector2, set_lat, set_long), fill = list(value = 0)) %>%
  group_by(sector2, var, year) %>%
  mutate(mean = mean(value),
         sd  = sd(value)) %>%
  ungroup() %>%
  mutate(anom = (value - mean)/sd)


plot_list2 <- list()
plot_list3 <- list()

#Make anomaly plots
for (s in 1:length(unique(d$sector2)))
{
  plot_list2[[s]] <- ggplot() +
  geom_tile(data = filter(d_grid, sector2 == unique(d$sector2)[s] & var == "ret_mt"), aes(x = set_long, y = set_lat, fill = anom))+
  borders("state")+
  coord_map(xlim = c(min(d$set_long),max(d$set_long)),ylim = c(min(d$set_lat), max(d$set_lat)))+
  facet_wrap(~year)+
  scale_fill_viridis()+
  theme_bw()+
  ggtitle(paste("Retained catch weight,", unique(d$sector2)[s]))

  plot_list3[[s]] <- ggplot() +
  geom_tile(data = filter(d_grid, sector2 == unique(d$sector2)[s] & var == "n_vessels"), aes(x = set_long, y = set_lat, fill = anom))+
  borders("state")+
  coord_map(xlim = c(min(d$set_long),max(d$set_long)),ylim = c(min(d$set_lat), max(d$set_lat)))+
  facet_wrap(~year)+
  scale_fill_viridis()+
  theme_bw()+
  ggtitle(paste("Number of vessels,", unique(d$sector2)[s]))

}


pdf(paste0("output/",scale, "_summaries_",data,"_",cs_sens_label,"_", split_wc,".pdf"))
gridExtra::grid.arrange(p1,p2)
print(p4)
print(p5)
gridExtra::grid.arrange(p6, p7)
p8
p9
p10
p11
for(i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
#If WC, add in fish ticket seasonal patterns in addition to observed landings
if (data == "WC")
{
  p11_ft
}
p12
p13
if (data == "Alaska")
{
  p14
  p15
}
if(data == "WC")
{
  gridExtra::grid.arrange(p14_ft,p14_hake)
  p15_ft
}

for(i in 1:length(plot_list2)) {
  print(plot_list2[[i]])
}

for(i in 1:length(plot_list3)) {
  print(plot_list3[[i]])
}
dev.off()



