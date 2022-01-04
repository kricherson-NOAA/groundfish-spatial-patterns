library(dplyr)
library(ggplot2)
library(zoo)
library(Hmisc)
library(lubridate)
library(ggridges)
library(sinkr) #Has a function to calculate distance in km between two points

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- c("Alaska", "WC")[2]
scale = c("region","port")[1]
n_top_ports <- 10

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

#Which WC sectors to include?
wc_sectors <- c("LE/CS Trawl",
                "OA Fixed Gear",
                "Limited Entry Sablefish",
                "LE Fixed Gear DTL",
                "Nearshore",
                "At-sea hake CP",
                "At-sea hake MS",
                "CS Fixed Gear",
                "Shoreside/midwater Hake")[1:7]

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
    dplyr::mutate(sector2 = ifelse(grepl("At-sea hake", sector2),
                                   "At-sea hake",
                                   sector2)) #Combine at-sea CP and MS for now

  if(split_wc == "one_area")
  {
    d$area <- "WC"
  }

 ft <- readRDS("Data/ft.rds") %>%
   dplyr::filter(sector2 %in% wc_sectors)

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
} else {
  d$v <- d$area
  port_list = unique(d$r_port)
}

# only include individuals who fish in a single port in a year
d$port_fisher = paste(d$year, d$drvid)
port_fisher = dplyr::group_by(d, year, drvid) %>%
  dplyr::summarize(n = length(unique(r_port)),
                   port_fisher = paste(year, drvid)) %>%
  dplyr::filter(n == 1)

#I get an error about "$ operator is invalid for atomic vectors" when filtering to data to port_fisher %in% port_fisher$port_fisher, this is a workaround -KR
single_port_fishers <- port_fisher$port_fisher

# do coarse summaries by area and year
area_cog <-
  dplyr::filter(d, r_port %in% port_list) %>%
  dplyr::group_by(v, year) %>%
  dplyr::summarize(sum_mt = sum(ret_mt,na.rm=T),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2),
                   individuals="Ind. ignored")
# do same -- but vessel/individual averages
area_cog_ind <- dplyr::filter(d, r_port %in% port_list, port_fisher %in% single_port_fishers) %>%
  dplyr::group_by(v, drvid, year) %>%
  dplyr::summarize(n = n(),
                   sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2)) %>%
  dplyr::filter(n >= 10, !is.na(lat_mean+lon_mean+lat_sd+long_sd)) %>%
  dplyr::group_by(v, year) %>%
  dplyr::summarize(sum_mt = mean(sum_mt),
                     lat_mean = mean(lat_mean),
                   lon_mean = mean(lon_mean),
                   lat_sd = mean(lat_sd),
                   long_sd = mean(long_sd),
                   total_sd = mean(total_sd),
                   individuals="Ind. averaged")

# instead of just stratifying by area, do area + permit (sector2)
area_permit_cog <-
  dplyr::filter(d, r_port %in% port_list) %>%
  dplyr::group_by(v, sector2, year) %>%
  dplyr::summarize(sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2),
                   individuals="Ind. ignored")

# do same -- but vessel/individual averages
area_permit_cog_ind <-
  dplyr::filter(d, r_port %in% port_list, port_fisher %in% single_port_fishers) %>%
  dplyr::group_by(v, sector2, drvid, year) %>%
  dplyr::summarize(n = n(),
                   sum_mt = sum(ret_mt),
                   lat_mean = wtd.mean(set_lat, ret_mt),
                   lon_mean = wtd.mean(set_long, ret_mt),
                   lat_sd = sqrt(wtd.var(set_lat, ret_mt)),
                   long_sd = sqrt(wtd.var(set_long, ret_mt)),
                   total_sd = sqrt(lat_sd^2 + long_sd^2)) %>%
  dplyr::filter(n >= 10, !is.na(lat_mean+lon_mean+lat_sd+long_sd)) %>%
  dplyr::group_by(v, year, sector2) %>%
  dplyr::summarize(sum_mt = mean(sum_mt),
                   lat_mean = mean(lat_mean),
                   lon_mean = mean(lon_mean),
                   lat_sd = mean(lat_sd),
                   long_sd = mean(long_sd),
                   total_sd = mean(total_sd),
                   individuals="Ind. averaged")

#Calculating distance from return port. For WC, this is the haul distance from port
#First, taken over all observations
haul_dist <-
  dplyr::filter(d, r_port %in% port_list, port_fisher %in% single_port_fishers) %>%
  dplyr::group_by(v, sector2, year, haul_id) %>%
  dplyr::summarise(port_dist = sinkr::earthDist(set_long, set_lat, r_port_long, r_port_lat)) %>%
  dplyr::group_by(v, sector2, year) %>%
  dplyr::summarise(mean_haul_dist = mean(port_dist, na.rm = T))

#Second, taken over individuals/vessels, then across port/sector
haul_dist_ind <-
  dplyr::filter(d, r_port %in% port_list) %>%
  dplyr::group_by(v, sector2, year, drvid, haul_id) %>%
  dplyr::summarise(port_dist = sinkr::earthDist(set_long, set_lat, r_port_long, r_port_lat)) %>%
  dplyr::group_by(v, sector2, year, drvid) %>%
  dplyr::summarise(mean_haul_dist = mean(port_dist, na.rm = T)) %>%
  dplyr::group_by(v, sector2, year) %>%
  dplyr::summarise(mean_ind_haul_dist = mean(mean_haul_dist, na.rm = T))

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
  geom_density_ridges() +
  xlab("Calendar day") +
  ylab("Year") +
  ggtitle("Distribution of landings")

#Also plot by sector
p11 <- ggplot(dplyr::filter(d[d_sample,], r_port %in% port_list), aes(j_set_day, year, group=year)) +
  geom_density_ridges() +
  xlab("Calendar day") +
  ylab("Year") +
  ggtitle("Distribution of landings")+
  facet_wrap(~sector2, scales = "free")

plot_list <- list()
sub = dplyr::filter(d[d_sample,], r_port %in% port_list)
for(i in 1:length(unique(sub$v))) {
  plot_list[[i]] <- ggplot(dplyr::filter(sub, v == unique(sub$v)[i]), aes(j_set_day, year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle(paste0("Distribution of landings: ",unique(sub$v)[i]))
}

#For WC, let's also plot fish ticket landings since partial observer coverage makes seasonal patterns harder to discern
if (data == "WC")
{
  ft$id = seq(1,nrow(ft))
  ft_sample = sample(ft$id, size=nrow(ft), replace=T, prob = ft$mt)

  #Note: not filtering by port for now
  p10 <- ggplot(ft[ft_sample,], aes(yday(landing_date), year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle("Distribution of landings (fish tickets)")


  p11 <- ggplot(ft[ft_sample,], aes(yday(landing_date), year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle("Distribution of landings (fish tickets)")+
    facet_grid(area~sector2, scales = "free")

  plot_list <- list()
  sub = ft[ft_sample,] %>%
    dplyr::mutate(j_landing_date = yday(landing_date))

  for(i in 1:length(unique(sub$area))) {
    plot_list[[i]] <- ggplot(dplyr::filter(sub, area == unique(sub$area)[i]), aes(j_landing_date, year, group=year)) +
      geom_density_ridges() +
      xlab("Calendar day") +
      ylab("Year") +
      ggtitle(paste0("Distribution of landings: ",unique(sub$area)[i]))
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

# also add calculations for the effective number of days fished. This
# follows from the portfolio work we've done previously (Anderson et al. PNAS,
# Ward et al. J Appl Ecol) -- the formula is Effective[x] = sum(p_i^2),
# where p_i is the proportion of landings for any individual fisher
# this is calculated separately by person and year, and then averaged across people
season_eff <- dplyr::filter(d, port_fisher %in% port_fisher) %>%
  dplyr::group_by(year, drvid, j_set_day) %>%
  dplyr::summarise(ret = sum(ret_mt), sector2=sector2[1]) %>% # calculate sum by person-day-year
  dplyr::group_by(year, drvid) %>%  # sum across days
  dplyr::mutate(ret_yr = sum(ret), norm_ret = ret/ret_yr) # normalize
season_eff <- dplyr::group_by(season_eff, year, drvid) %>%
  dplyr::summarize(sector2 = sector2[1],
                   ret_yr = ret_yr[1],
                   sum_p2 = sum(norm_ret^2))
# calculate weighted avg across fishers by retained catch --
season_eff <- dplyr::group_by(season_eff, year, sector2) %>%
  dplyr::summarize(eff_days = wtd.mean(1/sum_p2, ret_yr))

p14 <- ggplot(season_eff, aes(year,eff_days)) +
  geom_line() +
  facet_wrap(~sector2, scale="free_y") +
  ylab("Effective days fished") +
  xlab("Year") + theme_bw()

pdf(paste0("output/",scale, "_summaries_",data,"_", split_wc,".pdf"))
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
p12
p13
p14
dev.off()


