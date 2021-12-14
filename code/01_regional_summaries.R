library(dplyr)
library(ggplot2)
library(zoo)
library(Hmisc)
library(lubridate)
library(ggridges)

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- "Alaska"
scale = c("region","port")[2]
n_top_ports <- 10

if(data == "Alaska") {
  d <- readRDS("Data/subset_pfxcommercial_cleaned_allyears_renamed.rds")
} else {
  # Kate -- add file here
  d$area <- "WC"
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
area_cog_ind <- dplyr::filter(d, r_port %in% port_list, port_fisher %in% port_fisher$port_fisher) %>%
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
  dplyr::filter(d, r_port %in% port_list, port_fisher %in% port_fisher$port_fisher) %>%
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
  facet_grid(sector2 ~ v, scale="free") +
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
  facet_wrap(~area, scale="free_y")

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


plot_list <- list()
sub = dplyr::filter(d[d_sample,], r_port %in% port_list)
for(i in 1:length(unique(sub$v))) {
  plot_list[[i]] <- ggplot(dplyr::filter(sub, v == unique(sub$v)[i]), aes(j_set_day, year, group=year)) +
    geom_density_ridges() +
    xlab("Calendar day") +
    ylab("Year") +
    ggtitle(paste0("Distribution of landings: ",unique(sub$v)[i]))
}



pdf(paste0("output/",scale,"_summaries_",data,".pdf"))
gridExtra::grid.arrange(p1,p2)
print(p4)
print(p5)
gridExtra::grid.arrange(p6, p7)
p8
p9
p10
for(i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
dev.off()


