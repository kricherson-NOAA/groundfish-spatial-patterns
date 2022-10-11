library(tidyverse)
library(ggspatial)
library(ggmap)
library(viridis)
library(ggpubr)

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- c("Alaska", "WC")[2]
scale = c("region","port")[1]
n_top_ports <- 50

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

# these dataframes get created by 01_regional_summaries.r
cs_sens_label = c("allvessels", "stayers")[1]
area_permit_cog = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog",".rds"))
area_permit_cog_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog-ind",".rds"))
haul_dist = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist",".rds"))
haul_dist_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist-ind",".rds"))
eff_days = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_days",".rds"))
eff_days_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_days-ind",".rds"))

#What would it look like it we plot the region-level COG over time?
#sbbox <- make_bbox(lon = area_permit_cog$lon_mean, lat = area_permit_cog$lat_mean, f = .1)
sbbox <- make_bbox(lon = c(-126,-120), lat = c(34.5,48), f = .1)
sq_map <- get_map(location = sbbox, maptype = "sattelite", source = "google")

ascp_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "At-sea hake CP"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("At-sea\nhake CP")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog$total_sd))

asms_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "At-sea hake MS"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("At-sea\nhake MS")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog$total_sd))



bt_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "LE/CS Trawl"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("Groundfish\nbottom trawl")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog$total_sd))


sabl_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "Limited Entry Sablefish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("Limited entry\nsablefish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog$total_sd))


all_maps <- ggarrange(asms_map, ascp_map, bt_map, sabl_map,
          #labels = c("A", "B", "C"),
          ncol = 4,
          nrow = 1,
          common.legend = TRUE, legend="right")

ggsave(all_maps, file = paste0("figures/cg_inertia_map_",scale, "_",data,"_", split_wc,".jpeg"), height=7, width=10)

#Also make an individual level map, maybe for supplement?
asms_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "At-sea hake MS"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("At-sea\nhake")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog_ind$total_sd))

ascp_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "At-sea hake CP"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("At-sea\nhake")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog_ind$total_sd))


bt_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "LE/CS Trawl"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("Groundfish\nbottom trawl")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog_ind$total_sd))


sabl_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "Limited Entry Sablefish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = 0.75)+
  scale_color_viridis()+
  ggtitle("Limited entry\nsablefish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog_ind$total_sd))


all_maps_ind <- ggarrange(asms_map_ind, ascp_map_ind, bt_map_ind, sabl_map_ind,
                      #labels = c("A", "B", "C"),
                      ncol = 4,
                      common.legend = TRUE, legend="right")

ggsave(all_maps_ind, file = paste0("figures/cg_inertia_map_ind_",scale, "_",data,"_", split_wc,".jpeg"), height=7, width=10)




