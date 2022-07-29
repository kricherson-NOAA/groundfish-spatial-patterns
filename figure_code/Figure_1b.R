library(tidyverse)
library(ggspatial)
library(ggmap)
library(viridis)
library(ggpubr)

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- c("Alaska", "WC")[1]
scale = c("region","port")[1]
n_top_ports <- 50

transparency <- 0.3
#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]
cs_sens_label = "allvessels"
# these dataframes get created by 01_regional_summaries.r
cs_sens_label = "allvessels"
area_permit_cog = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog",".rds"))
area_permit_cog_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_cog-ind",".rds"))
haul_dist = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist",".rds"))
haul_dist_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_hauldist-ind",".rds"))
eff_days = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_days",".rds"))
eff_days_ind = readRDS(paste0("data/",data,"_",cs_sens_label,"_",scale,"_days-ind",".rds"))

area_permit_cog$sector2[which(area_permit_cog$sector2=="longline")] = "Longline"
area_permit_cog$sector2[which(area_permit_cog$sector2=="misc. groundfish")] = "Misc. groundfish"
area_permit_cog$sector2[which(area_permit_cog$sector2=="pelagic trawl")] = "Pelagic trawl"
area_permit_cog$sector2[which(area_permit_cog$sector2=="rockfish")] = "Rockfish"
area_permit_cog_ind$sector2[which(area_permit_cog_ind$sector2=="longline")] = "Longline"
area_permit_cog_ind$sector2[which(area_permit_cog_ind$sector2=="misc. groundfish")] = "Misc. groundfish"
area_permit_cog_ind$sector2[which(area_permit_cog_ind$sector2=="pelagic trawl")] = "Pelagic trawl"
area_permit_cog_ind$sector2[which(area_permit_cog_ind$sector2=="rockfish")] = "Rockfish"

#What would it look like it we plot the region-level COG over time?
#sbbox <- make_bbox(lon = area_permit_cog$lon_mean, lat = area_permit_cog$lat_mean, f = .1)
sbbox <- make_bbox(lon = c(-165,-135), lat = c(50,60), f = .1)
sq_map <- get_map(location = sbbox, maptype = "sattelite", source = "google")

ll_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "longline"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Longline")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog$total_sd))

rf_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "rockfish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Rockfish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog$total_sd))

pt_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "pelagic trawl"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Pelagic trawl")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog$total_sd))

gf_map <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog, sector2 == "misc. groundfish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Misc. groundfish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog$total_sd))

all_maps <- ggarrange(gf_map,ll_map, pt_map,rf_map,
          #labels = c("A", "B", "C"),
          ncol = 2,
          nrow = 2,
          common.legend = TRUE, legend="right") +
  bgcolor("white")

ggsave(all_maps, file = paste0("figures/cg_inertia_map_",scale, "_",data,"_", split_wc,".jpeg"), height=7, width=7)

#Also make an individual level map, maybe for supplement?
ll_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "longline"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year), alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Longline")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  scale_size(limits = range(area_permit_cog_ind$total_sd))

rf_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "rockfish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Rockfish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog_ind$total_sd))

pt_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "pelagic trawl"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Pelagic trawl")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog_ind$total_sd))

gf_map_ind <- ggmap(sq_map) +
  geom_point(data = dplyr::filter(area_permit_cog_ind, sector2 == "misc. groundfish"), mapping = aes(x = lon_mean, y = lat_mean, size = total_sd, color = year),alpha = transparency)+
  scale_color_viridis()+
  ggtitle("Misc. groundfish")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(color = "Year")+
  labs(size = "Inertia")+
  geom_hline(yintercept = 40 + 1/6)+
  scale_size(limits = range(area_permit_cog_ind$total_sd))


all_maps_ind <- ggarrange(gf_map_ind,ll_map_ind, pt_map_ind,rf_map_ind,
                      #labels = c("A", "B", "C"),
                      ncol = 2,
                      nrow = 2,
                      common.legend = TRUE, legend="right") +
  bgcolor("white")

ggsave(all_maps_ind, file = paste0("figures/cg_inertia_map_ind_",scale, "_",data,"_", split_wc,".jpeg"), height=7, width=7)




