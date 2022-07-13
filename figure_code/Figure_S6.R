library(tidyverse)
library(ggspatial)
library(ggmap)
library(viridis)
library(ggpubr)

# these plots things like the COG or intertia (variance) for the larger ecoregions
data <- c("Alaska", "WC")[1]
scale = c("region","port")[2]
n_top_ports <- 50

transparency <- 0.6
#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

port_df <- readRDS(paste0("data/",data,"_",scale,"_ports",".rds"))
port_df$sector2[which(port_df$sector2=="longline")] = "Longline"
port_df$sector2[which(port_df$sector2=="misc. groundfish")] = "Misc. groundfish"
port_df$sector2[which(port_df$sector2=="pelagic trawl")] = "Pelagic trawl"
port_df$sector2[which(port_df$sector2=="rockfish")] = "Rockfish"

g <- ggplot(port_df, aes(year, n_port, group=subarea, col = subarea)) +
  geom_line(alpha=transparency) +
  facet_wrap(~sector2, scale="free_y") +
  theme_bw() +
  xlab("") +
  ylab("Utilized ports") +
  scale_color_viridis_d(end=0.8)

ggsave(g, filename = paste0("figures/Utilized_ports_",data,".png"),
       height = 5, width = 6)
