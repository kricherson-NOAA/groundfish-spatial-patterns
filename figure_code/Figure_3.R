library(dplyr)
library(ggplot2)
library(viridis)

data <- c("Alaska", "WC")[1]
scale = c("region","port")[2]

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

# for Alaska data, model 5 is best supported for

dist = readRDS(paste0("output/predictions_",scale, "_","haul_dist","_",data,"_", split_wc,".rds"))
dist_ind = readRDS(paste0("output/predictions_",scale, "_","haul_dist-ind","_",data,"_", split_wc,".rds"))

# Only use data from best model
dist = dplyr::filter(dist, model==5) %>%
  dplyr::mutate("Scale"="Aggregate")
dist_ind = dplyr::filter(dist_ind, model==5) %>%
  dplyr::mutate("Scale"="Individual")
df = rbind(dist, dist_ind) %>%
  dplyr::rename(Sector = sector2, Area = subarea, Year = year)

# predictions are made for each port -- show average estimates
df = dplyr::group_by(df, Sector, Area, Year, Scale) %>%
  dplyr::summarize(
    n = n(), # n is number of ports
    fit = mean(fit,na.rm=T),
    se = sqrt((1/(n*n)) * sum(se.fit^2))
  )

# Renaming -- AK specific
df$Sector = as.character(df$Sector)
df$Sector = paste0(toupper(substr(df$Sector,1,1)), substr(df$Sector,2,nchar(df$Sector)))
df$Sector = as.factor(df$Sector)
df$Area = as.character(df$Area)
df$Area[which(df$Area=="CG")] = "Central Gulf"
df$Area[which(df$Area=="SE")] = "Southeast"
df$Area[which(df$Area=="SEI")] = "Southeast Inshore"
df$Area[which(df$Area=="WG")] = "Western Gulf"
df$Area[which(df$Area=="WY")] = "Western Yakutat"

# Could back-transform responses -- all logged in modeling script. CIs make plot
# hard to visualize
df$lo = exp(df$fit - 1.96*df$se)
df$hi = exp(df$fit + 1.96*df$se)
df$fit = exp(df$fit)


g = ggplot(df, aes(Year, fit, col=Scale, fill=Scale, group = Scale)) +
  geom_ribbon(aes(ymin=lo, ymax=hi),alpha=0.5, col=NA) +
  geom_line() +
  facet_wrap(Area~Sector,scale="free_y") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Year") +
  ylab("Distance from port") +
  scale_color_viridis_d(end=0.5) +
  scale_fill_viridis_d(end=0.5)
g
ggsave(paste0("figures/distance_",scale, "_",data,"_", split_wc,".jpeg"), height = 7, width=7)




