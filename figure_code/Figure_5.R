library(dplyr)
library(ggplot2)
library(viridis)

data <- c("Alaska", "WC")[2]
scale = c("region","port")[2]

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

#Use only individuals fishing before/after catch shares (FALSE includes all)?
cs_sensitivity <- FALSE

#label that appends file names with whether we subset only to vessels present both before and after CS ("stayers")
if(cs_sensitivity)
{
  cs_sens_label = "stayers"
}else{
  cs_sens_label = "allvessels"
}

#call up best models as needed
best_model_df <- model_results %>% dplyr::group_by(Run) %>% dplyr::filter(AIC == min(AIC))

effort = readRDS(paste0("output/predictions_",scale, "_","eff_days","_",data,"_",cs_sens_label,"_", split_wc,".rds"))
effort_ind = readRDS(paste0("output/predictions_",scale, "_","eff_days-ind","_",data,"_",cs_sens_label,"_", split_wc,".rds"))

# Only use data from best model
# if(data == "Alaska"){
#   effort = dplyr::filter(effort, model==6) %>%
#     dplyr::mutate("Scale"="Aggregate")
#   effort_ind = dplyr::filter(effort_ind, model==6) %>%
#     dplyr::mutate("Scale"="Individual")
# }else{
#   effort = dplyr::filter(effort, model==2) %>%
#     dplyr::mutate("Scale"="Aggregate")
#   effort_ind = dplyr::filter(effort_ind, model==6) %>%
#     dplyr::mutate("Scale"="Individual")
# }

#try calling up the best model predictions dynamically
effort <- dplyr::filter(effort, model == dplyr::filter(best_model_df, Run == "eff_days")$Model) %>% 
  dplyr::mutate("Scale"="Aggregate")

effort_ind <- dplyr::filter(effort_ind, model == dplyr::filter(best_model_df, Run == "eff_days-ind")$Model) %>% 
  dplyr::mutate("Scale"="Individual")

df = rbind(effort, effort_ind) %>%
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
#df$lo = exp(df$fit - 1.96*df$se)
#df$hi = exp(df$fit + 1.96*df$se)
#df$fit = exp(df$fit)
df$lo = df$fit - 1.96*df$se
df$hi = df$fit + 1.96*df$se

g = ggplot(df, aes(Year, fit, col=Scale, fill=Scale, group = Scale)) +
  geom_ribbon(aes(ymin=lo, ymax=hi),alpha=0.5, col=NA) +
  geom_line() +
  facet_wrap(Area~Sector,scale="free_y") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Year") +
  ylab("Ln effective days fished") +
  scale_color_viridis_d(end=0.5) +
  scale_fill_viridis_d(end=0.5)
g

ggsave(paste0("figures/effort_",scale, "_",data,"_",cs_sens_label,"_", split_wc,".jpeg"), height = 7, width = 7)





