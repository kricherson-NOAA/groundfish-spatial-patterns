library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)

data <- c("Alaska", "WC")[1]
scale = c("region","port")[2]

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

if(data == "Alaska"){
  best_model_index = c(5,5,5,5,6,6,5,5,6,6) #
}else{
  best_model_index = c(6,6,4,4,2,6)
}
run_names = c("inertia","inertia-ind",
              "haul_dist","haul_dist-ind",
              "eff_days","eff_days-ind",
              "lat","lat-ind",
              "lon","lon-ind")

# Separate data frames for each sector
df_1 = data.frame(Model = 1:length(run_names),
                  run = run_names, Est = NA, SE = NA, P_value = NA)
df_2 = data.frame(Model = 1:length(run_names),
                  run = run_names, Est = NA, SE = NA, P_value = NA)

for(run in 1:length(run_names)) {
  # save models for later summarizing
  fits = readRDS(paste0("output/gams_",scale, "_",run_names[run],"_",data,"_", split_wc,".rds"))
  df_1$Est[run] = summary(fits[[best_model_index[run]]])$p.coeff[2]
  df_1$SE[run] = summary(fits[[best_model_index[run]]])$se[2]
  df_1$P_value[run] = summary(fits[[best_model_index[run]]])$p.pv[2]

  df_2$Est[run] = summary(fits[[best_model_index[run]]])$p.coeff[3]
  df_2$SE[run] = summary(fits[[best_model_index[run]]])$se[3]
  df_2$P_value[run] = summary(fits[[best_model_index[run]]])$p.pv[3]
}

df_1$Run = c("Inertia (aggregate)", "Inertia (individual)",
           "Distance (aggregate)", "Distance (individual)",
           "Days (aggregate)", "Days (individual)",
           "Latitude (aggregate)", "Latitude (individual)",
           "Longitude (aggregate)", "Longitude (individual)")
df_2$Run <- df_1$Run

if(data == "Alaska") {
  df_1$Sector <- "rockfish"
  df_2$Sector <- "longline"
} else {
  df_1$Sector <- "LE/CS Trawl"
  df_2$Sector <- "At-sea hake"
}
df = rbind(df_1, df_2)
df = dplyr::select(df, -run)

saveRDS(df,paste0("output/table_catchshares_",data,".rds"))

df_table <- df %>%
  dplyr::select(Run, Est, SE, P_value, Sector) %>%
  mutate_if(is.numeric, round, 4)

write.csv(df_table,paste0("output/table_catchshares_",data,".csv"), row.names = F)
