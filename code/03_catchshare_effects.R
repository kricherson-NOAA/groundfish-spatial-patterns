library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)

data <- c("Alaska", "WC")[1]
scale = c("region","port")[2]

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

best_model_index = c(5,5,5,5,6,6) #
run_names = c("area_permit_cog","area_permit_cog-ind","haul_dist","haul_dist-ind","eff_days","eff_days-ind")

df = data.frame(Model = 1:6, run = run_names, Est = NA, SE = NA, P_value = NA)

for(run in 1:length(run_names)) {

  # save models for later summarizing
  fits = readRDS(paste0("output/gams_",scale, "_",run_names[run],"_",data,"_", split_wc,".rds"))
  df$Est[run] = summary(fits[[5]])$p.coeff[2]
  df$SE[run] = summary(fits[[5]])$se[2]
  df$P_value[run] = summary(fits[[5]])$p.pv[2]
}

df$Run = c("Inertia (aggregate)", "Inertia (individual)", "Distance (aggregate)", "Distance (individual)", "Days (aggregate)", "Days (individual)")

df = dplyr::select(df, -run)

saveRDS(df,paste0("output/table_catchshares_",data,".rds"))
