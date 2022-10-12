library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)

# include_all_vessels isn't included as a flag here because we'll just make
# one summary for each cs sensitivity, binding results together below
all_combos <- expand.grid(data = c("Alaska", "WC")[2],
                          scale = c("port"),
                          split_wc = c("north_south","one_area")[1],
                          cs_sensitivity = c(TRUE,FALSE))

for(ii in 1:nrow(all_combos)) {
  data <- all_combos$data[ii]
  scale <- all_combos$scale[ii]
  #Split west coast into north/south of 40 10?
  split_wc <- all_combos$split_wc[ii]
  cs_sensitivity <- all_combos$cs_sensitivity[ii]

  #label that appends file names with whether we subset only to vessels present both before and after CS ("stayers")
  if(cs_sensitivity)
  {
    cs_sens_label = "stayers"
  }else{
    cs_sens_label = "allvessels"
  }

  model_results_all <- readRDS(paste0("output/aic_",scale,"_",data,"_",cs_sens_label,"_", "allvessels","_",split_wc,".rds"))
  model_results_single <- readRDS(paste0("output/aic_",scale,"_",data,"_",cs_sens_label,"_", "singleport","_",split_wc,".rds"))
  model_results <- rbind(model_results_all, model_results_single)
# if(data == "Alaska"){
#   best_model_index = c(5,5,5,5,6,6,5,5,6,6) #
# }else{
#   best_model_index = c((model_results %>% group_by(Run) %>% filter(AIC == min(AIC)))$Model)
# }
best_models <- model_results %>%
    dplyr::group_by(Run) %>%
    dplyr::filter(AIC == min(AIC)) %>%
    dplyr::group_by(Run, Scale,Metric,Area) %>%
    dplyr::summarise(Model = Model[1])
best_model_index = c(best_models$Model)

run_names = best_models$Run

# Separate data frames for each sector
df_1 = data.frame(Model = 1:length(run_names),
                  run = run_names, Est = NA, SE = NA, P_value = NA)
df_2 = data.frame(Model = 1:length(run_names),
                  run = run_names, Est = NA, SE = NA, P_value = NA)

if(all_combos$data[1] == "WC")
{
  df_3 = data.frame(Model = 1:length(run_names),
                    run = run_names, Est = NA, SE = NA, P_value = NA)
}

for(run in 1:length(run_names)) {
  # save models for later summarizing
  fits = readRDS(paste0("output/gams_",scale, "_",run_names[run],"_",data,"_",cs_sens_label,"_", split_wc,".rds"))
  df_1$Est[run] = summary(fits[[best_model_index[run]]])$p.coeff[2]
  df_1$SE[run] = summary(fits[[best_model_index[run]]])$se[2]
  df_1$P_value[run] = summary(fits[[best_model_index[run]]])$p.pv[2]

  df_2$Est[run] = summary(fits[[best_model_index[run]]])$p.coeff[3]
  df_2$SE[run] = summary(fits[[best_model_index[run]]])$se[3]
  df_2$P_value[run] = summary(fits[[best_model_index[run]]])$p.pv[3]
  
  if(all_combos$data[1] == "WC")
  {
    df_3$Est[run] = summary(fits[[best_model_index[run]]])$p.coeff[4]
    df_3$SE[run] = summary(fits[[best_model_index[run]]])$se[4]
    df_3$P_value[run] = summary(fits[[best_model_index[run]]])$p.pv[4]
  }
}

df_1$Run = c("Days (aggregate)", "Days (individual)",
             "Distance (aggregate)", "Distance (individual)",
            "Inertia (aggregate)", "Inertia (individual)",
           "Latitude (aggregate)", "Latitude (individual)",
           "Longitude (aggregate)", "Longitude (individual)")
df_2$Run <- df_1$Run

if(all_combos$data[1] == "WC")
{
  df_3$Run <- df_1$Run
}



if(data == "Alaska") {
  df_1$Sector <- "rockfish"
  df_2$Sector <- "longline"
} else {
  df_1$Sector <- "LE/CS Trawl"
  df_2$Sector <- "At-sea hake CP"
  df_3$Sector <- "At-sea hake MS"
}
df = rbind(df_1, df_2)
if(all_combos$data[1] == "WC")
{
  df = rbind(df_1, df_2, df_3)
}

df = dplyr::select(df, -run) %>% 
  #NOTE: the coef values aren't correct for the At-sea hake haul_dist model, since that model only includes shoreside sectors
  mutate_at(vars(Est,SE, P_value), 
            ~(ifelse(grepl("Distance", Run) & grepl("At-sea hake", Sector), NA, .)))

saveRDS(df,paste0("output/table_catchshares_",data,"_",cs_sens_label,".rds"))

df_table <- df %>%
  dplyr::select(Run, Est, SE, P_value, Sector) %>%
  mutate_if(is.numeric, round, 4)

write.csv(df_table,paste0("output/table_catchshares_",data,"_",cs_sens_label,".csv"), row.names = F)

}
