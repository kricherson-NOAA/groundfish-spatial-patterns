library(mgcv)
library(dplyr)
library(gratia)
library(ggplot2)

data <- c("Alaska", "WC")[2]
scale = c("region","port")[2]

#Split west coast into north/south of 40 10?
split_wc <- c("north_south", "one_area")[1]

# these dataframes get created by 01_regional_summaries.r
area_permit_cog = readRDS(paste0("data/",data,"_",scale,"_cog",".rds"))
area_permit_cog_ind = readRDS(paste0("data/",data,"_",scale,"_cog-ind",".rds"))
haul_dist = readRDS(paste0("data/",data,"_",scale,"_hauldist",".rds"))
haul_dist_ind = readRDS(paste0("data/",data,"_",scale,"_hauldist-ind",".rds"))
eff_days = readRDS(paste0("data/",data,"_",scale,"_days",".rds"))
eff_days_ind = readRDS(paste0("data/",data,"_",scale,"_days-ind",".rds"))

# using area permit_cog as an example, the predictor variables here are:
# year, port (v) / subarea, and sector2.

# Example questions:
# - are trends in inertia explained better by region (subarea), sector,
# or is there no common pattern?
plot_list = list()
pred_list = list()

for(run in c("inertia","inertia-ind",
             "haul_dist","haul_dist-ind",
             "eff_days","eff_days-ind",
             "lat","lat-ind",
             "lon","lon-ind")) {

  if(run %in% c("inertia","lat","lon")) dat = area_permit_cog
  if(run %in% c("inertia-ind","lat-ind","lon-ind")) dat = area_permit_cog_ind
  if(run=="haul_dist") dat = haul_dist
  if(run=="haul_dist-ind") dat = haul_dist_ind
  if(run=="eff_days") dat = eff_days
  if(run=="eff_days-ind") dat = eff_days_ind

  if(run%in% c("inertia","inertia-ind")) {
    ylabel = "ln sqrt(inertia)"
    dat$response = log(dat$total_sd)
  }
  if(run%in% c("lat","lat-ind")) {
    ylabel = "latitude"
    dat$response = dat$lat_mean
  }
  if(run%in% c("lon","lon-ind")) {
    ylabel = "longitude"
    dat$response = dat$lon_mean
  }
  if(run%in% c("haul_dist","haul_dist-ind")) {
    ylabel = "ln distance"
    dat$response=log(dat$mean_haul_dist)
  }
  if(run%in% c("eff_days","eff_days-ind")) {
    ylabel = "ln eff. days"
    dat$response=log(dat$effective_days)
  }

  dat = dplyr::filter(dat, !is.na(response), is.finite(response)) %>%
    dplyr::filter(!is.na(sector2), !is.na(subarea))

  dat$port = as.factor(dat$v)
  # only use ports with 10 or more years of data
  dat = group_by(dat, port) %>%
    dplyr::mutate(nyear = length(unique(year))) %>%
    dplyr::filter(nyear>=10) %>%
    dplyr::select(-nyear)

  # TODO: discuss appropriate weights. What level of variability / sample size do we care about?
  dat$weights = 1
  #if(run %in% c("area_permit_cog-ind", "haul_dist-ind","eff_days-ind")) dat$weights = dat$n
  #dat = dplyr::filter(dat, total_cv > 0, !is.na(total_cv)) %>%
  #  dplyr::mutate(tot_weights = 1/total_cv,
  #                weights = 1)#tot_weights/mean(tot_weights))

  # filter out combinations that aren't common in the data
  dat$sector2 = as.factor(dat$sector2)
  dat$subarea = as.factor(dat$subarea)
  dat$sector_subarea = as.factor(paste(dat$sector2, dat$subarea))
  not_rare = dplyr::group_by(dat, sector_subarea) %>%
    dplyr::summarise(n = n()) %>% dplyr::filter(n >= quantile(n,0.25))
  #Need to adjust this for WC so that we don't exclude at-sea hake, which only has one "port"
  if(data == "WC")
  {
    #dat = dplyr::filter(dat, sector_subarea %in% not_rare$sector_subarea | sector2 == "At-sea hake")
    NULL #Filtering only to common combinations in the WC data removes LE sablefish south for the distance from port calculations, and I don't think we want that
  }else{
    dat = dplyr::filter(dat, sector_subarea %in% not_rare$sector_subarea)
  }
  dat$sector_subarea = as.factor(as.character(dat$sector_subarea))

  # binary indicator for rationalization
  dat$catch_share = 0 # 0 before, 1 after
  if(data == "Alaska") {
    # longline misc. groundfish    pelagic trawl         rockfish
    dat$catch_share[which(dat$sector2 == "rockfish" & dat$year >= 2005)] = 1
    dat$catch_share[which(dat$sector2 == "longline" & dat$year >= 1995)] = 2
  }else{
    dat$catch_share[which(dat$sector2 == "LE/CS Trawl" & dat$year >= 2011)] = 1
    dat$catch_share[which(dat$sector2 == "At-sea hake" & dat$year >= 2011)] = 2
  }
  dat$catch_share = as.factor(dat$catch_share)

  # global year effect, with port level smooths (port = v)
  fit = list()
  K = 10 # knots for year effects, for factor smooths

  # New null model, response varies by subarea
  fit[[1]] = gam(response ~ catch_share + s(year,bs="ps"), weights = weights,data = dat)
  fit[[2]] = gam(response ~ catch_share + sector2 + subarea + s(year,bs="ps"), weights = weights,data = dat)
  fit[[3]] = gam(response ~ catch_share + sector2 + subarea + s(year, subarea, k=K, bs="fs", m=2), weights = weights,data = dat)
  fit[[4]] = gam(response ~ catch_share + sector2 + subarea + s(year, sector2, k=K, bs="fs", m=2), weights = weights,data = dat)
  fit[[5]] = gam(response ~ catch_share + sector2 + s(year, subarea, k=K, bs="fs", m=2) + s(year, sector2, k=K, bs="fs", m=2), weights = weights,data = dat)
  fit[[6]] = gam(response ~ catch_share + sector2 + subarea + s(year, sector_subarea, k=K, bs="fs", m=2), weights = weights,data = dat)

  # save models for later summarizing
  saveRDS(fit, paste0("output/gams_",scale, "_",run,"_",data,"_", split_wc,".rds"))

  # Add random intercepts to ports
  # fit[[7]] = gam(response ~ s(year,bs="ps") + s(port,bs="re"), weights = weights,data = dat)
  # fit[[8]] = gam(response ~ sector2 + subarea + s(year,bs="ps") + s(port,bs="re"), weights = weights,data = dat)
  # fit[[9]] = gam(response ~ sector2 + s(year, subarea, k=K, bs="fs", m=2) + s(port,bs="re"), weights = weights,data = dat)
  # fit[[10]] = gam(response ~ subarea + s(year, sector2, k=K, bs="fs", m=2) + s(port,bs="re"), weights = weights,data = dat)
  # fit[[11]] = gam(response ~ s(year, subarea, k=K, bs="fs", m=2) + s(year, sector2, k=K, bs="fs", m=2) + s(port,bs="re"), weights = weights,data = dat)
  # fit[[12]] = gam(response ~ s(year, sector_subarea, k=K, bs="fs", m=2) + s(port,bs="re"), weights = weights,data = dat)

  # create new data to predict on -- representing mean for each area-sector
  newdata = expand.grid(year = unique(dat$year),
                        sector2 = unique(dat$sector2),
                        port = unique(dat$port),
                        catch_share = 0)
  if(data == "Alaska") {
    # longline misc. groundfish    pelagic trawl         rockfish
    newdata$catch_share[which(newdata$sector2 == "rockfish" & newdata$year >= 2005)] = 1
    newdata$catch_share[which(newdata$sector2 == "longline" & newdata$year >= 1995)] = 2
  }else{
    newdata$catch_share[which(newdata$sector2 == "LE/CS Trawl" & newdata$year >= 2011)] = 1
    newdata$catch_share[which(newdata$sector2 == "At-sea hake" & newdata$year >= 2011)] = 2
  }
  newdata$catch_share = as.factor(newdata$catch_share)
  # add block

  newdata = dplyr::filter(newdata,!is.na(sector2), !is.na(port))
  port_area = dplyr::group_by(dat, port) %>% dplyr::summarize(subarea = subarea[1])
  newdata = dplyr::left_join(newdata, port_area)
  newdata$sector_subarea = as.factor(paste(newdata$sector2, newdata$subarea))
  newdata = dplyr::filter(newdata,sector_subarea %in% unique(dat$sector_subarea))

  # This block is just making predictions for each model and binding all together
  model_pred = predict(fit[[1]], newdata = newdata, se.fit=TRUE)
  newdf = cbind(newdata, model_pred)
  newdf$model = 1
  newdf$run = run
  newdf$scale = scale
  newdf$region = data
  all_pred = newdf
  for(i in 2:6) {
    model_pred = predict(fit[[i]], newdata = newdata, se.fit=TRUE)
    newdf = cbind(newdata, model_pred)
    newdf$model = i
    newdf$run = run
    newdf$scale = scale
    newdf$region = data
    all_pred = rbind(all_pred, newdf)
  }
  saveRDS(all_pred, paste0("output/predictions_",scale, "_",run,"_",data,"_", split_wc,".rds"))

  # make initial plots using model with lowest AIC
  model_pred <- predict(fit[[which.min(lapply(fit,AIC))]], newdata=newdata, se.fit=TRUE) # predict to new port -- will be same as mean
  newdata = cbind(newdata, model_pred)

  newdata = dplyr::group_by(newdata, sector_subarea, year) %>%
    dplyr::summarize(
      sector2 = sector2[1],
      subarea = subarea[1],
      n = n(), # n is number of ports
                     fit = mean(fit,na.rm=T),
                     se = sqrt((1/(n*n)) * sum(se.fit^2))
                     )
  #keep track of which model had lowest AIC
  best_model <- which.min(lapply(fit,AIC))

  # look at individual smooths
  #sm = get_smooth(fit[[which.min(lapply(fit,AIC))]], "year")
  #sm = smooth_estimates(fit[[5]], data=newdata)
  #sm = dplyr::filter(sm, smooth=="s(year,sector_subarea)")
  #model_pred <- predict(fit[[which.min(lapply(fit,AIC))]], newdata=newdata, se.fit=TRUE) # predict to new port -- will be same as mean

  # save plots
  if(run=="inertia") {
    counter = 1
  } else {
    counter = counter + 1
  }
  plot_list[[counter]] = ggplot(newdata, aes(year, fit, col=sector2, fill=sector2)) +
    geom_line() +
    geom_ribbon(aes(ymin=fit-2*se, ymax=fit+2*se),alpha=0.5) +
    facet_wrap(subarea~sector2,scale="free_y") +
    theme_bw() + xlab("") + ylab(ylabel) + ggtitle(paste0(run, " (model ", best_model, ")"))

  pred_list[[counter]] = newdata
}

## Make summary table of AIC stats for paper
model_results = NULL
for(run in c("inertia","inertia-ind",
             "haul_dist","haul_dist-ind",
             "eff_days","eff_days-ind",
             "lat","lat-ind",
             "lon","lon-ind")) {
  fit = readRDS(paste0("output/gams_",scale, "_",run,"_",data,"_", split_wc,".rds"))
  # extract AIC etc
  df = data.frame("Model"=1:6, "Run" = run, "AIC" = unlist(lapply(fit,AIC)))
  df$Scale = ifelse(length(grep("ind",run))==0,"Aggregate","Individual")
  df$AIC = df$AIC - min(df$AIC)
  # bind all results together
  if(is.null(model_results)) {
    model_results = df
  } else {
    model_results = rbind(model_results, df)
  }
}
model_results$Metric = "Season"
model_results$Metric[which(model_results$Run %in% c("inertia","inertia-ind"))] = "Inertia"
model_results$Metric[which(model_results$Run %in% c("haul_dist","haul_dist-ind"))] = "Distance"
model_results$Metric[which(model_results$Run %in% c("lat","lat-ind"))] = "Latitude"
model_results$Metric[which(model_results$Run %in% c("lon","lon-ind"))] = "Longitude"
model_results$Area = data
saveRDS(model_results, paste0("output/aic_",scale, "_",run,"_",data,"_", split_wc,".rds"))


pdf(paste0("output/",scale, "_gams_",data,"_", split_wc,".pdf"))
plot_list[1]
plot_list[2]
plot_list[3]
plot_list[4]
plot_list[5]
plot_list[6]
plot_list[7]
plot_list[8]
plot_list[9]
plot_list[10]
dev.off()

# also make combined plots
df = expand.grid(subarea = unique(c(pred_list[[1]]$subarea,pred_list[[2]]$subarea)),
                 sector2 = unique(c(pred_list[[1]]$sector2,pred_list[[2]]$sector2)),
                 year = unique(c(pred_list[[1]]$year,pred_list[[2]]$year)))

newdf = left_join(df, dplyr::select(pred_list[[1]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Aggregate")
newdf_ind = left_join(df, dplyr::select(pred_list[[2]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Individual")
newdf = rbind(newdf,newdf_ind)

p1 = ggplot(newdf, aes(year, fit, col=Model, fill=Model)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-2*se, ymax=fit+2*se),alpha=0.5) +
  facet_wrap(subarea~sector2,scale="free_y", ncol = length(unique(newdf$sector2))) +
  theme_bw() + xlab("") + ylab("ln Inertia")

newdf = left_join(df, dplyr::select(pred_list[[3]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Aggregate")
newdf_ind = left_join(df, dplyr::select(pred_list[[4]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Individual")
newdf = rbind(newdf,newdf_ind)

p2 = ggplot(newdf, aes(year, fit, col=Model, fill=Model)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-2*se, ymax=fit+2*se),alpha=0.5) +
  facet_wrap(subarea~sector2,scale="free_y",ncol=length(unique(newdf$sector2))) +
  theme_bw() + xlab("") + ylab("ln Distance")

newdf = left_join(df, dplyr::select(pred_list[[5]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Aggregate")
newdf_ind = left_join(df, dplyr::select(pred_list[[6]], year, sector2, subarea, fit, se)) %>%
  dplyr::mutate("Model"="Individual")
newdf = rbind(newdf,newdf_ind)

p3 = ggplot(newdf, aes(year, fit, col=Model, fill=Model)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-2*se, ymax=fit+2*se),alpha=0.5) +
  facet_wrap(subarea~sector2,scale="free_y",ncol=length(unique(newdf$sector2))) +
  theme_bw() + xlab("") + ylab("ln Days")

pdf(paste0("output/",scale, "_gam-combined_",data,"_", split_wc,".pdf"))
p1
p2
p3
dev.off()
