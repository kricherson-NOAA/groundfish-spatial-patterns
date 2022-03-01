library(mgcv)
library(dplyr)
library(gratia)

data <- c("Alaska", "WC")[2]
scale = c("region","port")[2]

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
for(run in c("area_permit_cog","area_permit_cog-ind","haul_dist","haul_dist-ind","eff_days","eff_days-ind")) {

  if(run=="area_permit_cog") dat = area_permit_cog
  if(run=="area_permit_cog-ind") dat = area_permit_cog_ind
  if(run=="haul_dist") dat = haul_dist
  if(run=="haul_dist-ind") dat = haul_dist_ind
  if(run=="eff_days") dat = eff_days
  if(run=="eff_days-ind") dat = eff_days_ind

  if(run%in% c("area_permit_cog","area_permit_cog-ind")) {
    ylabel = "ln sqrt(inertia)"
    dat$response = log(dat$total_sd)
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
    dplyr::summarise(n = n()) %>% dplyr::filter(n > quantile(n,0.25))
  #Need to adjust this for WC so that we don't exclude at-sea hake, which only has one "port"
  if(data == "WC")
  {
    dat = dplyr::filter(dat, sector_subarea %in% not_rare$sector_subarea | sector2 == "At-sea hake")
  }else{
    dat = dplyr::filter(dat, sector_subarea %in% not_rare$sector_subarea)
  }
  dat$sector_subarea = as.factor(as.character(dat$sector_subarea))
  # port level smooths -- may be getting too fine here
  # global year effect, with port level smooths (port = v)
  fit = list()
  K = 10
  fit[[1]] = gam(response ~ sector2 * subarea + s(year,k=K) + s(year, port, k=K, bs="fs", m=2), weights = weights,data = dat)
  # sector specific year effects, with port level smooths
  fit[[2]] = gam(response ~ subarea + s(year, sector2, k=K, bs="fs", m=2) + s(year, port, k=K, bs="fs", m=2), weights = weights,data = dat)
  # area specific year effects, with port level smooths
  fit[[3]] = gam(response ~ sector2 + s(year, subarea, k=K, bs="fs", m=2) + s(year, port, k=K, bs="fs", m=2), weights = weights,data = dat)
  # area and sector specific year effects (non-interacting), with port level smooths
  fit[[4]] = gam(response ~ s(year, sector2, k=K, bs="fs", m=2)+ s(year, subarea, k=K, bs="fs", m=2) + s(year, port, k=K, bs="fs", m=2), weights = weights,data = dat)
  # area and sector specific year effects (interacting), with port level smooths
  fit[[5]] = gam(response ~ s(year, sector_subarea, k=K, bs="fs", m=2)+ s(year, port, k=K, bs="fs", m=2), weights = weights,data = dat)

  # create new data to predict on -- representing mean for each area-sector
  newdata = expand.grid(year = unique(dat$year),
                        sector2 = unique(dat$sector2),
                        port = unique(dat$port))
  newdata = dplyr::filter(newdata,!is.na(sector2), !is.na(port))
  port_area = dplyr::group_by(dat, port) %>% dplyr::summarize(subarea = subarea[1])
  newdata = dplyr::left_join(newdata, port_area)
  newdata$sector_subarea = as.factor(paste(newdata$sector2, newdata$subarea))
  newdata = dplyr::filter(newdata,sector_subarea %in% unique(dat$sector_subarea))

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

  # look at individual smooths
  #sm = get_smooth(fit[[which.min(lapply(fit,AIC))]], "year")
  #sm = smooth_estimates(fit[[5]], data=newdata)
  #sm = dplyr::filter(sm, smooth=="s(year,sector_subarea)")
  #model_pred <- predict(fit[[which.min(lapply(fit,AIC))]], newdata=newdata, se.fit=TRUE) # predict to new port -- will be same as mean

  # save plots
  if(run=="area_permit_cog") {
    counter = 1
  } else {
    counter = counter + 1
  }
  plot_list[[counter]] = ggplot(newdata, aes(year, fit, col=sector2, fill=sector2)) +
    geom_line() +
    geom_ribbon(aes(ymin=fit-2*se, ymax=fit+2*se),alpha=0.5) +
    facet_wrap(subarea~sector2,scale="free_y") +
    theme_bw() + xlab("") + ylab(ylabel) + ggtitle(run)

  pred_list[[counter]] = newdata
}

pdf(paste0("output/",scale, "_gams_",data,"_", split_wc,".pdf"))
plot_list[1]
plot_list[2]
plot_list[3]
plot_list[4]
plot_list[5]
plot_list[6]
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
