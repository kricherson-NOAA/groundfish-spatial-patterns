library(glmmTMB)
library(mgcv)
library(dplyr)
data <- c("Alaska", "WC")[1]
scale = c("region","port")[2]

# these dataframes get created by 01_regional_summaries.r
area_permit_cog = readRDS(paste0("data/",data,"_",scale,"_cog",".rds"))
area_permit_cog_ind = readRDS(paste0("data/",data,"_",scale,"_cog-ind",".rds"))
haul_dist = readRDS(paste0("data/",data,"_",scale,"_hauldist",".rds"))
haul_dist_ind = readRDS(paste0("data/",data,"_",scale,"_hauldist-ind",".rds"))

# using area permit_cog as an example, the predictor variables here are:
# year, port (v) / subarea, and sector2.

# Example questions:
# - are trends in inertia explained better by region (subarea), sector,
# or is there no common pattern?
dat = area_permit_cog
dat$port = as.factor(dat$v)
dat$sector2 = as.factor(dat$sector2)
dat$subarea = as.factor(dat$subarea)
dat$sector_subarea = as.factor(paste(dat$sector2, dat$subarea))
# only use ports with 10 or more years of data
dat = group_by(dat, port) %>%
  dplyr::mutate(nyear = length(unique(year))) %>%
  dplyr::filter(nyear>=10) %>%
  dplyr::select(-nyear)

dat$weights = (1/dat$total_cv) # weights are 1/CV
dat$weights = dat$weights/mean(dat$weights,na.rm=T)# normalize

# port level smooths -- may be getting too fine here
# global year effect, with port level smooths
fit1 = gam(log(total_sd) ~ sector2 * subarea + s(year, k=8) + s(year, port, k=8, bs="fs", m=2), weights = weights,data = dplyr::filter(dat, total_sd>0))
# sector specific year effects, with port level smooths
fit2 = gam(log(total_sd) ~ sector2 * subarea + s(year, sector2, k=8, bs="fs", m=2) + s(year, port, k=8, bs="fs", m=2), weights = weights,data = dplyr::filter(dat, total_sd>0))
# area specific year effects, with port level smooths
fit3 = gam(log(total_sd) ~ sector2 * subarea + s(year, subarea, k=8, bs="fs", m=2) + s(year, port, k=8, bs="fs", m=2), weights = weights,data = dplyr::filter(dat, total_sd>0))
# area and sector specific year effects (non-interacting), with port level smooths
fit4 = gam(log(total_sd) ~ sector2 * subarea + s(year, sector2, k=8, bs="fs", m=2)+ s(year, subarea, k=8, bs="fs", m=2) + s(year, port, k=8, bs="fs", m=2), weights = weights,data = dplyr::filter(dat, total_sd>0))
# area and sector specific year effects (interacting), with port level smooths
fit5 = gam(log(total_sd) ~ sector2 * subarea + s(year, sector_subarea, k=8, bs="fs", m=2)+ s(year, port, k=8, bs="fs", m=2), weights = weights,data = dplyr::filter(dat, total_sd>0))

newdata = expand.grid(year = unique(dat$year),
                     sector2 = unique(dat$sector2),
                     subarea = unique(dat$subarea))
newdata = dplyr::filter(newdata,!is.na(sector2), !is.na(subarea))
newdata$sector_subarea = as.factor(paste(newdata$sector2, newdata$subarea))
newdata = dplyr::filter(newdata,sector_subarea %in% unique(data$sector_subarea))
newdata$port = "new_port"
model_pred <- predict(fit5, newdata=newdata, se.fit=TRUE) # predict to new port -- will be same as mean

newdata = cbind(newdata, model_pred)

pdf(paste0("output/",scale, "_summaries_",data,"_", split_wc,".pdf"))

ggplot(newdata, aes(year, exp(fit), col=sector2, fill=sector2)) +
  geom_line() +
  geom_ribbon(aes(ymin=exp(fit-2*se.fit), ymax=exp(fit+2*se.fit)),alpha=0.5) +
  facet_wrap(subarea~sector2,scale="free_y") +
  theme_bw() + xlab("") + ylab("Inertia")

dev.off()


