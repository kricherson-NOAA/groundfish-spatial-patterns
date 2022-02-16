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
data = area_permit_cog
data$port = as.factor(data$v)
data$sector2 = as.factor(data$sector2)
data$subarea = as.factor(data$subarea)
# only use ports with 10 or more years of data
data = group_by(data, port) %>%
  dplyr::mutate(nyear = length(unique(year))) %>%
  dplyr::filter(nyear>=10) %>%
  dplyr::select(-nyear)

# single common pattern, port random effect
fit0 = gam(log(total_sd) ~  sector2 * subarea + s(year,k=8) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))

# sector - specific year and subarea smooths
fit1 = gam(log(total_sd) ~ sector2 * subarea + s(year,k=8,by=sector2) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))
fit2 = gam(log(total_sd) ~ sector2 * subarea + s(year,k=8,by=subarea) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))

# shared year smooth and separate factor smooths
fit3 = gam(log(total_sd) ~ sector2 * subarea + s(year, k=8, m=2) + s(year, sector2, k=8, bs="fs", m=2) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))
fit4 = gam(log(total_sd) ~ sector2 * subarea + s(year, k=8, m=2) + s(year, subarea, k=8, bs="fs", m=2) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))

# no shared year smooth but separate factor smooths
fit5 = gam(log(total_sd) ~ sector2 * subarea + s(year, sector2, k=8, bs="fs", m=2) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))
fit6 = gam(log(total_sd) ~ sector2 * subarea + s(year, subarea, k=8, bs="fs", m=2) + s(port,bs="re"), data = dplyr::filter(data, total_sd>0))

# port level smooths -- may be getting too fine here
fit7 = gam(log(total_sd) ~ sector2 * subarea + s(year, sector2, k=8, bs="fs", m=2) + s(year, port, k=8, bs="fs", m=2), data = dplyr::filter(data, total_sd>0))
fit8 = gam(log(total_sd) ~ sector2 * subarea + s(year, subarea, k=8, bs="fs", m=2) + s(year, port, k=8, bs="fs", m=2), data = dplyr::filter(data, total_sd>0))

data$sector_area = as.factor(paste(data$sector2, data$subarea))
m0 = glmmTMB(log(total_sd) ~ sector2 + subarea + (1|year) + (1|port), data = dplyr::filter(data, total_sd>0))
m1 = glmmTMB(log(total_sd) ~ (1|sector_area) + (1|year) + (1|port), data = dplyr::filter(data, total_sd>0))
m2 = glmmTMB(log(total_sd) ~ sector2 + subarea + as.factor(year) + (1|port), data = dplyr::filter(data, total_sd>0))
m3 = glmmTMB(log(total_sd) ~ sector_area + (1|year) + (1|port), data = dplyr::filter(data, total_sd>0))
m4 = glmmTMB(log(total_sd) ~ (1|sector_area) + subarea + (1|sector2:subarea) + (1|port), data = dplyr::filter(data, total_sd>0))

