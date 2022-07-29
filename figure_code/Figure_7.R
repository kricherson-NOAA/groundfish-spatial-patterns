
data <- c("Alaska", "WC")[1]
cs_sensitivity <- c(TRUE,FALSE)[2]

if(cs_sensitivity)
{
  cs_sens_label = "stayers"
}else{
  cs_sens_label = "allvessels"
}

df_table <- read.csv(paste0("output/table_catchshares_",data,"_",cs_sens_label,".csv"))

# parse run name
df_table$Scale <- "Aggregate"
df_table$Scale[grep("individual", df_table$Run)] <- "Individual"
df_table$Metric <- unlist(lapply(strsplit(df_table$Run," "), getElement, 1))

# make caterpillar plot of the metrics v catch share effects
g1 <- ggplot(df_table, aes(Metric, Est,col=Scale)) +
  geom_abline(aes(intercept=0,slope=0), col="grey80") +
  geom_pointrange(aes(ymin=Est-1.96*SE, ymax = Est+1.96*SE),
                  position=position_dodge(0.5)) +
  facet_wrap(~Sector, scale="free") +
  xlab("") + ylab("") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white")) +
  coord_flip()
ggsave(g1, filename = paste0("figures/catchshare_effects",data,"_",cs_sens_label,".png"))
