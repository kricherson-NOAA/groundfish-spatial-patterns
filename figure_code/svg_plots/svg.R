Figure_3_combined <- readRDS("~/Downloads/richerson_paper/Figure_3_combined.rds")
Figure_4_combined <- readRDS("~/Downloads/richerson_paper/Figure_4_combined.rds")
Figure_5_combined <- readRDS("~/Downloads/richerson_paper/Figure_5_combined.rds")
Figure_6_combined <- readRDS("~/Downloads/richerson_paper/Figure_6_combined.rds")
Figure_7_combined <- readRDS("~/Downloads/richerson_paper/Figure_7_combined.rds")

svg("Fig3.svg")
Figure_3_combined
dev.off()

svg("Fig4.svg")
Figure_4_combined
dev.off()

svg("Fig5.svg")
Figure_5_combined
dev.off()

svg("Fig6.svg", height = 7, width = 7.5)
Figure_6_combined
dev.off()

svg("Fig7.svg", height = 7, width = 7.5)
Figure_7_combined
dev.off()