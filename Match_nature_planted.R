library(dplyr)
library(MatchIt)
library(openxlsx)
library(Hmisc)
load("PSM_Data.RData")
############MATCH
m.out <- matchit(If_planted ~ prec_yr_sl + temp_yr_sl + DEM + Slope + HFP2010 + MeanLAI + ForestAge + Spe_rich + vpd_Slp + soil_Slp + srad_Slp,  #+LUCC_mean
                 data = data.Globe.Point,replace = TRUE,exact = c("ECO_ID","ForestType"),
                 method = "nearest",ratio=1,caliper=0.25)


m.data.i <- data.frame(m.out$match.matrix)
m.data.i <- transform(m.data.i,Order_reshape = rownames(m.data.i))
######################
######################
setwd('W:\\PA_CN_Planted\\RCode\\Step1_MatchLCC\\Result\\Planted_Nature\\Have_forest_type2_025')
write.csv(m.data.i,'matchID_Planted_Nature_ForestType.csv')
######################
######################
m_out <- summary(m.out, interactions = F)
write.xlsx(data.frame(rownames(m_out$sum.all),m_out$sum.all),"summary_matchit_all.xlsx")
write.xlsx(data.frame(rownames(m_out$sum.matched),m_out$sum.matched),"summary_matchit_matched.xlsx")
write.xlsx(data.frame(rownames(m_out$reduction),m_out$reduction),"summary_matchit_reduction.xlsx")

################
pdf(file = 'histogram.pdf',height = 7, width = 8)
plot(m.out, type = "histogram")
dev.off()
pdf(file = 'love_plot2.pdf',height = 5, width = 6)
plot(summary(m.out, interactions = F),
     var.order = "unmatched")
dev.off()

