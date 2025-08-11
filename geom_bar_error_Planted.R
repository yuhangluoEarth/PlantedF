library(ggplot2)
require(devEMF)
library(dplyr)
library(reshape2)
library(zyp)

realm.melt.all2 <- read.csv("zSample_Planted.csv")%>%filter(!is.na(Type))
realm.melt.all2 <- within(realm.melt.all2, xtype <- factor(xtype,levels = c("OBS","XGBoost","MMEM")))

p <- ggplot(data = realm.melt.all2,aes(x = xtype,y = MEAN, fill = Type))+
  geom_bar(stat = "identity",
           position = position_dodge2(padding = 0.05),width = 0.6,size =0.1)+
  scale_fill_manual(values =c("#fdcc09","#005c3a","#f39634","#fdcc09","#e72c6a","#97d346","#005c3a","#e72c6a","#e72c6a","#009bfa",
                              "#77e0d4","#79a2e8","#009bfa","#009bfa")) +
  geom_errorbar(aes(ymin = MEAN-STD,
                    ymax = MEAN+STD),position = position_dodge2(width = 0.6,padding = 0.7), size = 0.2,width = 0.6)+
  geom_point(position = position_dodge2(width = 0.6,padding = 0.6),size = 0.6,color = "black")+
  geom_text(aes(y = ifelse(MEAN>0,MEAN,MEAN-0.004),label = Text),position = position_dodge2(width = 0.6,padding = 0.6),size =4)+
  geom_hline(aes(yintercept = 0),color = "black",width = 1)+
  theme_bw()+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4),limits = c(-0.06,0.4))+
  xlab('')+
  ylab(expression(paste("Trend in LAI ",(m^2*m^-2*decade^-1))))+
  theme(axis.title.y = element_text(size = 10),
        axis.text = element_text(colour = "black"))+
  theme(panel.grid = element_blank())
p
ggsave(p, filename = 'Driver_Planted.pdf',width = 6,height = 3)
