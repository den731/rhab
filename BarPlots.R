## bar16srna.eps
library(cowplot)
library(viridis)
library(latex2exp)
library(viridis)
library(cowplot)

#########################################################################################################
# CHANGE THIS!!
A<-UltimateFactor  %>%  ggplot(aes(LK_CODE,S_SUM )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                     "white",
                                                                   na.rm = T,
                                                                   color=
                                                                     "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab("16S rRNA (Genecopies/mL)")
A
ggsave("bar16srna.eps", device="eps")

B<-UltimateFactor  %>%  ggplot(aes(LK_CODE,X16SRNA )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                       "white",
                                                                     na.rm = T,
                                                                     color=
                                                                       "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Sites ") + 
  ylab("16S rRNA (Genecopies/mL)")

#Combined figures
plot_grid(A,B, labels = c("A", "B"), align = "v", ncol = 1)
ggsave("responsecombine.eps",height = 9, units = "in", device="eps")

###########################################################################################################
## Barplot Nutrients nutboxlake
A <-  UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=OP)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

B <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=NO3)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

C <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=NH3)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

D <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=TP)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

E <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=TKN)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

F <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=TN)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

plot_grid(A,B,C,D,E,F,labels = c("A", "B", "C","D","E", "F"), align = "v", ncol = 2)
ggsave("nutboxplotlake.eps",height = 9 , units = "in", device="eps")

#################################################################################################
## barmcsum.eps Averaged Grab Samples

A<-UltimateFactor  %>%  ggplot(aes(LK_CODE,SUM )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                   "white",
                                                                 na.rm = T,
                                                                 color=
                                                                   "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab(TeX('Average MC ($\\mu$g/L)'))

UltimateFactor  %>%  ggplot(aes(LK_CODE,SUM )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                "white",
                                                              na.rm = T,
                                                              color=
                                                                "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab(" ") +
  ylab(TeX('Average MC ($\\mu$g/L)'))
ggsave("barmcsum.eps", device="eps")
######################################################################################################
# ZEBRAs

UltimateFactor  %>%  ggplot(aes(LK_CODE,MusselMass )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                       "white",
                                                                     na.rm = T,
                                                                     color=
                                                                       "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab(TeX('Mussel Mass (g)'))


######################################################################################################
#### [hobo.eps]

setEPS()
postscript(file="hobo.eps", width = 8, height = 10)
temp1<-UltimateFactor$wtemp
temp2<-UltimateFactor$hobotemp
light1<-UltimateFactor$hobolight
month1<-UltimateFactor$Month
par(mfcol=c(3,1))
boxplot(temp1~month1, xaxt='n', ann=F, xlab = " ", ylab="Water Temperature from multimeter (Celcius)")
title("A" , adj = 0)
boxplot(temp2~month1, xaxt='n', ann=F, xlab = " ", ylab="Water Temperature from HOBO loggers (Celcius)")
# axis(side =1,at=1:4,labels = c("July", "August", "September", "October"))
title("B", adj = 0)
boxplot(log(light1+1)~month1, xaxt='n', ann=F, xlab = "Month", ylab="Light Intensity (log10(lux))")
axis(side =1,at=1:4,labels = c("July", "August", "September", "October"))
title("C", adj = 0)
dev.off()
######################################################################################################
# [congenerbar.eps]

cong_sum <- UltimateFactor %>% gather(congener, average, Nodul:MC_LF) %>%
  group_by(LK_CODE, congener) %>%
  summarise_each(funs(mean),average_average=average)
a<-cong_sum %>% 
  ggplot(aes(x=LK_CODE, y=average_average, fill=congener, width=1)) + 
  geom_bar(stat="identity")  + 
  scale_fill_viridis(discrete=TRUE,option="B",
                     label=c( "[D-Asp3] MC-LR",
                              "[D-Asp3] MC-RR", 
                              "MC-HilR",
                              "MC-HtyR",
                              "MC-LA",
                              "MC-LF",
                              "MC-LR",
                              "MC-LW",
                              "MC-LY",
                              "MC-RR", 
                              "MC-WR",
                              "MC-YR", 
                              "Nodularin")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX('Average MC ($\\mu$g/L)')) +
  xlab("Lakes")

congenerssz <-
  UltimateFactor %>% gather(Congener, Concentrations, Nodul:MC_LF)

b<-congenerssz %>%
  ggplot(aes(x=reorder(Congener, desc(Congener)), y=Concentrations, width=0.3, fill=Congener, show)) +
  stat_summary(fun.y=mean, geom = "bar",na.rm = T,color="black", show.legend = NA) + 
  theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  #scale_x_discrete(label=c(
  #  "Nodularin",
  #  "MC-YR",
  #  "MC-WR",
  #  "MC-RR",
  #  "MC-LY",
  #  "MC-LW",
  #  "MC-LR",
  #  "MC-LF", 
  # "MC-LA", 
  #  "MC-HtyR",
# "MC-HilR", 
#  "[D-Asp3] MC-RR",
#  "[D-Asp3] MC-LR" 
#  )) +
ylab(TeX('Average MC($\\mu$g/L)}'))  + 
  xlab(" ") +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_text(2),
        axis.title.x=element_text(size=10),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        axis.line=element_blank()) +
  scale_fill_viridis(discrete=TRUE,option="B", guide = FALSE) +
  coord_flip() 

plot_grid(a,b, ncol = 2,
          rel_heights = c(10, 0.1),
          labels=c('A' , 'B'),
          rel_widths = c(1,1/3))
ggsave("congenerbar.eps", device = "eps" , width = 6, height = 5.5)

######################################################################################################
## spatttboxplotlake.eps
A <-  UltimateFactor %>%
  ggplot(aes(x=LK_CODE, y=SUM)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ") +
  ylab("MC (ppb)")

B <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=S_SUM)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab("MC in ng / g of resin per day ")
C <-  UltimateFactor %>% 
  mutate(Lat=as.factor(round(Lat,2))) %>%
  ggplot(aes(x=Lat, y=SUM)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab("") +
  ylab(" ")

D <- UltimateFactor %>% 
  mutate(Lat=as.factor(round(Lat,2))) %>%
  ggplot(aes(x=Lat, y=S_SUM)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab("Latitude") +
  ylab(" ")
plot_grid(A,C,B,D, labels ="AUTO", align = "v", ncol = 2, hjust = -0.1)
ggsave("spatttboxplotlake.eps", device="eps")

######################################################################################################
## watboxplotlake
A <-  UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=pH)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

B <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=conduc)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

C <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=turb)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

D <- UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, DO)) + 
  stat_summary(fun.y=mean, geom = "bar", fill="white",
               na.rm = T,
               color="black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4)) +
  xlab(" ")

plot_grid(A,B,C,D,labels = c("A", "B", "C","D"), align = "v", ncol = 2)
ggsave("watboxplotlake.eps",height = 9, units = "in", device="eps")




######################################################################################################

library(latex2exp)
library(tidyverse)
library(viridis)
library(cowplot)

cong_spatt <- UltimateFactor %>% gather(congener, average, S_D_Asp3_RR:S_MC_LW) %>% group_by(LK_CODE, congener) %>%
  summarise_at(vars(average), mean, na.rm=TRUE)



a<-  cong_spatt %>% 
  ggplot(aes(x=LK_CODE, y=average, fill=congener, width=1)) + 
  geom_bar(stat="identity")  + 
  scale_fill_viridis(discrete=TRUE,option="B", label=c( "[D-Asp3] MC-RR","[D-Atsp3] MC-LR",    "MC-HilR", "MC-HtyR","MC-LA", "MC-LR",  "MC-LW", "MC-LY","MC-RR", "MC-WR", "MC-YR", "Nodularin")) +
  
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4),
        legend.key.height = unit(1.46,"line"),
        legend.title  = element_blank()) +
  ylab(TeX('Average Concent. (ng of MC / g of resin per day)')) +
  xlab("Lakes")

cong_spatt1 <- UltimateFactor %>% gather(congener, average, S_D_Asp3_RR:S_MC_LW) 


b<- cong_spatt1 %>%
  ggplot(aes(x=reorder(congener, desc(congener)), y=average, width=0.3, fill=congener, show)) +
  stat_summary(fun.y=mean, geom = "bar",na.rm = T,color="black", show.legend = NA) + 
  theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_text(size=7),
        
        panel.grid.major=element_blank(),
        axis.line=element_blank()) +
  scale_fill_viridis(discrete=TRUE,option="B", guide = FALSE) +
  #scale_x_discrete(label=c(
  #  "Nodularin",
  #  "MC-YR",
  #  "MC-WR",
  #  "MC-RR",
  #  "MC-LY",
  #  "MC-LW",
  #  "MC-LR",
  #  "MC-LF", 
  # "MC-LA", 
  #  "MC-HtyR",
# "MC-HilR", 
#  "[D-Asp3] MC-RR",
#  "[D-Asp3] MC-LR" 
#  )) +
ylab("Average Concent. (ng of MC / g of resin per day)")  + 
  xlab(" ") +
  coord_flip() 

plot_grid(a,b, ncol = 2, rel_heights = c(10, 0.1),rel_widths = c(1,1/3))

ggsave("barspatts.eps", device="eps")


UltimateFactor %>% 
  gather(Congener, Concentrations, S_D_Asp3_RR:S_MC_LW) %>%
  ggplot(aes(Congener,Concentrations )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                         "white",
                                                       na.rm = T,
                                                       color=
                                                         "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  
  xlab("Congeners") +
  ylab("Concentrations (ng of MC / g of resin per day)") +
  
  scale_x_discrete(label=c( "[D-Asp3] MC-RR","[D-Asp3] MC-LR",    "MC-HilR", "MC-HtyR","MC-LA", "MC-LR",  "MC-LW", "MC-LY","MC-RR", "MC-WR", "MC-YR", "Nodularin")) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) 


######################################################################################################
