library(cowplot)
library(viridis)
library(latex2exp)
library(viridis)
library(cowplot)
library(reshape2)
library(dplyr)
##################### Import Data #################
MCSUMresponseAverage <- read.csv("MCSUMresponseAverage.csv", header = TRUE)
LOGTransformed <- read.csv("LOGTransformed.csv", header = TRUE)
UltimateFactor <- read.csv("UltimateFactor.csv", header = TRUE)

# 16s rRNA averaged########################################################################################################
# bar16srna.eps
A<-UltimateFactor  %>%  ggplot(aes(LK_CODE,X16SRNA )) + stat_summary(fun.y=mean, geom = "bar", fill=
                                                                     "white",
                                                                   na.rm = T,
                                                                   color=
                                                                     "black") + theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab("16S rRNA (Genecopies/mL)")
ggsave("bar16srna.eps", device="eps")

# Filter by month #############################################################
July <- UltimateFactor %>%
  filter(Month == "7") %>% 
  gather(congener, value, X16SRNA)
August <- UltimateFactor %>% 
  filter(Month == "8") %>% 
  gather(congener, value, X16SRNA)
Sept <- UltimateFactor %>% 
  filter(Month == "9") %>% 
  gather(congener, value, X16SRNA)
Oct <- UltimateFactor %>% 
  filter(Month == "10") %>% 
  gather(congener, value, X16SRNA)

# 16s rRNA monthly graph ########################################################
a <- LOGTransformed %>%
  filter(Month == "7") %>%
  select(LK_CODE, X16SRNA,MCYE) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_discrete(label=c("16s rRNA  ", "mcyE")) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab("Log(cp/L)") +
  xlab(" ") +
  scale_y_continuous(limits=c(0,7))

b <- LOGTransformed %>%
  filter(Month == "8") %>%
  select(LK_CODE, X16SRNA,MCYE) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab(" ") +
  scale_y_continuous(limits=c(0,7))
  
c <- LOGTransformed %>%
  filter(Month == "9") %>%
  select(LK_CODE, X16SRNA,MCYE) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  xlab("Lakes") +
  ylab("Log(cp/L)") +
  scale_y_continuous(limits=c(0,7))

d <- LOGTransformed %>%
  filter(Month == "10") %>%
  select(LK_CODE, X16SRNA,MCYE) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab("Lakes") +
  scale_y_continuous(limits=c(0,7))

legend_b <- get_legend(a + 
                         theme(legend.position = "bottom", 
                               legend.key.size = unit(0.3,"cm")) +
                         guides(colours = guide_legend(nrow = 1 )))

dd <- plot_grid(a + theme(legend.position = "none"),
                b + theme(legend.position = "none"),
                c + theme(legend.position = "none"),
                d + theme(legend.position = "none"), 
                labels = c("A", "B", "C", "D"),
                align = 'vh',
                ncol = 2)
plot_grid(dd, legend_b, ncol=1, nrow = 2, rel_heights = c(1, .3), rel_widths = c(1,0.2))
ggsave("gene.eps", device="eps")

# chloro and phyco ################################################
a <- UltimateFactor %>%
  filter(Month == "7") %>%
  select(LK_CODE, chloro,phyco) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values=c("#078907","#890770")) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab("RFU") +
  xlab(" ") +
  scale_y_continuous(limits=c(0,4.5))

b <- UltimateFactor %>%
  filter(Month == "8") %>%
  select(LK_CODE, chloro,phyco) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values=c("#078907","#890770")) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab(" ") +
  scale_y_continuous(limits=c(0,4.5))
  

c <- UltimateFactor %>%
  filter(Month == "9") %>%
  select(LK_CODE, chloro,phyco) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values=c("#078907","#890770")) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  xlab("Lakes") +
  ylab("RFU") +
  scale_y_continuous(limits=c(0,4.5))
  

d <- UltimateFactor %>%
  filter(Month == "10") %>%
  select(LK_CODE, chloro,phyco) %>%
  melt() %>%
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values=c("#078907","#890770")) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(" ") +
  xlab("Lakes") +
  scale_y_continuous(limits=c(0,4.5))
 

legend_b <- get_legend(a + 
                         theme(legend.position = "bottom", 
                               legend.key.size = unit(0.3,"cm")) +
                         guides(colours = guide_legend(nrow = 1 )))

dd <- plot_grid(a + theme(legend.position = "none"),
                b + theme(legend.position = "none"),
                c + theme(legend.position = "none"),
                d + theme(legend.position = "none"), 
                labels = c("A", "B", "C", "D"),
                align = 'vh',
                ncol = 2)
plot_grid(dd, legend_b, ncol=1, nrow = 2, rel_heights = c(1, .3), rel_widths = c(1,0.2))
ggsave("floro.eps", device="eps")

# Barplot Nutrients nutboxlake##########################################################################################################

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

# barmcsum.eps Averaged Grab Samples##############################################################

A<-UltimateFactor  %>%  
  ggplot(aes(LK_CODE,SUM )) +
  stat_summary(fun.y=mean, 
               geom = "bar", 
               fill="white",
               na.rm = T,
               color="black") + 
  theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab(TeX('Average MC ($\\mu$g/L)'))

UltimateFactor  %>%  
  ggplot(aes(LK_CODE,SUM )) + 
  stat_summary(fun.y=mean, 
               geom = "bar", 
               fill="white",
               na.rm = T,
               color= "black") +
  theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab(" ") +
  ylab(TeX('Average MC ($\\mu$g/L)'))
ggsave("barmcsum.eps", device="eps")
# ZEBRAs#################################################################################

UltimateFactor  %>% 
  ggplot(aes(LK_CODE,MusselMass )) + 
  stat_summary(fun.y=mean, 
               geom = "bar",
               fill="white",
               na.rm = T,
               color="black") + 
  theme_cowplot() +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.5) + 
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4)) +
  xlab("Lake Site") +
  ylab(TeX('Mussel Mass (g)'))


# [hobo.eps] #####################################################################################################


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

# [congenerbar.eps] ######################################

cong_sum <- UltimateFactor %>% 
  gather(congener, average, Nodul:MC_LF) %>%
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

# spatttboxplotlake.eps ##################################
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

# watboxplotlake ###################################################################################################
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




# spatts bar graph #######################################
cong_spatt <- UltimateFactor %>% 
  gather(congener, average, S_D_Asp3_RR:S_MC_LW) %>% 
  group_by(LK_CODE, congener) %>%
  summarise_at(vars(average), mean, na.rm=TRUE)



a<-  cong_spatt %>% 
  ggplot(aes(x=LK_CODE, y=average, fill=congener, width=1)) + 
  geom_bar(stat="identity")  + 
  scale_fill_viridis(discrete=TRUE,option="B", label=c( "[D-Asp3] MC-RR",
                                                        "[D-Atsp3] MC-LR",
                                                        "MC-HilR",
                                                        "MC-HtyR",
                                                        "MC-LA", 
                                                        "MC-LR",  
                                                        "MC-LW", 
                                                        "MC-LY","MC-RR",
                                                        "MC-WR", 
                                                        "MC-YR", 
                                                        "Nodularin")) +
  
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

# simple congener of spatts ###############################
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
