require(latex2exp)
library(viridis)
library(cowplot)
library(reshape2)
UltimateFactor %>% 
  ggplot(aes(x=LK_CODE, y=(SUM))) + 
  geom_point(aes(x=LK_CODE, y= SUM, color="SUM"), size=2, alpha=0.5) + 
  geom_point(aes(x=LK_CODE, y=(ELISA), color="ELISA"), size = 2, alpha=0.5) +  
  theme_classic() + ylab(TeX('$\\mu$g/L of MC')) + xlab( "Lakes")  + 
  theme(
    legend.title=element_blank(), 
    legend.position="bottom", 
    axis.text.x = element_text(
      angle = 90, 
      size = 7,
      vjust = 0.4)
  ) + 
  geom_hline(
    yintercept = 4, 
    color="#BB0000", 
    linetype="dashed", 
    show.legend = FALSE
  ) +  
  geom_text(aes(17,4.9, label = "EPA Guideline"), color="#BB0000", show.legend=FALSE) +
  scale_color_manual(labels = c("MC from ELISA", "MC from LC-MS/MS"), values = c("green", "black"))
cairo_ps(filename = "Microcystin.eps",
         width = 7, height = 7, pointsize = 12,
         fallback_resolution = 300)
print(x)
dev.off()
x

######################################################################################################
Delta %>% UltimateFactor %>% mutate(delta=SUM-ELISA)

######################################################################################################
July <- Delta %>% filter(Month == "7") %>% gather(congener, value, Nodul:MC_LF)
August <- Delta %>% filter(Month == "8") %>% gather(congener, value, Nodul:MC_LF)
Sept <- Delta %>% filter(Month == "9") %>% gather(congener, value, Nodul:MC_LF)
Oct <- Delta %>% filter(Month == "10") %>% gather(congener, value, Nodul:MC_LF)

july <- Delta %>% filter(Month == "7")
august <- Delta %>% filter(Month == "8")
sept <- Delta %>% filter(Month == "9")
oct <- Delta %>% filter(Month == "10")
melt.july <- july %>% select(LK_CODE, SUM, ELISA) %>% melt()
melt.august <- august %>% select(LK_CODE, SUM, ELISA) %>% melt()
melt.sept <- sept %>% select(LK_CODE, SUM, ELISA) %>% melt()
melt.oct <- oct %>% select(LK_CODE, SUM, ELISA) %>% melt() 
####################################################################################################
a<-melt.july %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

b<-melt.august %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

c<-melt.sept %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

d<-melt.oct %>% 
  ggplot(aes(x=LK_CODE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge", color="black")  + 
  theme_classic() + 
  scale_fill_viridis(discrete = T, option = "E",
                     label=c("LC-MS/MS", "ELISA")) +
  theme(axis.text.x = element_text(angle = 90, size = 7, vjust = 0.4),
        legend.key.height = unit(1.8,"line"),
        legend.title  = element_blank()) +
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

legend_b <- get_legend(a + theme(legend.position = "bottom", legend.key.size = unit(0.2,"cm")))
dd <- plot_grid(a + theme(legend.position = "none"),
          b + theme(legend.position = "none"),
          c + theme(legend.position = "none"),
          d + theme(legend.position = "none"), 
          labels = c("A", "B", "C", "D"),
          align = 'vh',
          ncol = 2)
plot_grid(dd, legend_b, ncol=1, rel_heights = c(1, .2), rel_widths = c(1,0.2))
ggsave("compare.eps", device="eps")
######################################################################################################
## month.eps##
a<-July %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
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
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab(" ")

b<-August %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
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
  ylab(" ") +
  xlab(" ")

c<-Sept %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
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
  ylab(TeX(' MC ($\\mu$g/L)')) +
  xlab("Lakes")

d<-Oct %>% 
  ggplot(aes(x=LK_CODE)) + 
  geom_bar(stat="identity", aes(y=value, fill=congener, width=1))  + 
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
  ylab("  ") +
  xlab("Lakes")

legend_b <- get_legend(a + theme(legend.position = "bottom", legend.key.size = unit(0.3,"cm")))
dd <- plot_grid(a + theme(legend.position = "none"),
          b + theme(legend.position = "none"),
          c + theme(legend.position = "none"),
          d + theme(legend.position = "none"), 
          labels = c("A", "B", "C", "D"),
          align = 'vh',
          ncol = 2)
plot_grid(dd, legend_b, ncol=1, rel_heights = c(1, .1), rel_widths = c(1,0.2))
ggsave("month.eps", device="eps")
