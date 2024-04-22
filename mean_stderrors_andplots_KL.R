
# So, it looks like 
edited_cs_estimates_by_site <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/edited_cs_estimates_by_site.csv")
###Mean Biomass Estimates with Standard Errors 

library(plyr)

Mean_Bio_SC <- ddply(edited_cs_estimates_by_site, c("Year", "Species", "Type"), summarise,
               N  = length(Biomass_100),
               mean = mean(Biomass_100),
               sd   = sd(Biomass_100),
               se   = sd / sqrt(N)
)

Mean_Bio_SC

###Mean Density Estimates with Standard Errors

Mean_Den_SC<-ddply(edited_cs_estimates_by_site, c("Year", "Species", "Type"), summarise,
                   N  = length(Density_100),
                   mean = mean(Density_100),
                   sd   = sd(Density_100),
                   se   = sd / sqrt(N)
)

Mean_Den_SC

#write.csv(Mean_Bio_SC, "C:/Users/loughlink/Documents/SPERA Project 2015 and 16/Seal Cove/R_Depletion_SealCove/Mean_Bio_SC.csv")
#write.csv(Mean_Den_SC, "C:/Users/loughlink/Documents/SPERA Project 2015 and 16/Seal Cove/R_Depletion_SealCove/Mean_Den_SC.csv")



####Just Brook Trout

Mean_Bio_BT<-subset(Mean_Bio_SC, Mean_Bio_SC$Species=="BT")

##Change Destroyed to Compensation for graph display
Mean_Bio_BT[4,3] = "Compensation"
Mean_Bio_BT[2,3] = "Compensation"

##Make subset of two points to colour (destroyed sites)
g1<-subset(Mean_Bio_BT, Year=="1988"&Type=="Compensation")
g2<-subset(Mean_Bio_BT, Year=="1989"&Type=="Compensation")

####Added column for generation called BT_Gen
index<-c(1988, 1989, 1990, 1991, 1992, 1993, 1994, 1999, 2007, 2015, 2016)
values<-c("0", "0", "0", "1", "1", "1", "1", "2", "4", "5", "6")
Mean_Bio_BT$BT_Gen<-values[match(Mean_Bio_BT$Year, index)] # base r but like left_join
Mean_Bio_BT


library(ggplot2)

#jpeg("mean_BTbio_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_Bio_BT, aes(as.factor(Year), mean, colour=BT_Gen)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  geom_point(data=g1, colour="red") +
  geom_point(data=g2, colour="red") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())



# recreate Kristin's biomass
Mean_Bio_BT1 <- Mean_Bio_BT

Mean_Bio_BT1$Type <- as.factor(Mean_Bio_BT1$Type)
levels(Mean_Bio_BT1$Type) <- c("Compensation", "Compensation",  "Control", "Downstream")

ggplot(Mean_Bio_BT1, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())

#####Just BT density plots

Mean_Den_BT<-subset(Mean_Den_SC, Mean_Den_SC$Species=="BT")

##Change Destroyed to Compensation for graph display
Mean_Den_BT[4,3] = "Compensation"
Mean_Den_BT[2,3] = "Compensation"

##Make subset of two points to colour (destroyed sites)
d1<-subset(Mean_Den_BT, Year=="1988"&Type=="Compensation")
d2<-subset(Mean_Den_BT, Year=="1989"&Type=="Compensation")



#jpeg("mean_BTden_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_Den_BT, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Density Estimate (No./100 sq. m)") +
  xlab("Year") +
  geom_point(data=d1, colour="red") +
  geom_point(data=d2, colour="red") +
  theme(panel.grid.minor=element_blank(),
      panel.grid.major=element_blank())

dev.off()
par(mfrow=c(1,1))


#####BTYOY

Mean_Bio_BTYOY<-subset(Mean_Bio_SC, Mean_Bio_SC$Species=="BTYOY")

##Change Destroyed to Compensation for graph display
Mean_Bio_BTYOY[4,3] = "Compensation"
Mean_Bio_BTYOY[2,3] = "Compensation"

##Make subset of two points to colour (destroyed sites)
a1<-subset(Mean_Bio_BTYOY, Year=="1988"&Type=="Compensation")
a2<-subset(Mean_Bio_BTYOY, Year=="1989"&Type=="Compensation")



jpeg("mean_BTYOYbio_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_Bio_BTYOY, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1), colour="black") +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  geom_point(data=a1, shape=17, size=4) + 
  geom_point(data=a2, shape=17, size=4) +
 theme(panel.grid.minor=element_blank(),
      panel.grid.major=element_blank())

dev.off()
par(mfrow=c(1,1))

#####Just BTYOY density plots

Mean_Den_BTYOY<-subset(Mean_Den_SC, Mean_Den_SC$Species=="BTYOY")

##Change Destroyed to Compensation for graph display
Mean_Den_BTYOY[4,3] = "Compensation"
Mean_Den_BTYOY[2,3] = "Compensation"

##Make subset of two points to colour (destroyed sites)
c1<-subset(Mean_Den_BTYOY, Year=="1988"&Type=="Compensation")
c2<-subset(Mean_Den_BTYOY, Year=="1989"&Type=="Compensation")


jpeg("mean_BTYOYden_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_Den_BTYOY, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Density Estimate (No./100 sq. m)") +
  xlab("Year") +
  geom_point(data=c1, colour="red") +
  geom_point(data=c2, colour="red") +
  theme(panel.grid.minor=element_blank(),
      panel.grid.major=element_blank())

dev.off()
par(mfrow=c(1,1))

#########BT BAR PLOT
ggplot(Mean_Bio_BT, aes(x=factor(Year), y=mean, fill=Type)) + 
  geom_bar(stat="identity", position=position_dodge(1), colour="black") +
  theme_bw() + 
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  scale_fill_grey() +
  theme_bw()



####BT Point Plot 

jpeg("BT_Bio_nofacets.jpg", width=8, height=5, units='in', res=600)


Mean_Bio_BT<-subset(Mean_Bio_SC, Mean_Bio_SC$Species=="BT")

ggplot(Mean_Bio_BT, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=4, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  
  scale_colour_manual(values=c("black", "blue", "red", "dark grey"),
                      name="",
                      breaks=c("Compensation", "Control", "Destroyed", "Downstream"),
                      labels=c("Compensation", "Control", "Destroyed", "Downstream")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  theme_bw() +
 
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.15, .84)) +
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.minor=element_blank(),
      panel.grid.major=element_blank()) 

dev.off()

#####BARPLOT  ###TOO BUSY
ggplot(Mean_Bio_BT, aes(as.factor(Year), mean, fill=Type)) + 
  geom_bar(colour="black", stat="identity", width=0.5, position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  
  scale_fill_manual(values=c("blue", "black", "red", "dark grey"),
                      name="",
                      breaks=c("Compensation", "Control", "Destroyed", "Downstream"),
                      labels=c("Compensation", "Control", "Destroyed", "Downstream")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  theme_bw() +
  
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.15, .84)) +
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 

#####all SPECIES  #####Not worth using

ggplot(Mean_Bio_SC, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(Species~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") 
  


# my attempt to reproduce Kristin's values
str(edited_cs_estimates_by_site)

library(magrittr)
library(dplyr)
# this is to look at if we should use gamma
tmp <- edited_cs_estimates_by_site |>
  filter(Habitat == "Pool")
 str(tmp) 
mean(tmp$Biomass_100)
var(tmp$Biomass_100)

mean(tmp$Biomass_100)/var(tmp$Biomass_100)
mean(tmp$Biomass_100)^2/var(tmp$Biomass_100)
 
 BT1.glm <- glm(Biomass_100~Time*Treatment, family=Gamma(link=inverse), data=tmp) 
 BT1.glm <- glm(Biomass_100~Time, family=Gamma(link=inverse), data=tmp) 
 
 
 tmp$Biomass_100
tmp$l

str(Mean_Bio_SC)
BT1.glm <- glm(Biomass_100~Time*Treatment, data=tmp) 
plot(BT1.glm)

# new fig ----
# by Riffles
# tmp <- edited_cs_estimates_by_site |>
#   filter(Pool == "No" & Species == "BT") |>
#   group_by(Year, Type) |>
#   summarise(N  = length(Biomass_100),
#                      mean = mean(Biomass_100),
#                      sd   = sd(Biomass_100),
#                      se   = sd / sqrt(N))
# 
#   tmp
#   tmp$Type <- as.factor(tmp$Type)
#   levels(tmp$Type) <- c("Compensation", "Control", "Compensation", "Downstream")


  
# p1 <- ggplot(tmp, aes(as.factor(Year), mean)) + 
#   geom_point(size=4, position=position_dodge(1)) +
#   theme_bw() + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
#   facet_grid(~Type) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
#   #ylab("Mean Biomass Estimate (g/100 sq. m)") +
#   ylab("") +
#   xlab("Year") +
#   theme(panel.grid.minor=element_blank(),
#         panel.grid.major=element_blank()) +
#   geom_vline(xintercept = 3, linetype = 3)
# p1

# by Pools
# tmp1 <- edited_cs_estimates_by_site |>
#   filter(Pool == "Yes" & Species == "BT") |>
#   group_by(Year, Type) |>
#   summarise(N  = length(Biomass_100),
#             mean = mean(Biomass_100),
#             sd   = sd(Biomass_100),
#             se   = sd / sqrt(N)
#             )
# head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
# tmp1
# 
# # tmp[tmp$Type == "Downstream",]
# # tmp[tmp$Type == "Compensation",]
# # edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "BT",]
# 
# 
# tmp1$Type <- as.factor(tmp1$Type)
# levels(tmp1$Type) <- c("Compensation", "Compensation")
# tmp_row <- tmp1[1,]
# tmp_row[1,] <- NA
# tmp_row$Year <- 1990
# tmp_row$Type <- "Compensation"
# tmp_row$Type <- as.factor(tmp_row$Type)
# tmp_row$Year <- as.factor(tmp_row$Year)
# tmp1 <- rbind(tmp1, tmp_row)

# p2 <- ggplot(tmp1, aes(as.factor(Year), mean)) + 
#   geom_point(size=4, position=position_dodge(1)) +
#   theme_bw() + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
#   facet_grid(~Type) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
# #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
#   ylab("") +
#   xlab("Year") +
#   theme(panel.grid.minor=element_blank(),
#         panel.grid.major=element_blank()) +
#   geom_vline(xintercept = 3, linetype = 3)

source("glmm_fun.R")
library(dplyr)
library(ggplot2)
library(cowplot)

# biomass ----
# BT
bt.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BT", Biomass_100)
bt.bio.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "BT", Biomass_100)
p1 <- fig.np(bt.bio.np.summ)
p2 <- fig.np(bt.bio.pl.summ)

bottom_row <- plot_grid(p2, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p1, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
#draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BT_biomass.png"), width=10, height=8, units="in")


# BTYOY
btyoy.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BTYOY", Biomass_100)
btyoy.bio.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "BTYOY", Biomass_100)
p3 <- fig.np(btyoy.bio.np.summ)
p4 <- fig.np(btyoy.bio.pl.summ)

bottom_row <- plot_grid(p4, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p6, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BTYOY_biomass.png"), width=10, height=8, units="in")



# AS
as.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "AS ", Biomass_100)
as.bio.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "AS ", Biomass_100)
p5 <- fig.np(as.bio.np.summ)
p6 <- fig.np(as.bio.pl.summ)

bottom_row <- plot_grid(p6, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p5, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/AS_biomass.png"), width=10, height=8, units="in")


# ASYOY
asyoy.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "AS ", Biomass_100)
asyoy.bio.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "AS ", Biomass_100)
p7 <- fig.np(asyoy.bio.np.summ)
p8 <- fig.np(asyoy.bio.pl.summ)

bottom_row <- plot_grid(p8, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p7, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/ASYOY_biomass.png"), width=10, height=8, units="in")

# density ----

# BT
bt.den.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BT", Density_100)
bt.den.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "BT", Density_100)
p9 <- fig.np(bt.den.np.summ)
p10 <- fig.np(bt.den.pl.summ)

bottom_row <- plot_grid(p10, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p9, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (# fish/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BT_density.png"), width=10, height=8, units="in")


# BTYOY
btyoy.den.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BTYOY", Density_100)
btyoy.den.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "BTYOY", Density_100)
p11 <- fig.np(btyoy.den.np.summ)
p12 <- fig.np(btyoy.den.pl.summ)

bottom_row <- plot_grid(p12, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p11, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (# fish/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BTYOY_density.png"), width=10, height=8, units="in")



# AS
as.den.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "AS ", Density_100)
as.den.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "AS ", Density_100)
p13 <- fig.np(as.den.np.summ)
p14 <- fig.np(as.den.pl.summ)

bottom_row <- plot_grid(p14, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p13, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (# fish/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/AS_density.png"), width=10, height=8, units="in")


# ASYOY
asyoy.den.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "AS ", Density_100)
asyoy.den.pl.summ <- fig.data(edited_cs_estimates_by_site, "Yes", "AS ", Density_100)
p15 <- fig.np(asyoy.den.np.summ)
p16 <- fig.np(asyoy.den.pl.summ)

bottom_row <- plot_grid(p16, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p15, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (# fish/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/ASYOY_density.png"), width=10, height=8, units="in")
