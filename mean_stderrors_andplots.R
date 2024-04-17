

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

write.csv(Mean_Bio_SC, "C:/Users/loughlink/Documents/SPERA Project 2015 and 16/Seal Cove/R_Depletion_SealCove/Mean_Bio_SC.csv")
write.csv(Mean_Den_SC, "C:/Users/loughlink/Documents/SPERA Project 2015 and 16/Seal Cove/R_Depletion_SealCove/Mean_Den_SC.csv")






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
Mean_Bio_BT$BT_Gen<-values[match(Mean_Bio_BT$Year, index)]
Mean_Bio_BT


library(ggplot2)

jpeg("mean_BTbio_by_year.jpg", width=8, height=5, units='in', res=600)

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

dev.off()
par(mfrow=c(1,1))

#####Just BT density plots

Mean_Den_BT<-subset(Mean_Den_SC, Mean_Den_SC$Species=="BT")

##Change Destroyed to Compensation for graph display
Mean_Den_BT[4,3] = "Compensation"
Mean_Den_BT[2,3] = "Compensation"

##Make subset of two points to colour (destroyed sites)
d1<-subset(Mean_Den_BT, Year=="1988"&Type=="Compensation")
d2<-subset(Mean_Den_BT, Year=="1989"&Type=="Compensation")


library(ggplot2)

jpeg("mean_BTden_by_year.jpg", width=8, height=5, units='in', res=600)

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


library(ggplot2)

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


library(ggplot2)

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
  
