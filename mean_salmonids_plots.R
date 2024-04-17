library(plyr)

Mean_SalmonidBio_SC <- ddply(edited_cs_salmonids_bysite, c("Year", "Type"), summarise,
                     N  = length(Biomass_100),
                     mean = mean(Biomass_100),
                     sd   = sd(Biomass_100),
                     se   = sd / sqrt(N)
)

Mean_SalmonidBio_SC


library(ggplot2)


#######NO FACETS
jpeg("mean_allsalmonids_by_year_nofacets.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_SalmonidBio_SC, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
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



####WITH FACETS

##Change Destroyed to Compensation for graph display
Mean_SalmonidBio_SC[4,2] = "Compensation"
Mean_SalmonidBio_SC[2,2] = "Compensation"

##Make subset of two points to colour (destroyed sites)
s1<-subset(Mean_SalmonidBio_SC, Year=="1988"&Type=="Compensation")
s2<-subset(Mean_SalmonidBio_SC, Year=="1989"&Type=="Compensation")

jpeg("mean_allsalmonids_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_SalmonidBio_SC, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  geom_point(data=s1, colour="red") +
  geom_point(data=s2, colour="red") +
  theme(panel.grid.minor=element_blank(),
      panel.grid.major=element_blank()) 

dev.off()


######SALMONID DENSITY

library(plyr)

Mean_SalmonidDen_SC <- ddply(edited_cs_salmonids_bysite, c("Year", "Type"), summarise,
                             N  = length(Density_100),
                             mean = mean(Density_100),
                             sd   = sd(Density_100),
                             se   = sd / sqrt(N)
)

Mean_SalmonidDen_SC


###With Facets

##Change Destroyed to Compensation for graph display
Mean_SalmonidDen_SC[4,2] = "Compensation"
Mean_SalmonidDen_SC[2,2] = "Compensation"

##Make subset of two points to colour (destroyed sites)
g1<-subset(Mean_SalmonidDen_SC, Year=="1988"&Type=="Compensation")
g2<-subset(Mean_SalmonidDen_SC, Year=="1989"&Type=="Compensation")

jpeg("mean_density_allsalmonids_by_year.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_SalmonidDen_SC, aes(as.factor(Year), mean)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("Year") +
  geom_point(data=g1, colour="red") +
  geom_point(data=g2, colour="red") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 

dev.off()



#######NO FACETS
jpeg("mean_density_allsalmonids_by_year_nofacets.jpg", width=8, height=5, units='in', res=600)

ggplot(Mean_SalmonidDen_SC, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
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
  theme(legend.position=c(.84, .84)) +
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 

dev.off()


library(plyr)



####################BIOMASS - NO FACETS - NO DOWNSTREAM



Mean_SalmonidBio_SC <- ddply(edited_cs_salmonids_bysite, c("Year", "Type"), summarise,
                             N  = length(Biomass_100),
                             mean = mean(Biomass_100),
                             sd   = sd(Biomass_100),
                             se   = sd / sqrt(N)
)

SalmonidBio_SC_nodown<-subset(Mean_SalmonidBio_SC, Mean_SalmonidBio_SC$Type!="Downstream")





#######NO FACETS

jpeg("mean_salmonids_nodown_nofacets.jpg", width=8, height=5, units='in', res=600)

ggplot(SalmonidBio_SC_nodown, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  
  scale_colour_manual(values=c("black", "dark grey", "red"),
                      name="",
                      breaks=c("Compensation", "Control", "Destroyed"),
                      labels=c("Compensation", "Control", "Destroyed")) +
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


######SALMONID DENSITY

SalmonidDen_SC_nodown<-subset(Mean_SalmonidDen_SC, Mean_SalmonidDen_SC$Type!="Downstream")





#######NO FACETS

library(ggplot2)

jpeg("mean_salmonidsdensity_nodown_nofacets.jpg", width=8, height=5, units='in', res=600)

ggplot(SalmonidDen_SC_nodown, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Density Estimate (g/100 sq. m)") +
  xlab("Year") +
  
  scale_colour_manual(values=c("black", "dark grey", "red"),
                      name="",
                      breaks=c("Compensation", "Control", "Destroyed"),
                      labels=c("Compensation", "Control", "Destroyed")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  theme_bw() +
  
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.85, .84)) +
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) 

dev.off()










