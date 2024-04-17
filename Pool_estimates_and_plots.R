
######POOLS ONLY
###Mean BT Biomass Estimates with Standard Errors 
SC_BT_Pools_allyears <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/SC_BT_Pools_allyears.csv")
library(plyr)

BTPoolBio_SC <- ddply(SC_BT_Pools_allyears, c("Year", "Type"), summarise,
                     N  = length(biomass),
                     mean = mean(biomass),
                     sd   = sd(biomass),
                     se   = sd / sqrt(N)
)

BTPoolBio_SC

###Mean BT Density Estimates with Standard Errors

BTPoolDen_SC<-ddply(SC_BT_Pools_allyears, c("Year", "Type"), summarise,
                   N  = length(density),
                   mean = mean(density),
                   sd   = sd(density),
                   se   = sd / sqrt(N)
)

BTPoolDen_SC

######BT BIOMASS PLOTS

library(ggplot2)

jpeg("BTPoolBio_by_year_nocolour.jpg", width=8, height=5, units='in', res=600)

ggplot(BTPoolBio_SC, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=14)) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.88, .88)) +
  theme(legend.text=element_text(size=14)) +
  scale_fill_discrete(name="",
                      breaks=c("Lunker", "No Lunker"),
                      labels=c("LUNKER", "No LUNKER")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("Lunker", "No Lunker"),
                      labels=c("LUNKER", "No LUNKER")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") 
  

dev.off()
par(mfrow=c(1,1))


#####BT Density Plot

library(ggplot2)

jpeg("BTPoolDen_by_year_nocolour.jpg", width=8, height=5, units='in', res=600)

ggplot(BTPoolDen_SC, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=14)) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.70, .88)) +
  theme(legend.text=element_text(size=14)) +
  scale_fill_discrete(name="",
                      breaks=c("Lunker", "No Lunker"),
                      labels=c("LUNKER", "No LUNKER")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("Lunker", "No Lunker"),
                      labels=c("LUNKER", "No LUNKER")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  ylab("Mean Density Estimate (g/100 sq. m)") +
  xlab("Year") 


dev.off()
par(mfrow=c(1,1))


######Statistics - Pools

BTPoolBio_SC

lunkerbio<-subset(BTPoolBio_SC, BTPoolBio_SC$Type=="Lunker")
nolunkerbio<-subset(BTPoolBio_SC, BTPoolBio_SC$Type=="No Lunker")

t.test(lunkerbio$mean, nolunkerbio$mean)

wilcox.test(lunkerbio$mean, nolunkerbio$mean)

boxplot(BTPoolBio_SC$mean~BTPoolBio_SC$Type)
hist(BTPoolBio_SC$mean)


lunkerden<-subset(BTPoolDen_SC, BTPoolDen_SC$Type=="Lunker")
nolunkerden<-subset(BTPoolDen_SC, BTPoolDen_SC$Type=="No Lunker")

t.test(lunkerden$mean, nolunkerden$mean)

wilcox.test(lunkerden$mean, nolunkerden$mean)

boxplot(BTPoolDen_SC$mean~BTPoolDen_SC$Type)
hist(BTPoolDen_SC$mean)
