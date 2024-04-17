#####Frequency/Age Histogram
###Removed all NA's (40 records) and all AS and Brown Trout (1477 records) in Excel before importing to R
#####File called BTonly_age_allyears.csv




###Check to ensure only BT included
levels(BTonly_age_allyears$Species)

BT1to4<-subset(BTonly_age_allyears, BTonly_age_allyears$Age!="5")

library(ggplot2)

jpeg("ageyearhist.jpg", width=7.5, height=7.5, units='in', res=600)

ggplot(BT1to4, aes(Age)) + 
  geom_histogram(binwidth=1, colour="white", fill="black") +
  facet_grid(Year~Type) +
  ylab("Count") + xlab("Age") +
  theme_bw() +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 

dev.off()
