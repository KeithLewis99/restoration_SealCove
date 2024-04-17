####Used datafile Counts_Fishenteringcreekeachhour_nozeros.csv
library(lattice)
trellis.device(color="F")
histogram(~Counts_Fishenteringcreekeachhour_nozeros$Hour_Enter|factor(Counts_Fishenteringcreekeachhour_nozeros$Year_Enter), xlab="Hour of Day (24 hours)", breaks=24, type="count", ylab="Frequency")


library(ggplot2)

jpeg("Figure4test.jpg", width=7.5, height=4, units='in', res=600)

#1991 data
BT1991<-subset(sealcove1991bytype, sealcove1991bytype$Species!='AS')
BT1991<-subset(BT1991, BT1991$Species!='ASYOY')

#1992 data
BT1992<-subset(sealcove1992bytype, sealcove1992bytype$Species!='AS')
BT1992<-subset(BT1992, BT1992$Species!='ASYOY')

#1993 data
BT1993<-subset(sealcove1993bytype, sealcove1993bytype$Species!='AS')
BT1993<-subset(BT1993, BT1993$Species!='ASYOY')


#####1994 Data
BT1994<-subset(sealcove1994bytype, sealcove1994bytype$Species!='AS')
BT1994<-subset(BT1994, BT1994$Species!='ASYOY')

####1999 Data
BT1999<-subset(sealcove1999bytype, sealcove1999bytype$Species!='AS')
BT1999<-subset(BT1999, BT1999$Species!='ASYOY')

ggplot(BT1999, aes(Length.mm)) + 
  geom_histogram(binwidth=20, colour="white", fill="black") +
  facet_grid(~Station) +
  ylab("Count") + xlab("Fork Length (mm)") +
  xlim(c(0,250)) +
  theme_bw() +
  annotate("text", x=200, y=50, size=10, label="1999") +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 


######2007 SCR Data
BT2007<-subset(SealCove2007bytype, SealCove2007bytype$Species!='AS')
BT2007<-subset(BT2007, BT2007$Species!='ASYOY')

ggplot(BT2007, aes(Length.mm)) + 
  geom_histogram(breaks=seq(0, 250, by=25), colour="white", fill="black") +
  facet_grid(~Station) +
  ylab("Count") + xlab("Fork Length (mm)") +
  xlim(c(0,250)) +
  theme_bw() +
  annotate("text", x=200, y=50, size=10, label="2007") +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 


#######SCR 2015 data

BT2015<-subset(sealcove2015bytype, sealcove2015bytype$Species!="AS")
BT2015<-subset(BT2015, BT2015$Species!="ASYOY")

ggplot(BT2015, aes(Length.mm)) + 
  geom_histogram(binwidth=20, colour="white", fill="black") +
  facet_grid(~Station) +
  ylab("Count") + xlab("Fork Length (mm)") +
  xlim(c(0,250)) + ylim(c(0,200)) +
  theme_bw() +
  annotate("text", x=200, y=50, size=12, label="2015") +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 

xlim(c(0,250)) + ylim(c(0,200)) +

#####SCR 2016 Data

BT2016<-subset(sealcove2016bytype, sealcove2016bytype$Species!="AS")
BT2016<-subset(BT2016, BT2016$Species!="ASYOY")


####All years

BTallyears<-rbind(BT1991,BT1992,BT1993,BT1994,BT1999,BT2007,BT2007,BT2015,BT2016)

jpeg("lengthyearhistearly.jpg", width=7.5, height=8, units='in', res=600)

BTearly<-rbind(BT1991,BT1992,BT1993,BT1994)

ggplot(BTearly, aes(Length.mm)) + 
  geom_histogram(binwidth=20, colour="white", fill="black") +
  facet_grid(Year~Station) +
  scale_x_continuous(breaks=seq(10,230,30)) +
  ylab("Count") + xlab("Fork Length (mm)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 




dev.off()
par(mfrow=c(1,1))

scale_x_continuous(breaks=seq(10,230,20))



jpeg("lengthyearhistlate.jpg", width=7.5, height=7.5, units='in', res=600)

BTlate<-rbind(BT1999,BT2007,BT2015,BT2016)

ggplot(BTlate, aes(Length.mm)) + 
  geom_histogram(binwidth=20, colour="white", fill="black") +
  facet_grid(Year~Station) +
  scale_x_continuous(breaks=seq(10,230,30)) +
  ylab("Count") + xlab("Fork Length (mm)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 




dev.off()
par(mfrow=c(1,1))

scale_x_continuous(breaks=seq(10,230,20))
#######

library(ggplot2)

BT1516<-rbind(BT2015,BT2016)

ggplot(BT1516, aes(Length.mm)) + 
  geom_histogram(binwidth=20, colour="white", fill="black") +
  facet_grid(Year~Station) +
  scale_x_continuous(breaks=seq(10,230,20)) +
  ylab("Count") + xlab("Fork Length (mm)") +
  theme_bw() +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) 

