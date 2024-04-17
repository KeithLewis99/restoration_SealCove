######Creating dataset of mean biomass by habitat for each year for BT only

BT_estimates_by_site <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/BT_estimates_by_site.csv")

library(plyr)

BTbyhabitat <- ddply(BT_estimates_by_site, c("Year", "Pool", "Time", "Treatment"), summarise,
                     N  = length(Biomass_round),
                     mean = mean(Biomass_round),
                     sd   = sd(Biomass_round),
                     se   = sd / sqrt(N)
)


BTbyhabitat


###plots

boxplot(mean~Treatment, data=BTbyhabitat)

boxplot(mean~Pool, data=BTbyhabitat)

boxplot(mean~Time, data=BTbyhabitat)


###GLM (Gamma)


BT1.glm<- glm(mean~Time*Treatment*Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT1.glm)
anova(BT1.glm, test = "F")

BT1A.glm<-glm(mean~Time+Treatment+Pool+Time:Pool+Time:Treatment+Time:Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT1A.glm)

BT2.glm<- glm(mean~Time+Treatment+Pool+Time:Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT2.glm)

#Model validation
plot(BT2.glm)


residBT2.glm<-resid(BT2.glm)
plot(residBT2.glm)
boxplot(residBT2.glm~BTbyhabitat$Pool)
boxplot(residBT2.glm~BTbyhabitat$Time)
boxplot(residBT2.glm~BTbyhabitat$Treatment)

par(mfrow=c(1,2))
boxplot(BTbyhabitat$mean[BTbyhabitat$Pool=="Yes"]~BTbyhabitat$Time[BTbyhabitat$Pool=="Yes"], main="Pool")
boxplot(BTbyhabitat$mean[BTbyhabitat$Pool=="No"]~BTbyhabitat$Time[BTbyhabitat$Pool=="No"], main="Riffle/Flat")

par(mfrow=c(1,2))
boxplot(BTbyhabitat$mean[BTbyhabitat$Treatment=="Control"]~BTbyhabitat$Time[BTbyhabitat$Treatment=="Control"], main="Control")
boxplot(BTbyhabitat$mean[BTbyhabitat$Treatment=="Impact"]~BTbyhabitat$Time[BTbyhabitat$Treatment=="Impact"], main="Impact")

par(mfrow=c(1,2))
boxplot(BTbyhabitat$mean[BTbyhabitat$Treatment=="Control"]~BTbyhabitat$Pool[BTbyhabitat$Treatment=="Control"], main="Control")
boxplot(BTbyhabitat$mean[BTbyhabitat$Treatment=="Impact"]~BTbyhabitat$Pool[BTbyhabitat$Treatment=="Impact"], main="Impact")

library(ggplot2)
ggplot(BTbyhabitat, aes(mean, Time)) +
  geom_point(position_dodge(0.5))

jpeg("pooltimetreatment.jpg", width=9, height=5, units='in', res=600)
par(mfrow=c(1,2))
boxplot(mean~Pool, data=BTbyhabitat, xlab="Pool", ylab="Mean BT biomass (g/100 sq. m)")
boxplot(mean~Treatment, data=BTbyhabitat, xlab="Treatment", ylab="Mean Brook Trout biomass (g/100 sq. m)")
dev.off()


interaction.plot(testBTbyhabitat$Time, Pool, testBTbyhabitat$mean, fun=mean, type=c("l"), xlab="Time", ylab="Mean BT Biomass (g/100 sq. m)")
dev.off()

jpeg("interactiontimehabitat.jpg", width=6.2, height=5, units='in', res=600)
interaction.plot(testBTbyhabitat$Time, Pool, testBTbyhabitat$mean, fun=mean, type=c("l"), xlab="Time", ylab="Mean BT Biomass (g/100 sq. m)")
dev.off()

abline(BT2.glm)

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
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(axis.text = element_text(size=14)) +
  theme(axis.title=element_text(size=14)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") 




























######Creating dataset of mean density by habitat for each year for BT only

library(plyr)

BTdensitybyhabitat <- ddply(BT_estimates_by_site, c("Year", "Pool", "Time", "Treatment"), summarise,
                     N  = length(Density_round),
                     mean = mean(Density_round),
                     sd   = sd(Density_round),
                     se   = sd / sqrt(N)
)

BTdensitybyhabitat


BTden1.glm<- glm(mean~Time*Treatment*Pool, family=poisson, data=BTdensitybyhabitat)

summary(BTden1.glm)

BTden1A.glm<-glm(mean~Time+Treatment+Pool+Time:Pool+Time:Treatment, family=poisson, data=BTdensitybyhabitat)

summary(BTden1A.glm)

BTden2.glm<- glm(mean~Time+Treatment+Pool+Time:Pool, family=poisson, data=BTdensitybyhabitat)

summary(BTden2.glm)

BTden3.glm<-glm(mean~Time+Treatment+Pool, family=poisson, data=BTdensitybyhabitat)

summary(BTden3.glm)

BTden4.glm<-glm(mean~Treatment+Pool, family=poisson, data=BTdensitybyhabitat)

summary(BTden4.glm)

anova(BTden3.glm,BTden4.glm, test="Chisq")
anova(BTden3.glm,BTden4.glm, test="F")

###overdispersed - used quasipoisson to account for this

BTden5.glm<-glm(mean~Treatment+Pool, family=quasipoisson, data=BTdensitybyhabitat)

summary(BTden5.glm)

plot(BTden5.glm)

par(mfrow=c(1,1))
residBTden5.glm<-resid(BTden5.glm)
plot(residBTden5.glm)
boxplot(residBTden5.glm~BTdensitybyhabitat$Pool)
boxplot(residBTden5.glm~BTdensitybyhabitat$Time)
boxplot(residBTden5.glm~BTdensitybyhabitat$Treatment)

BTden6.glm<-glm(mean~Treatment+Pool, family=Gamma(link=identity), data=BTdensitybyhabitat)
summary(BTden6.glm)

BTden6A.glm<-glm(mean~Time+Treatment+Pool+Time:Pool+Time:Treatment, family=Gamma, data=BTdensitybyhabitat)

summary(BTden6A.glm)

residBTden6.glm<-resid(BTden6.glm)
plot(residBTden6.glm)



