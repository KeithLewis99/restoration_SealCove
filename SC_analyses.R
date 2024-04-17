
edited_cs_estimates_by_site <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/edited_cs_estimates_by_site.csv")

BT_cs_estimatesbytype <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/BT_cs_estimatesbytype.csv")

BT_estimates_for_models <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/BT_estimates_for_models.csv")

###SEAL COVE

hist(edited_cs_estimates_by_site$Biomass_100)

hist(edited_cs_estimates_by_site$Density_100)

names(edited_cs_estimates_by_site)

BT_estimates_by_site<-subset(edited_cs_estimates_by_site, edited_cs_estimates_by_site$Species=="BT")

write.csv(BT_estimates_by_site, file="C:/Users/loughlink/Documents/SPERA Project 2015 and 16/Seal Cove/R_Depletion_SealCove/CS_Estimates/BT_estimates_by_site.csv")

###EXPLORATORY PLOTs

boxplot(Biomass_100~Type, data=BT_estimates_by_site)

boxplot(Biomass_100~as.factor(Year), data=BT_estimates_by_site)

boxplot(Biomass_100~as.factor(Station), data=BT_estimates_by_site)

boxplot(Biomass_100~as.factor(Habitat), data=BT_estimates_by_site)

library(ggplot2)

plot(Biomass_100~as.factor(Station), data=BT_estimates_by_site)

ggplot(BT_estimates_by_site, aes(as.factor(Station), Biomass_100, colour=Habitat)) + 
  geom_point(size=3, position=position_dodge(0.5)) 

ggplot(BTbyhabitat, aes(mean)) + 
  geom_histogram() 

ggplot(BT_after, aes(as.factor(Habitat), log10(Biomass_100), colour=Treatment)) + 
  geom_point(size=3, position=position_dodge(0.5)) 


boxplot(Biomass_100~Type, data=edited_cs_salmonids_bysite)

boxplot(Biomass_100~as.factor(Year), data=edited_cs_salmonids_bysite)

boxplot(Biomass_100~as.factor(Station), data=edited_cs_salmonids_bysite)

boxplot(Biomass_100~as.factor(Habitat), data=edited_cs_salmonids_bysite)


######MODELS - BIOMASS

#Simplest Analysis - Does biomass differ in compensation compared to destroyed?

BT.glm1<-glm(Biomass_round~Station, data=BT_cs_estimatesbytype, family=Gamma)

summary(BT.glm1)

plot(BT.glm1)
anova(BT.glm1, test = "F")
######Mean biomass by habitat for each year

library(plyr)

BTbyhabitat <- ddply(BT_estimates_by_site, c("Year", "Pool", "Time", "Treatment"), summarise,
                      N  = length(Biomass_round),
                      mean = mean(Biomass_round),
                      sd   = sd(Biomass_round),
                      se   = sd / sqrt(N)
)

BTbyhabitat

BT1.glm<- glm(mean~Time*Treatment*Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT1.glm)

BT2.glm<- glm(mean~Time+Treatment+Pool+Time:Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT2.glm)


anova(BT1.glm, BT2.glm, test = "F")

#Model validation
plot(BT2.glm)

residBT2.glm<-resid(BT2.glm)
plot(residBT2.glm)
boxplot(residBT2.glm~BTbyhabitat$Pool)
boxplot(residBT2.glm~BTbyhabitat$Time)



BT.3.glm<-glm(mean~Time+Pool+Time:Pool, family=Gamma(link=identity), data=BTbyhabitat)

summary(BT.3.glm)

anova(BT1.glm, BT2.glm, BT.3.glm, test = "F")

boxplot(mean~Treatment, data=BTbyhabitat)

boxplot(mean~Pool, data=BTbyhabitat)

boxplot(mean~Time, data=BTbyhabitat)

plot(mean~Pool|Year, data=BTbyhabitat)

###Mixed model with site as the random effect

library(lme4)

BT_after<-subset(BT_estimates_for_models, BT_estimates_for_models$Type!="Destroyed")

BT.lmer<- glmer(Biomass_100 ~ Treatment + (1|Station), family=Gamma, data=BT_after)
summary(BT.lmer)

# my attempt to reproduce Kristen's analyses
tmp <- lmer(logbio ~ Type + (1|Station), data=SC_BT_Pools_allyears)
summary(tmp)

tmp1 <- lmer(logbio ~ (1|Station), data=SC_BT_Pools_allyears)
summary(tmp1)

anova(tmp)
anova(tmp, tmp1, test = "F")

##Two way ANOVA - unbalanced design

library(car)
BT_anova<-aov(Biomass_round~Type + as.factor(Year), data=BT_estimates_by_site)
anova(BT_anova, type="III")


###GLM with Poisson Distribution

BT.glm1<-glm(Biomass_round~Habitat, family=poisson, data=BT_estimates_by_site)

summary(BT.glm1)

plot(BT.glm1)

####log transformed data

BT.lm2<-lm(log10(Biomass_100)~Habitat, data=BT_estimates_by_site)

summary(BT.lm2)

plot(BT.lm2)


BT.lm3<-lm(log10(Biomass_100)~ Type, data=BT_estimates_by_site)

summary(BT.lm3)

plot(BT.lm3)

anova(BT.lm3, BT.lm2, test = "F")
