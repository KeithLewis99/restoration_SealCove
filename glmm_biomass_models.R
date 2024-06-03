# source ----
source("glmm_data.R")
source("glmm_fun.R")

# Dave Fifield helped alot with below and provided expert guidance, especially on glmmTMB.  Paul Regular provided some great insights on hurdle models.

# Expands greatly upon original analyses by Dave Cote and Kristin Loughlin
# See ReadMe for thoughts on this approach. 

# library ----
library(nlme)
library(glmmTMB)
library(DHARMa)
library(readr)
library(tidyr)
library(ggplot2)
library(cowplot)



# BT ----
## glm ----
# same as Cote but with biomass for all sites
# BT1.glm.not.pool.full <- glm(Biomass_100~Time*Treatment, family=Gamma(link=log), data=bt.np)
# summary(BT1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(BT1.glm.not.pool.full) 
# graphics.off()
# # variance is not homogeneous and normality of resids is poor
# 
# ## gls ----
# # the Cote way
# bt.gls1 <- gls(mean~Time*Treatment, data=BTbyhabitat.not.pool)
# bt.gls2 <- gls(mean~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=BTbyhabitat.not.pool)
# anova(bt.gls1, bt.gls2) # suggests no improvement with this variance structure
# 
# # using all the data
# bt.gls3 <- gls(Biomass_100~Time*Treatment, data=bt.np)
# bt.gls4 <- gls(Biomass_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=bt.np)
# anova(bt.gls3, bt.gls4) # suggests Big improvement with this variance structure
# plot(bt.gls4)        # but variance is still heterogeneous
# 
# ## lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# bt.lme1 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=bt.np)
# bt.lme2 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, data=bt.np)
# anova(bt.lme1, bt.lme2)
# # the anova suggests that the weights really help here 
# summary(bt.lme1)
# plot(bt.lme1)
# # heterogeneity looks great but variance increases with increased fitted value
# 
# # the following is to try and understand why the fit v res plot has so many values - Basically, we have 11 years and two categorical variables - time (before/after) and trt (control/impact).  So, 11x2 =22 but in 1990, only the controls were sampled so 21.
# bt.np$fits <- fitted(bt.lme1)
# unique(fitted(bt.lme1))
# bt.np$resid <- resid(bt.lme1, type = "normalized")
# #mm <- model.matrix(~Time*Treatment, data = bt.np)
# #write.csv(mm[1:98, 1:4], "mm.csv")
# #View(bt.np[, c(2,4,6, 7, 13, 14, 18:20)])
# 
# # test normality which is not great
# qqnorm(bt.np$resid)
# qqline(bt.np$resid)
# 
# #summary(lm(Biomass_100~Time*Treatment, data=bt.np))


## glmm ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
bt.glmm1 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  dispformula = ~ Int,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.np
)

summary(bt.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt.glmm2 <- glmmTMB(Biomass_100~Time*Treatment + (1|Year), 
                 #dispformula = ~ Int,
                 family = Gamma(link=log),
                 REML = TRUE,
                 data=bt.np)
summary(bt.glmm2)
# str(bt.glmm2)
anova(bt.glmm1, bt.glmm2) # this suggests that model bt.glmm1 with dispersion is much better than without (bt.glmm2)
## The estimates are virtually identical but the Std. Errors are slightly smaller than the model with dispersion.  
#Further, the diagnostics look better much better for this model (see below but tested separately for bt.glmm2 which is no longer in the code), especially for homogeneity of variance and this model has a much better AIC. 
# OK, the bt.glmm1 model is best.  Now, its time to see if its valid (Fifield said no need to test the less valid model at this point)?  Talked to Fifield about how to do this.  He and glmmTMB suggest using the DHARMa package

## diagnostics ----
bt.glmm1_simres <- simulateResiduals(bt.glmm1)
# str(bt.glmm1_simres,1)
plot(bt.glmm1_simres)
# The normality and homogeneity of variance look great.  Fifield said that this needs to be looked at for all the usual reasons: normality and homogeneity of variance.  But what about temporal and spatial independence? 


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt.glmm1_simres_recalc <- recalculateResiduals(bt.glmm1_simres, group = bt.np$Year)
# plot(bt.glmm1_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(bt.glmm1_simres_recalc, time = unique(bt.np$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
bt.glmm1_simres_recalcSpace <- recalculateResiduals(bt.glmm1_simres, group = as.factor(bt.np$Station_new))
unique(bt.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(bt.glmm1_simres_recalcSpace, x = unique(bt.np$X), y = unique(bt.np$Y))

spatialAutoCorrBase_fun(bt.np, bt.glmm1_simres_recalcSpace)   

bt.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], 
          bt.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (bt.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.


summary(bt.glmm1)
mean_by_site(bt.np.biomass.station, "no", "b")
baci.plot(bt.np.biomass.baci, "b")

bt.np[bt.np$Station_new == "D2",]
bt.glmm1_sum <- summary(bt.glmm1)
bt.glmm1_fit <- fitted(bt.glmm1, se.fit = T)
bt.glmm1_ran <- ranef(bt.glmm1)
bt.glmm1_res <- residuals(bt.glmm1)



## this is a dispersion model and it seems to match the fitted values but probably wouldn't match the SE values without bringing the dispersion model in.
exp(bt.glmm1_sum$coefficients$cond[1,1] + 
      bt.glmm1_sum$coefficients$cond[2,1] +
      bt.glmm1_sum$coefficients$cond[3,1] +
      bt.glmm1_sum$coefficients$cond[4,1] +
      bt.glmm1_ran$cond$Year[1,])
bt.glmm1_fit[1] # matches the above




# BTYOY ----
## Cote approach

# BTYOY1.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data = BTYOYbio100byhabitat.not.pool)

# summary(BTYOY1.glm.not.pool.full)

# par(mfrow=c(2,2))
# plot(BTYOY1.glm.not.pool.full)
# graphics.off() #resids are awful

# 
# 
# # examine distribution and check for zeros
# hist(btyoy.np$Biomass_100)
# length(btyoy.np$Biomass_100)
# length(btyoy.np$Biomass_100[btyoy.np$Biomass_100 == 0])
# btyoy.np[btyoy.np$Biomass_100 == 0,]
# btyoy.np[btyoy.np$Biomass_100 < 0,]
# plot(density(btyoy.np$Biomass_100))
# 
# # 4 of 98 values are 0.  So, there are a number of options.
# ## #1 inflate the zeros slightly
# ## #2 fall back on lmm with a non-gamma distribution
# ## #3 zero inflated or hurdle
# ## #4 tweadie
# ## # This website gives a great discussion on zi v hurdle and when to use them.  Basically, gamma has to be positive real number.  Is the zero there due to sampling (they just weren't there) or structure (you could well have no btyoy in some places).  This website advises that zi models are a mix while hurdle models have only structural zeros. Clarke suggested in an email that these are structural zeros as larger conspecifics will eat the smaller fish, ergo use a hurdle model.
# ### https://biol609.github.io/lectures/13_zinf.html#53_the_error_generating_process
# 
# 
# ## glm ----
# ### same as Cote but with biomass for all sites AND biomass adjustment
# BTYOY1.glm.not.pool.full<- glm(Biomass_100~Time*Treatment, family=Gamma(link=log), data=btyoy.np.001)
# summary(BTYOY1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(BTYOY1.glm.not.pool.full) # resids are pretty awful
# graphics.off()
# 
# ## gls ----
# # using all the data
# btyoy.gls1 <- gls(Biomass_100~Time*Treatment, data=btyoy.np)
# btyoy.gls2 <- gls(Biomass_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=btyoy.np)
# anova(btyoy.gls1, btyoy.gls2) # suggests no improvement with this variance structure
# plot(btyoy.gls1)        # and variance is still heterogeneous
# 
# ## lmm ----
# # btyoy.gls2 helps the variance issue a bit - let's try random effect
# btyoy.lme1 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=btyoy.np)
# btyoy.lme2 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, data=btyoy.np)
# anova(btyoy.lme1, btyoy.lme2)
# # the anova suggests that the weights don't help really at all
# summary(btyoy.lme1)
# plot(btyoy.lme1)
# # heterogeneity looks reasonable although a few low variance at low fitted values and some curving in the middle.  Could stop here but sample sizes are small.

## glmm -----
### with biomass adjustment - just running this to see if it makes much difference as it isn't "right"
# btyoy.glmm1 <- glmmTMB(
#   Biomass_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = TRUE,
#   data = btyoy.np.001
# )
# 
# summary(btyoy.glmm1)
# # str(btyoy.glmm1)
# 
# 
# ## Compre the results of:

# ### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
# ### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
# btyoy.glmm2 <- glmmTMB(Biomass_100~Time*Treatment + (1|Year), 
#                  #dispformula = ~ Int,
#                  family = Gamma(link=log),
#                  REML = TRUE,
#                  data=btyoy.np.001)
# summary(btyoy.glmm2)
# # str(btyoy.glmm2)

# anova(btyoy.glmm1, btyoy.glmm2) # this suggests that model with dispersion is quite a bit better than the model without.


# ## The estimates are virtually identical but the Std. Errors differ and could affect inferences.  

## alt distributions ----

### I did zi gamma and tweadie before I did the hurdle.  But it seems like hurdle models are most appropriate. 

### hurdle gamma with dispersion

btyoy.glmm3 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  #Biomass_100 ~ Time * Treatment + numYear + (1 | Year), # bad diagnostics
  #Biomass_100 ~ Time * Treatment + I(numYear^2) + (1 | Year), #NAs
  dispformula = ~ Int,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.np
)

summary(btyoy.glmm3)

### hurdle gamma without dispersion

btyoy.glmm3a <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.np
)
summary(btyoy.glmm3a)
anova(btyoy.glmm3, btyoy.glmm3a)

# this shows that the model with dispersion is better - compare to tweadie below btyoy.glmm3

### tweadie
btyoy.glmm4 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  dispformula = ~ Int,
  #  family = inverse.gaussian(link = "1/mu^2"),
  #  family = Gamma(link="identity"),
  # family=ziGamma(link="log"), ziformula = ~1,
  family = tweedie, # link is log
  REML = TRUE,
  data = btyoy.np
)

summary(btyoy.glmm4)
anova(btyoy.glmm3, btyoy.glmm4) # tweadie looks somewhat better (btyoy.glmm4) - proceed to check diagnostics

## diagnostics ----
# simulate resids
btyoy.glmm4_simres <- simulateResiduals(btyoy.glmm3)
plot(btyoy.glmm4_simres)

# The normality and homogeneity of variance look great.  
# test zero inflation which looks fine.
testZeroInflation(btyoy.glmm4_simres)

# temporal independence
## Because this is a nested model with multiple measurements by Year, we can't just do the below. Rather, we need to recalculate them with Year as a grouping variable
btyoy.glmm4_simres_recalc <- recalculateResiduals(btyoy.glmm4_simres, group = btyoy.np$Year)

testTemporalAutocorrelation(btyoy.glmm4_simres_recalc, time = unique(btyoy.np$Year))

# resids look great for autocorrelation - one could argue some temproal trends but the years are well spaced and this is likely spurious

# but rethinking this, the trend does look curvilinear and maybe later years are a problem

# I did try to fix the patterns using a correlation structure but I think that ar1 is inappropriate because times are not even and we just don't have the data - all models fail to converge.

## Good website on model syntax https://stats.stackexchange.com/questions/477338/reading-multilevel-model-syntax-in-intuitive-ways-in-r-lme4

# btyoy.glmm5 <- glmmTMB(

#   Biomass_100 ~ Time * Treatment + (1 | Year) + us(Station_new + 0 | Year),
#   #dispformula = ~ Int,
#   family = tweedie, # link is log
#   REML = TRUE,
#   data = btyoy.np
# )

# btyoy.glmm5
# spatial independence

## Need UTMs of the sites.  However, the unique stations are confusing. 

# recalculate resids with stations as the grouping variable

# btyoy.glmm4_simres_recalcSpace <- recalculateResiduals(btyoy.glmm4_simres, group = as.factor(bt.np$Station_new))

# unique(btyoy.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.

# #str(bt.glmm1_simres_recalcSpace)


# testSpatialAutocorrelation(btyoy.glmm4_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)

# testSpatialAutocorrelation(btyoy.glmm4_simres_recalcSpace, x = unique(btyoy.np$X), y = unique(btyoy.np$Y))

# # Diagnostics look fantastic for this model.  Proceed with this model - btyoy.glmm4.  See glmm_anova for above but without REML which will allow for the BACI.
# # In retrospect, I disagree.  Temporal trend and spatial issues.
# summary(btyoy.glmm4)


### Explore ----
btyoy.np$pos <- numFactor(btyoy.np$X, btyoy.np$Y) # this is to create a position as per the Dharma webpage but I can't get it or any spatial structure to fit.

#https://stackoverflow.com/questions/24192428/what-does-the-capital-letter-i-in-r-linear-regression-formula-mean 
btyoy.glmm4_new <- glmmTMB(
  #Biomass_100 ~ Time * Treatment + I(as.numeric(Year)^2) + (1 | Year),
  Biomass_100 ~ Time * Treatment + numYear + (1 | Year),
  #Biomass_100 ~ Time * Treatment + I(numYear^2) + (1 | Year), # NA
#  Biomass_100 ~ Time * Treatment + as.factor(Type) + (1 | Year),
  dispformula = ~ Int,
  family = tweedie, # link is log
  REML = TRUE,
# control = glmmTMBControl(  # gives NAs
#   optimizer = optim,
#   optArgs=list(method = "BFGS")),
  data = btyoy.np
)
summary(btyoy.glmm4_new)

btyoy.glmm4_new_simres <- simulateResiduals(btyoy.glmm4_new)
plot(btyoy.glmm4_new_simres)
# homogeneity OK but fits v resids are a little curvilinear
testZeroInflation(btyoy.glmm4_new_simres)

btyoy.glmm4_new_simres_recalc <- recalculateResiduals(btyoy.glmm4_new_simres, group = btyoy.np$Year)
testTemporalAutocorrelation(btyoy.glmm4_new_simres_recalc, time = unique(btyoy.np$Year))


### spatial independence
# recalculate resids with stations as the grouping variable
btyoy.glmm4_new_simres_recalcSpace <- recalculateResiduals(btyoy.glmm4_new_simres, group = as.factor(bt.np$Station_new))
unique(btyoy.np$Station_new) 

testSpatialAutocorrelation(btyoy.glmm4_new_simres_recalcSpace, x = unique(btyoy.np$X), y = unique(btyoy.np$Y))

spatialAutoCorrBase_fun(btyoy.np, btyoy.glmm4_new_simres_recalcSpace)  
btyoy.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], btyoy.glmm4_new_simres_recalcSpace, coords.np)
spatialAutoCorrGG_fun(btyoy.np.biomass.all)

# OK - there are only 13 values in this 
# This is a substantial improvement over the previous ito temporal residuals.  The only problem is one lag is slightly over the blue line but this is fine - p is far from alpha.  But there is some curvilinear trend in the resids and the spatial resids are awful.
summary(btyoy.glmm4_new)

### Explore 2----

# this helps - plot and temporal resids look fine but 
btyoy.glmm4_new1 <- glmmTMB(
  #Biomass_100 ~ Time*Treatment + Treatment*I(numYear^2) + (1 | Year), #NAs
  Biomass_100 ~ Time*Treatment + Treatment*numYear + (1 | Year),
  #  Biomass_100 ~ Time + Treatment + Treatment*I(as.numeric(Year)^2) + (1 | Year),
  dispformula = ~ Int,
  family = tweedie, # link is log
  REML = TRUE,
  control = glmmTMBControl(  # gives NAs
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = btyoy.np
)
summary(btyoy.glmm4_new1)

btyoy.glmm4_new_simres1 <- simulateResiduals(btyoy.glmm4_new1)
# normality good, fits v resids is better
plot(btyoy.glmm4_new_simres1)
testZeroInflation(btyoy.glmm4_new_simres1)

btyoy.glmm4_new_simres_recalc1 <- recalculateResiduals(btyoy.glmm4_new_simres1, group = btyoy.np$Year)
testTemporalAutocorrelation(btyoy.glmm4_new_simres_recalc1, time = unique(btyoy.np$Year))
# so this is a slight improvement again in the temp resids and there will be no debate.  Also, p-values of conditional model are about the same as Expolore 1 so go with this. 


### spatial independence
# recalculate resids with stations as the grouping variable
btyoy.glmm4_simres_recalcSpace1 <- recalculateResiduals(btyoy.glmm4_new_simres1, group = as.factor(btyoy.np$Station_new))

testSpatialAutocorrelation(btyoy.glmm4_simres_recalcSpace1, x = unique(btyoy.np$X), y = unique(btyoy.np$Y))

spatialAutoCorrBase_fun(btyoy.np, btyoy.glmm4_simres_recalcSpace1)  
btyoy.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], btyoy.glmm4_simres_recalcSpace1, coords.np)

spatialAutoCorrGG_fun(btyoy.np.biomass.all)
# Spatials aren't great but Moran's I is not sig.  so with

# decided to go with btyoy.glmm4_new - its not great but the autocorro is not significant and resids aren't horrible - interpret with caution
summary(btyoy.glmm4_new)
mean_by_site(btyoy.np.biomass.station, "no", "b")
baci.plot(btyoy.np.biomass.baci, "b")


# AS ----
## Cote approach
# ASbio100byhabitat.not.pool$mean <- ifelse(ASbio100byhabitat.not.pool$mean == 0, 0.01, ASbio100byhabitat.not.pool$mean)
# AS1.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data= ASbio100byhabitat.not.pool)
# summary(AS1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(AS1.glm.not.pool.full)
# graphics.off()
# # resids aren't awful but not great
# 
# 
# # examine distribution and check for zeros
# hist(as.np$Biomass_100)
# length(as.np$Biomass_100)
# length(as.np$Biomass_100[as.np$Biomass_100 == 0])
# as.np[as.np$Biomass_100 == 0,]
# as.np[as.np$Biomass_100 < 0,]
# plot(density(as.np$Biomass_100))
# 
# # 15 of 99 values are 0. See above options.
# 
# 
# ## glm ----
# ### same as Cote but with biomass for all sites
# AS1.glm.not.pool.full <- glm(Biomass_100~Time*Treatment, family=Gamma(link=log), data=subset(as.np, Biomass_100 > 0))
# summary(AS1.glm.not.pool.full)
# 
# par(mfrow=c(2,2))
# plot(AS1.glm.not.pool.full) # resids are awful
# graphics.off()
# 
# ## gls ----
# # using all the data
# as.gls1 <- gls(Biomass_100~Time*Treatment, data=as.np)
# as.gls2 <- gls(Biomass_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=as.np)
# anova(as.gls1, as.gls2) # suggests virtually no improvement with this variance structure
# plot(as.gls2)        # and variance is still heterogeneous
# 
# 
# ## lmm ----
# # as.gls2 helps the variance issue a bit - let's try random effect
# as.lme1 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=as.np)
# as.lme2 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, data=as.np)
# anova(as.lme1, as.lme2)
# # the anova suggests that the weights help
# summary(as.lme1)
# plot(as.lme1)
# # heterogeneity doesn't look great - definitely more resids in middle and right - some hint of a linear trend.  



## glmm -----
# as.glmm1 <- glmmTMB(
#   Biomass_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = TRUE,
#   data = as.np
# )
# 
#  summary(as.glmm1)
# str(as.glmm1)
# I get the error "Error in solve.default(as.matrix(Qm)) : system is computationally singular: reciprocal condition number = 1.37471e-19".  A brief google search suggests multicollinearity


## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as.glmm2 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = as.np
)

summary(as.glmm2) # can't compare as the model with dispersion doesn't converge.  Go to Tweedie

### Tweedie
as.glmm3 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  family = tweedie, # link is log
  REML = TRUE,
  data = as.np
)
summary(as.glmm3)
anova(as.glmm2, as.glmm3) # tweadie (as.glmm3) looks only slightly better but proceed with it


## diagnostics ----
# OK, the as.glmm3 model is best.  Is it valid.  
as.glmm3_simres <- simulateResiduals(as.glmm3)
# str(as.glmm3_simres,1)
plot(as.glmm3_simres)

# The normality is great - homogeneity of variance isn't too bad but not significant.  But is this a major concern?  None of the values appear significant.  

# test zero inflation which looks fine.
testZeroInflation(as.glmm3_simres)

# temporal independence
## Because this is a nested model with multiple measurements by Year, we can't just do the below.
# rather, we need to recalculate them with Year as a grouping variable
as.glmm3_simres_recalc <- recalculateResiduals(as.glmm3_simres, group = as.np$Year)
testTemporalAutocorrelation(as.glmm3_simres_recalc, time = unique(as.np$Year))
# resids look a little funny but acf looks great.


# spatial independence
## Need UTMs of the sites.  However, the unique stations are confusing. 
# recalculate resids with stations as the grouping variable
as.glmm3_simres_recalcSpace <- recalculateResiduals(as.glmm3_simres, group = as.factor(as.np$Station_new))
unique(as.np$Station_new) # OK - there are only 12 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(as.glmm3_simres_recalcSpace, x = unique(as.np$X), y = unique(as.np$Y))
spatialAutoCorrBase_fun(as.np, as.glmm3_simres_recalcSpace)   

as.np.biomass.all <- spatialData_join(as.np.biomass.station[-4,], as.glmm3_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(as.np.biomass.all)


# space is fine - reasonable mix of reds and blues.  
#Proceed with this model - as.glmm3.  See glmm_anova for above but without REML which will allow for the BACI.

summary(as.glmm3)
mean_by_site(as.np.biomass.station, "no", "b")
baci.plot(as.np.biomass.baci, "b")


# ASYOY ----
## Cote approach
# ASYOY1.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data= ASYOYbio100byhabitat.not.pool)
# summary(ASYOY1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(ASYOY1.glm.not.pool.full)
# graphics.off() # normality isn't great and homogeneity is awful
# 
# 
# # examine distribution and check for zeros
# hist(asyoy.np$Biomass_100)
# length(asyoy.np$Biomass_100)
# length(asyoy.np$Biomass_100[asyoy.np$Biomass_100 == 0])
# asyoy.np[asyoy.np$Biomass_100 == 0,]
# asyoy.np[asyoy.np$Biomass_100 < 0,]
# plot(density(asyoy.np$Biomass_100))
# 
# # 20 of 130 values are 0. See above options.
# 
# 
# ## glm ----
# ### same as Cote but with biomass for all sites
# AS1.glm.not.pool.full <- glm(Biomass_100~Time*Treatment, family=Gamma(link=log), data=subset(asyoy.np, Biomass_100 > 0))
# summary(AS1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(AS1.glm.not.pool.full) # resids are awful
# graphics.off()
# 
# 
# ## gls ----
# # using all the data
# asyoy.gls1 <- gls(Biomass_100~Time*Treatment, data=asyoy.np)
# asyoy.gls2 <- gls(Biomass_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=asyoy.np)
# anova(asyoy.gls1, asyoy.gls2) # suggests massive improvement with this variance structure
# plot(asyoy.gls1)        # but variance is still heterogeneous with larger values at larger and smaller fitted values
# 
# ## lmm ----
# # as.gls2 helps the variance issue a bit - let's try random effect
# asyoy.lme1 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=asyoy.np)
# asyoy.lme2 <- lme(Biomass_100~Time*Treatment, random =  ~ 1|Year, data=asyoy.np)
# anova(asyoy.lme1, asyoy.lme2)
# # the anova suggests that the weights help
# summary(asyoy.lme1)
# plot(asyoy.lme1)
# # heterogeneity looks OK ito trends but its not homogeneneous for lower and middle fitted values but not the higher fitted values


## glmm -----
# asyoy.glmm1 <- glmmTMB(
#   Biomass_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = TRUE,
#   data = asyoy.np
# )
# summary(asyoy.glmm1) # this gives NA values for AIC and errors.
# # str(asyoy.glmm1)


## Compre the results of glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
asyoy.glmm2 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = asyoy.np
)

summary(asyoy.glmm2)
# str(asyoy.glmm2)
# model without dispersion will be used as model with dispersion won't converge. Compare to Tweedie 

asyoy.glmm3 <- glmmTMB(
  Biomass_100 ~ Time * Treatment + (1 | Year),
  family = tweedie,
  REML = TRUE,
  data = asyoy.np
)
summary(asyoy.glmm3)
anova(asyoy.glmm2, asyoy.glmm3)  # the Gamma hurdle model looks slightly better so proceed with that - asyoy.glmm2


## diagnostics ----
asyoy.glmm2_simres <- simulateResiduals(asyoy.glmm2)
#str(asyoy.glmm2_simres,1)
plot(asyoy.glmm2_simres)
# The normality and homogeneity of variance look great.  

# test zero inflation which looks fine.
testZeroInflation(asyoy.glmm2_simres)

# temporal independence
## Because this is a nested model with multiple measurements by Year, we need to recalculate them with Year as a grouping variable
asyoy.glmm2_simres_recalc <- recalculateResiduals(asyoy.glmm2_simres, group = asyoy.np$Year)
testTemporalAutocorrelation(asyoy.glmm2_simres_recalc, time = unique(asyoy.np$Year))
# resids look great

# spatial independence
asyoy.glmm2_simres_recalcSpace <- recalculateResiduals(asyoy.glmm2_simres, group = as.factor(bt.np$Station_new))
testSpatialAutocorrelation(asyoy.glmm2_simres_recalcSpace, x = unique(asyoy.np$X), y = unique(asyoy.np$Y))

# plot with spatial correction
spatialAutoCorrBase_fun(asyoy.np, asyoy.glmm2_simres_recalcSpace)   

# plot it spatially
# asyoy_temp1 <- as.data.frame(cbind(asyoy.glmm2_simres_recalcSpace$scaledResiduals,
#                                    as.character(unique(btyoy.glmm4_new_simres_recalcSpace$group)))) %>% 
#   rename(scResid = V1, Station = V2)
# asyoy.np.biomass.resids1 <- left_join(asyoy.np.biomass.station[-4,], asyoy_temp1, by=c("Station_new" =  "Station"))

asyoy.np.biomass.all <- spatialData_join(asyoy.np.biomass.station[-4,], asyoy.glmm2_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(asyoy.np.biomass.all)


# spatial independence is OK - proceed to glmm_anova - asyoy.glmm2
summary(asyoy.glmm2)
###  OK, I finally feel that this is defensible - so stop obsessing and move on!
mean_by_site(asyoy.np.biomass.station, "no", "b")
baci.plot(asyoy.np.biomass.baci, "b")
fitted(asyoy.glmm2, se.fit = T)
ranef(asyoy.glmm2)
residuals(asyoy.glmm2)



asyoy.np[asyoy.np$Station_new == "D2",]
asyoy.glmm2_sum <- summary(asyoy.glmm2)
asyoy.glmm2_fit <- fitted(asyoy.glmm2, se.fit = T)
asyoy.glmm2_ran <- ranef(asyoy.glmm2)
asyoy.glmm2_res <- residuals(asyoy.glmm2)
asyoy.glmm2_sum$coefficients$zi[1,1]
exp(asyoy.glmm2_sum$coefficients$zi[1,1])/(1+exp(asyoy.glmm2_sum$coefficients$zi[1,1])) # this gives the same as plogis but plogis may not be right
plogis(asyoy.glmm2_sum$coefficients$zi[1,1]) 
# this converts from logit back to probability but we want q, not p
plogis(1-asyoy.glmm2_sum$coefficients$zi[1,1])


# this actually seems to work - matches last value 
## https://discourse.mc-stan.org/t/mathematical-notation-for-a-zero-inflated-negative-binomial-model-in-brms/21066/13
## https://www.montana.edu/rotella/documents/502/Prob_odds_log-odds.pdf

# 1/(1 + exp(zi)) = q or event of a zero
# exp(zi) = p or the probablitly its not a zero or 1-q
## in this case, zi is applied equally to all observations.
exp(asyoy.glmm2_sum$coefficients$cond[1,1] + 
      # asyoy.glmm2_sum$coefficients$cond[2,1] + 
      # asyoy.glmm2_sum$coefficients$cond[3,1] + 
      # asyoy.glmm2_sum$coefficients$cond[4,1] + 
      asyoy.glmm2_ran$cond$Year[11,]) * # year 11, i.e., 2016
  1/(1 + exp(asyoy.glmm2_sum$coefficients$zi[1,1])) # zi term
asyoy.glmm2_fit[96]

# this works too - matches first value (year 1)
exp(asyoy.glmm2_sum$coefficients$cond[1,1] + 
       asyoy.glmm2_sum$coefficients$cond[2,1] + 
       asyoy.glmm2_sum$coefficients$cond[3,1] + 
       asyoy.glmm2_sum$coefficients$cond[4,1] + 
      asyoy.glmm2_ran$cond$Year[1,]) *
  1/(1 + exp(asyoy.glmm2_sum$coefficients$zi[1,1])) 
asyoy.glmm2_fit[1]

# first zero value D15 in 1991 (Year 4) Control - After - this matches 
exp(asyoy.glmm2_sum$coefficients$cond[1,1] + 
      # asyoy.glmm2_sum$coefficients$cond[2,1] + 
      # asyoy.glmm2_sum$coefficients$cond[3,1] + 
      # asyoy.glmm2_sum$coefficients$cond[4,1] + 
      asyoy.glmm2_ran$cond$Year[4,]) *
  1/(1 + exp(asyoy.glmm2_sum$coefficients$zi[1,1]))
asyoy.glmm2_fit[15]
asyoy.glmm2_res[15] # so not a good fit but the resid is large and gets it to the actual value which is zero

# **POOLS ----
# **POOLS ----

## BT ----
# # using all the data
# btp.gls3 <- gls(Biomass_100~Time, data=bt.pl)
# btp.gls4 <- gls(Biomass_100~Time, weights = varIdent(form = ~ 1|Int), data=bt.pl)
# anova(btp.gls3, btp.gls4) # suggests no improvement with this variance structure
# plot(btp.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# btp.lme1 <- lme(Biomass_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=bt.pl)
# btp.lme2 <- lme(Biomass_100~Time, random =  ~ 1|Year, data=bt.pl)
# anova(btp.lme1, btp.lme2)
# # the anova suggests no improvement with the variance structure
# summary(btp.lme1)
# plot(btp.lme1)
# # but variance is still heterogeneous
# 
# # test normality sucks
# qqnorm(bt.pl$resid)
# qqline(bt.pl$resid)


### glmm ----
#### Gamma is OK here because there are no zeros
btp.glmm1 <- glmmTMB(
  Biomass_100 ~ Time + (1 | Year),
    dispformula = ~ Time,
    family = Gamma(link = log),
    REML = TRUE,
  data = bt.pl
)

summary(btp.glmm1)


## Compre the results of: glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
btp.glmm2 <- glmmTMB(
  Biomass_100~Time + (1|Year), 
      family = Gamma(link=log),
      REML = TRUE,
    data=bt.pl)
summary(btp.glmm2)

anova(btp.glmm1, btp.glmm2) # this suggests that model btp.glmm2 is slightly better than btp.glmm1, i.e., without dispersion 


### diagnostics ----
btp.glmm2_simres <- simulateResiduals(btp.glmm2)
# str(btp.glmm2_simres,1)
plot(btp.glmm2_simres)
# The normality and homogeneity of variance look OK.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btp.glmm2_simres_recalc <- recalculateResiduals(btp.glmm2_simres, group = bt.pl$Year)
testTemporalAutocorrelation(btp.glmm2_simres_recalc, time = unique(bt.pl$Year))

# resids look great: conclude no temproal issues
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# recalculate resids with stations as the grouping variable
#btp.glmm2_simres_recalcSpace <- recalculateResiduals(btp.glmm2_simres, group = as.factor(bt.pl$Station_new))
#unique(bt.pl$Station_new) 
#testSpatialAutocorrelation(btp.glmm2_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# can't really say much with 4 pools but dosn't appear to be clustering - proceed with btp.glmm2
summary(btp.glmm2)
mean_by_site(bt.pl.biomass.station, "yes", "d")
baci.plot(bt.pl.biomass.baci, "b")


### LUNKERS ----
btl.glmm1 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.lu
)

summary(btl.glmm1)

btl.glmm2 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
#  dispformula = ~ Lunker,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.lu
)

summary(btl.glmm2)

anova(btl.glmm1, btl.glmm2)  # the model without dispersion is slightly better

### diagnostics ----
btl.glmm2_simres <- simulateResiduals(btl.glmm2)
# str(btl.glmm2_simres,1)
plot(btl.glmm2_simres)
# The normality and homogeneity of variance look OK.    


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btl.glmm2_simres_recalc <- recalculateResiduals(btl.glmm2_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btl.glmm2_simres_recalc, time = unique(bt.lu$Year))

# resids look pretty good but a few more above 0 than below 

### spatial independence - don't do for lunkers

# temporal resids aren't great so try this
btl.glmm2_new <- glmmTMB(
  #Biomass_100 ~ Lunker + as.numeric(Year) + (1 | Year),
  Biomass_100 ~ Lunker + numYear + (1 | Year),
  #  dispformula = ~ Lunker,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.lu
)

btl.glmm2_simres_new <- simulateResiduals(btl.glmm2_new)
# str(btl.glmm2_simres,1)
plot(btl.glmm2_simres_new)
# The normality and homogeneity of variance look OK.    


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btl.glmm2_simres_recalc_new <- recalculateResiduals(btl.glmm2_simres_new, group = bt.lu$Year)
testTemporalAutocorrelation(btl.glmm2_simres_recalc_new, time = unique(bt.lu$Year))

# this seems better
summary(btl.glmm2_new)
mean_by_site(bt.lu.biomass.station, "lunker", "d")


# BTYOY ----
# using all the data
# btyoyp.gls3 <- gls(Biomass_100~Time, data=btyoy.pl)
# btyoyp.gls4 <- gls(Biomass_100~Time, weights = varIdent(form = ~ 1|Int), data=btyoy.pl)
# anova(btyoyp.gls3, btyoyp.gls4) # suggests Big improvement with this variance structure
# plot(btyoyp.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# btyoyp.lme1 <- lme(Biomass_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=btyoy.pl)
# btyoyp.lme2 <- lme(Biomass_100~Time, random =  ~ 1|Year, data=btyoy.pl)
# anova(btyoyp.lme1, btyoyp.lme2)
# # the anova suggests that the weights really help here 
# summary(btyoyp.lme1)
# plot(btyoyp.lme1)
# # heterogeneity looks awful
# 
# # test normality sucks
# qqnorm(btyoy.pl$resid)
# qqline(btyoy.pl$resid)

#summary(lm(Biomass_100~Time, data=btyoy.pl))

### glmm ----
#### Gamma is OK here because there are no zeros but it doesn't converge
btyoyp.glmm0 <- glmmTMB(
  Biomass_100 ~ Time + numYear + (1 | Year),
  #Biomass_100 ~ Time + I(numYear^2) + (1 | Year),
#  dispformula = ~ Time,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = btyoy.pl
)
summary(btyoyp.glmm0) # this is OK but resids vs time aren't great

#### Use Tweedie instead

btyoyp.glmm1 <- glmmTMB(
  Biomass_100 ~ Time + (1 | Year),
  dispformula = ~ Time,
  family = tweedie,
  REML = TRUE,
  data = btyoy.pl
)

summary(btyoyp.glmm1)


## Compre the results of: glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
btyoyp.glmm2 <- glmmTMB(Biomass_100~Time + (1|Year), 
                     #dispformula = ~ Int,
                     family = tweedie,
                     REML = TRUE,
                     data=btyoy.pl)
summary(btyoyp.glmm2)
#str(btyoyp.glmm2)
anova(btyoyp.glmm1, btyoyp.glmm2) # this suggests that model btyoyp.glmm1 is a fair bit better than btyoyp.glmm2, i.e., with dispersion 
## The estimates are virtually identical but the Std. Errors are a fair bit smaller than the model with dispersion and p-value changes.  Proceed with btyoyp.glmm1

### diagnostics ----
btyoyp.glmm1_simres <- simulateResiduals(btyoyp.glmm0)
# str(btyoyp.glmm1_simres,1)
plot(btyoyp.glmm1_simres)
# The normality is not great but homogeneity is pretty good.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyp.glmm1_simres_recalc <- recalculateResiduals(btyoyp.glmm1_simres, group = bt.pl$Year)
# plot(bt.glmm1_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(btyoyp.glmm1_simres_recalc, time = unique(btyoy.pl$Year))

# resids look domed but ACF is great. 


### spatial independence
# btyoy.pl <- left_join(btyoy.pl, coords, by = c("Station_new" = "Station"))
# #str(btyoy.pl)
# #bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]
# # just coords for pool
# coords.pl <- as.data.frame(coords[c(5:6, 9, 11, 14) ,]) 
# nrow(coords.pl)
# 
# # recalculate resids with stations as the grouping variable
# btyoyp.glmm1_simres_recalcSpace <- recalculateResiduals(btyoyp.glmm1_simres, group = as.factor(bt.pl$Station_new))
# unique(btyoy.pl$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
# #str(bt.glmm1_simres_recalcSpace)
# 
#testSpatialAutocorrelation(btyoyp.glmm1_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# spatial autocorrelation is awful but not sure how to fix.  Proceed with this model - btyoyp.glmm1

### Explore ----
btyoyp.glmm1_new <- glmmTMB(
  #Biomass_100 ~ Time + I(as.numeric(Year)^2) + (1 | Year),
  #Biomass_100 ~ Time + as.numeric(Year) + (1 | Year),
  #Biomass_100 ~ Time + I(numYear^2) + (1 | Year), #NA
  Biomass_100 ~ Time + numYear + (1 | Year),
  dispformula = ~ Time,
  family = tweedie,
  REML = TRUE,
   control = glmmTMBControl(  # gives NAs
     optimizer = optim, 
     optArgs=list(method = "BFGS")),
  data = btyoy.pl
)

summary(btyoyp.glmm1_new)

### diagnostics ----
btyoyp.glmm1_new_simres <- simulateResiduals(btyoyp.glmm1_new)
# str(btyoyp.glmm1_simres,1)
plot(btyoyp.glmm1_new_simres)
# The normality is greatly improved and homogeneity is pretty good.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyp.glmm1_new_simres_recalc <- recalculateResiduals(btyoyp.glmm1_new_simres, group = bt.pl$Year)
# plot(bt.glmm1_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(btyoyp.glmm1_new_simres_recalc, time = unique(btyoy.pl$Year))

# resids look better but ACF is great. 


### spatial independence

# recalculate resids with stations as the grouping variable
# btyoyp.glmm1_new_simres_recalcSpace <- recalculateResiduals(btyoyp.glmm1_new_simres, group = as.factor(bt.pl$Station_new))
# testSpatialAutocorrelation(btyoyp.glmm1_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)
# 
summary(btyoyp.glmm1_new) # go with this
#summary(btyoyp.glmm0) # temp resids aren't great
mean_by_site(btyoy.pl.biomass.station, "yes", "d")
ggplot(btyoy.pl, aes(x = Year, y = Biomass_100)) + geom_point()

### LUNKERS ----

btyoyl.glmm1 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.lu
)

summary(btyoyl.glmm1)

btyoyl.glmm2 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.lu
)

summary(btyoyl.glmm2)

anova(btyoyl.glmm1, btyoyl.glmm2)  # with dispersion is quite a bit better - btyoyl.glmm1

### diagnostics ----
btyoyl.glmm1_simres <- simulateResiduals(btyoyl.glmm1)
#str(btl.glmm1_simres,1)
plot(btyoyl.glmm1_simres)
# The normality is not great but homogeneity of variance look OK.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyl.glmm1_simres_recalc <- recalculateResiduals(btyoyl.glmm1_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btyoyl.glmm1_simres_recalc, time = unique(bt.lu$Year))

# resids seem like they have a trend: not sure what to do


### spatial independence
# recalculate resids with stations as the grouping variable
# btyoyl.glmm1_simres_recalcSpace <- recalculateResiduals(btyoyl.glmm1_simres, group = as.factor(bt.lu$Station_new))
# unique(btyoy.lu$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
# #str(bt.glmm1_simres_recalcSpace)
# 
# testSpatialAutocorrelation(btyoyl.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)
# text(coords.lu$X, coords.lu$Y, labels = coords.lu$Station)
# # spatial autocorrelation looks bad but hard to tell.  Don't proceed with this model yet.

# Explore -----

btyoyl.glmm1_new <- glmmTMB(
  # Biomass_100 ~ Lunker + as.numeric(Year) + (1 | Year),
  #Biomass_100 ~ Lunker*as.numeric(Year) + (1 | Year),
  Biomass_100 ~ Lunker + numYear + (1 | Year),
  #Biomass_100 ~ Lunker + Lunker:as.numeric(Year) + (1 | Year),
  #dispformula = ~ Lunker,
  # family = Gamma(link = log),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.lu
)

summary(btyoyl.glmm1_new)


### diagnostics ----
btyoyl.glmm1_new_simres <- simulateResiduals(btyoyl.glmm1_new)
#str(btl.glmm1_simres,1)
plot(btyoyl.glmm1_new_simres)
# The normality is not great but homogeneity of variance look OK.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyl.glmm1_new_simres_recalc <- recalculateResiduals(btyoyl.glmm1_new_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btyoyl.glmm1_new_simres_recalc, time = unique(bt.lu$Year))

# these are much better - proceed with this model
summary(btyoyl.glmm1_new) # temp resids aren't great - but driven by C3
mean_by_site(btyoy.lu.biomass.station, "lunker", "d")



## AS ----
# using all the data
# asp.gls3 <- gls(Biomass_100~Time, data=as.pl)
# asp.gls4 <- gls(Biomass_100~Time, weights = varIdent(form = ~ 1|Int), data=as.pl)
# anova(asp.gls3, asp.gls4) # suggests Big improvement with this variance structure
# plot(asp.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# asp.lme1 <- lme(Biomass_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=as.pl)
# asp.lme2 <- lme(Biomass_100~Time, random =  ~ 1|Year, data=as.pl)
# anova(asp.lme1, asp.lme2)  # the weights have almost no impact here
# summary(asp.lme1)
# plot(asp.lme1)
# # heterogeneity pretty bad and variance increases with increased fitted value
# 
# # test normality sucks
# qqnorm(asp.lme1$resid)
# qqline(asp.lme1$resid)

#summary(lm(Biomass_100~Time, data=as.pl))

### glmm ----
### But, to do the above and take gamma into account, need glmmTMB
#### the Gamma with dispersion doesn't converge
#### Use Tweedie instead

asp.glmm1 <- glmmTMB(
  Biomass_100 ~ Time + (1 | Year),
  #dispformula = ~ Time,  # doesn't converge
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.pl
)
summary(asp.glmm1)

asp.glmm2 <- glmmTMB(
  Biomass_100 ~ Time + (1 | Year),
#  dispformula = ~ Time, # produces NaN in std errors and 
  family = tweedie,
  REML = TRUE,
  data = as.pl
)
summary(asp.glmm2)

anova(asp.glmm1, asp.glmm2) # this suggests that the Tweedie model with dispersion is a smidge better than the others - asp.glmm2

### diagnostics ----
asp.glmm2_simres <- simulateResiduals(asp.glmm1)
# str(asp.glmm2_simres,1)
plot(asp.glmm2_simres)
# The normality is not great but homogeneity is pretty good.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asp.glmm2_simres_recalc <- recalculateResiduals(asp.glmm2_simres, group = bt.pl$Year)
# plot(bt.glmm2_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(asp.glmm2_simres_recalc, time = unique(as.pl$Year))

# resids look OK for the time and great for autocorrelation: conclude no temproal issues


### spatial independence
# as.pl <- left_join(as.pl, coords, by = c("Station_new" = "Station"))
# #str(bt.pl)
# #as.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]
# # just coords for pool
# coords.pl <- as.data.frame(coords[c(5:6, 9, 11, 14) ,]) 
# #nrow(coords.pl)
# 
# # recalculate resids with stations as the grouping variable
# asp.glmm2_simres_recalcSpace <- recalculateResiduals(asp.glmm2_simres, group = as.factor(bt.pl$Station_new))
# unique(as.pl$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
# #str(bt.glmm1_simres_recalcSpace)
# 
# testSpatialAutocorrelation(asp.glmm2_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# spatial autocorrelation is about as good as it can be but could improve the temporal.  Thry new model

### Explore ----

asp.glmm2_new <- glmmTMB(
  #Biomass_100 ~ Time + I(as.numeric(Year)^2) + (1 | Year),
  Biomass_100 ~ Time + I(numYear^2) + (1 | Year), # gives NAs
  #Biomass_100 ~ Time + as.numeric(Year) + (1 | Year), - problems with resids
  # Biomass_100 ~ Time + (1 | Year),
  # dispformula = ~ Time, # produces NaN in std errors and 
  family = tweedie,
  REML = TRUE,
  # control = glmmTMBControl(  # gives NAs
  #   optimizer = optim, 
  #   optArgs=list(method = "BFGS")),
  data = as.pl
)
summary(asp.glmm2_new)


asp.glmm3 <- glmmTMB(
  Biomass_100 ~ Time + (1 | Year),
  #Biomass_100 ~ Time + numYear + (1 | Year), # bad heterogeneity
    dispformula = ~ Time,
    family = ziGamma(link = "log"),
    ziformula = ~1,
    REML = TRUE,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs=list(method = "BFGS")),
  data = as.pl
)
summary(asp.glmm3)

### diagnostics ----
asp.glmm2_new_simres <- simulateResiduals(asp.glmm3)
# str(asp.glmm2_simres,1)
plot(asp.glmm2_new_simres)
# The normality is not great nor is homogeneity


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asp.glmm2_new_simres_recalc <- recalculateResiduals(asp.glmm2_new_simres, group = bt.pl$Year)
# plot(bt.glmm2_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(asp.glmm2_new_simres_recalc, time = unique(as.pl$Year))

# resids look OK for the time and great for autocorrelation: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
# asp.glmm2_simres_recalcSpace <- recalculateResiduals(asp.glmm2_simres, group = as.factor(bt.pl$Station_new))
# testSpatialAutocorrelation(asp.glmm2_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# The change in the model makes some difference with temporal resids but makes other plots worse.  But this is no where near signficant so proceed with original
summary(asp.glmm3)
mean_by_site(as.pl.biomass.station, "yes", "d")



### LUNKERS ----
asl.glmm1 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.lu
)

summary(asl.glmm1)

asl.glmm2 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  #  dispformula = ~ Lunker,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.lu
)

summary(asl.glmm2)

anova(asl.glmm1, asl.glmm2)  # with dispesion is slightly better but really, no difference

### diagnostics ----
asl.glmm1_simres <- simulateResiduals(asl.glmm1)
plot(asl.glmm1_simres)
# The normality and homogeneity of variance look pretty good.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asl.glmm1_simres_recalc <- recalculateResiduals(asl.glmm1_simres, group = as.lu$Year)
testTemporalAutocorrelation(asl.glmm1_simres_recalc, time = unique(bt.lu$Year))

# resids have a real temporal trend but acf looks great - need to explore


### spatial independence
# recalculate resids with stations as the grouping variable
# asl.glmm1_simres_recalcSpace <- recalculateResiduals(asl.glmm1_simres, group = as.factor(bt.lu$Station_new))
# testSpatialAutocorrelation(asl.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)

# spatial autocorrelation is OK.  Tried exploring a model with Year as covariate but all had residual issues.  P is far from alpha so proceed.  
summary(asl.glmm1)
mean_by_site(as.lu.biomass.station, "lunker", "d")



## ASYOY ----
# # using all the data
# asyoyp.gls3 <- gls(Biomass_100~Time, data=asyoy.pl)
# asyoyp.gls4 <- gls(Biomass_100~Time, weights = varIdent(form = ~ 1|Int), data=asyoy.pl)
# anova(asyoyp.gls3, asyoyp.gls4) # suggests small improvement with this variance structure
# plot(asyoyp.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# asyoyp.lme1 <- lme(Biomass_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=asyoy.pl)
# asyoyp.lme2 <- lme(Biomass_100~Time, random =  ~ 1|Year, data=asyoy.pl)
# anova(asyoyp.lme1, asyoyp.lme2)
# # the anova suggests that the weights don't help here 
# summary(asyoyp.lme1)
# plot(asyoyp.lme1)
# # heterogeneity looks great but variance increases with increased fitted value
# 
# # test normality - really sucks
# qqnorm(asyoyp.lme1$resid)
# qqline(asyoyp.lme1$resid)

#summary(lm(Biomass_100~Time, data=asyoy.pl))

### glmm ----
#### Gamma is OK here because there are no zeros but it doesn't converge
#### Use Tweedie instead

asyoyp.glmm1 <- glmmTMB(
  #Biomass_100 ~ Time + (1 | Year),
  Biomass_100 ~ Time + numYear + (1 | Year),
  dispformula = ~ Time,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = asyoy.pl
)
summary(asyoyp.glmm1)

asyoyp.glmm2 <- glmmTMB(
  #Biomass_100 ~ Time + (1 | Year), # this was better than asyoyp.glmm1 but temporal issues. 
  Biomass_100 ~ Time + numYear + (1 | Year), # this model has good diagnostics
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = asyoy.pl
)
summary(asyoyp.glmm2)

anova(asyoyp.glmm1, asyoyp.glmm2) # these are virtually identical.Use without dispersion.  Further, result is significant so no need to check Tweedie.


### diagnostics ----
asyoyp.glmm2_simres <- simulateResiduals(asyoyp.glmm2)
#str(asyoyp.glmm1_simres,1)
plot(asyoyp.glmm2_simres)
# The normality is not great nor is homogeneity.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoyp.glmm2_simres_recalc <- recalculateResiduals(asyoyp.glmm2_simres, group = as.pl$Year)
testTemporalAutocorrelation(asyoyp.glmm2_simres_recalc, time = unique(asyoy.pl$Year))

# resids look lower in recent years and acf has a pattern: 


### spatial independence

# asyoy.pl <- left_join(as.pl, coords, by = c("Station_new" = "Station"))
# str(as.pl)
# #as.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]
# # just coords for pool
# coords.pl <- as.data.frame(coords[c(5:6, 9, 11, 14) ,]) 
# 
# # recalculate resids with stations as the grouping variable
# asyoyp.glmm2_simres_recalcSpace <- recalculateResiduals(asyoyp.glmm2_simres, group = as.factor(as.pl$Station_new))
# unique(asyoy.pl$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
# #str(as.glmm1_simres_recalcSpace)
# 
# testSpatialAutocorrelation(asyoyp.glmm2_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# spatial autocorrelation is OK but other diagnostics aren't great.  
### Tried using year as a covariate but it didn't help. P-value far from alpha so proceed with 
summary(asyoyp.glmm2)
mean_by_site(asyoy.pl.biomass.station, "yes", "d")
baci.plot(asyoy.pl.biomass.baci, "b")


asyoy.pl[asyoy.pl$Station_new == "D1",]
asyoyp.glmm2_sum <- summary(asyoyp.glmm2)
asyoyp.glmm2_fit <- fitted(asyoyp.glmm2, se.fit = T)
asyoyp.glmm2_ran <- ranef(asyoyp.glmm2)
asyoyp.glmm2_res <- residuals(asyoyp.glmm2)
asyoyp.glmm2_sum$coefficients$zi[1,1]


# this actually seems to work - matches last value 
## https://discourse.mc-stan.org/t/mathematical-notation-for-a-zero-inflated-negative-binomial-model-in-brms/21066/13
## https://www.montana.edu/rotella/documents/502/Prob_odds_log-odds.pdf

# 1/(1 + exp(zi)) = q or event of a zero
# 1/(1 + exp(-zi)) = p or the probability its not a zero or 1-q
## in this case, zi is applied equally to all observations.
exp(asyoy.glmm2_sum$coefficients$cond[1,1] + 
      # asyoy.glmm2_sum$coefficients$cond[2,1] + 
      # asyoy.glmm2_sum$coefficients$cond[3,1] + 
      # asyoy.glmm2_sum$coefficients$cond[4,1] + 
      asyoy.glmm2_ran$cond$Year[11,]) *
  1/(1 + exp(asyoy.glmm2_sum$coefficients$zi[1,1]))
asyoy.glmm2_fit[96]

# this works too - matches first value
exp(asyoyp.glmm2_sum$coefficients$cond[1,1] + 
      asyoyp.glmm2_sum$coefficients$cond[2,1] + 
      asyoyp.glmm2_sum$coefficients$cond[3,1]*1988 + 
      asyoyp.glmm2_ran$cond$Year[1,]) *
  1/(1 + exp(asyoyp.glmm2_sum$coefficients$zi[1,1])) 
asyoyp.glmm2_fit[1]

# first zero value D15 in 1991 Control - After - this matches 
exp(asyoy.glmm2_sum$coefficients$cond[1,1] + 
      # asyoy.glmm2_sum$coefficients$cond[2,1] + 
      # asyoy.glmm2_sum$coefficients$cond[3,1] + 
      # asyoy.glmm2_sum$coefficients$cond[4,1] + 
      asyoy.glmm2_ran$cond$Year[4,]) *
  1/(1 + exp(asyoy.glmm2_sum$coefficients$zi[1,1]))
asyoy.glmm2_fit[15]
asyoy.glmm2_res[15] # so not a good fit but the resid is large and gets it to the actual value which is zero



### LUNKERS ----

asyoyl.glmm1 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = asyoy.lu
)
summary(asyoyl.glmm1)

asyoyl.glmm2 <- glmmTMB(
  Biomass_100 ~ Lunker + (1 | Year),
  family = ziGamma(link = "log"),
  ziformula = ~1,  REML = TRUE,
  data = asyoy.lu
)
summary(asyoyl.glmm2)

anova(asyoyl.glmm1, asyoyl.glmm2)  # without dispesion is slightly better

### diagnostics ----
asyoyl.glmm2_simres <- simulateResiduals(asyoyl.glmm2)
plot(asyoyl.glmm2_simres)
# The normality and homogeneity of variance look OK.  
plot(asyoy.lu$Year, asyoy.lu$Biomass_100)

### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoyl.glmm2_simres_recalc <- recalculateResiduals(asyoyl.glmm2_simres, group = as.lu$Year)
testTemporalAutocorrelation(asyoyl.glmm2_simres_recalc, time = unique(as.lu$Year))

# resids have a real temporal trend as do acfs  - need to explore


### spatial independence

# recalculate resids with stations as the grouping variable
# asyoyl.glmm2_simres_recalcSpace <- recalculateResiduals(asyoyl.glmm2_simres, group = as.factor(as.lu$Station_new))
# testSpatialAutocorrelation(asyoyl.glmm2_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)

# spatial autocorrelation is OK.  Explore model

### new model ----
#### https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
asyoyl.glmm2_new <- glmmTMB(
  #Biomass_100 ~ Lunker + as.numeric(Year) + (1 | Year),
  Biomass_100 ~ Lunker + numYear + (1 | Year),
  #Biomass_100 ~ Lunker + I(numYear^2) + (1 | Year), # gets NAs
  family = ziGamma(link = "log"),
  ziformula = ~1,  REML = TRUE,
  # control = glmmTMBControl(  # gives NAs
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = asyoy.lu
)
summary(asyoyl.glmm2_new)

### diagnostics ----
asyoyl.glmm2_new_simres <- simulateResiduals(asyoyl.glmm2_new)
plot(asyoyl.glmm2_new_simres)
# The normality and homogeneity of variance look OK.  

### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoyl.glmm2_new_simres_recalc <- recalculateResiduals(asyoyl.glmm2_new_simres, group = as.lu$Year)
testTemporalAutocorrelation(asyoyl.glmm2_new_simres_recalc, time = unique(as.lu$Year))
# resids have a bit of a temporal trend as does 

summary(asyoyl.glmm2)
summary(asyoyl.glmm2_new) # go with this - its all driven by one site anyway
mean_by_site(asyoy.lu.biomass.station, "lunker", "d")


# fading plots ----
## riffles ----
### see scratch_pad_glm.R for all the rationale and justification for why this works

# back transformed
bt.np_fit <- fitted(bt.glmm1, se.fit=T)

# log scale
bt.np_pred <- as.data.frame(predict(bt.glmm1, se.fit = T))

# random effect
bt.np_rdm <- ranef(bt.glmm1)


# combine fitted and predicted values with the dataframe
df_bt.np <- as.data.frame(cbind(bt.np, bt.np_fit, bt.np_pred))
df_bt.np[, c(1:2, 6, 12:13, 17, 23:25)]
df1_bt.np <- df_bt.np[, c(1:2, 6, 12:13, 17, 23:25)] |>
  filter(Time == "After" & Treatment == "Impact") 
df1_bt.np

# filter the data frame for mean value by year
df2_bt.np <- df_bt.np |>
  filter(Time == "After" & Treatment == "Impact") |>
  group_by(Year) |>
  summarise(mean = mean(Biomass_100), # this is the mean by year
            fit = mean(fit), # this is teh fixed effect for that year
            se = mean(se.fit) # this is teh SE for that year
  )
# see scratch_pad.glm for notes about calculating standard error

# plot with predicted line, and confidence intervals and predicted value for the Before:impact period
png("output/fade_np_BTbiomass.png", pointsize=10, width=2800, height=2000, res=600)

plot(as.numeric(as.character(df2_bt.np$Year)), df2_bt.np$mean, 
     pch = 16, ylim = c(200, 800), 
     xlab = "Year", ylab = "Mean Biomass Estimate (g/100 sq. m)")

lines(as.numeric(as.character(df2_bt.np$Year)), 
      exp(df2_bt.np$fit), col="black")
# note that i'm not 100% sure if this is calculated correctly as i've just taken the value of se as an average.  There may be some delta method approach involved.
lines(as.numeric(as.character(df2_bt.np$Year)), exp(df2_bt.np$fit+1.96*df2_bt.np$se), col="black", lty=2)
lines(as.numeric(as.character(df2_bt.np$Year)), exp(df2_bt.np$fit-1.96*df2_bt.np$se), col="black", lty=2)

yr_1988 <- df_bt.np$bt.np_fit[1]
yr_1989 <- df_bt.np$bt.np_fit[8]
abline(a=(yr_1988 + yr_1989)/2, b=0,col="blue")
legend(1994,800, text.font=1, 
       c("Mean Biomass - 1991-2016", "Model Fit - Pools", "95% C.I.","Mean Biomass - 1988 & 89"),
       lty=c(0,1,2,1), pch=c(1,NA,NA,NA), cex = 0.75, # gives the legend appropriate symbols (lines)
       col=c("black", "black","black","blue" )) # gives the legend lines the correct color and width
# puts text in the legend 

dev.off()



## pools ----
### the below figure has an almost straight line for the mean and CIs which is a bit wonky.  Why?  Basically, the Year random effect has variance that is almost zero.  The below FAQ page documents what "singular effects" are and why they occur and what to do about it.  
#### https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
#### From the above page: "If a variance component is zero, dropping it from the model will have no effect on any of the estimated quantities (although it will affect the AIC, as the variance parameter is counted even though it has no effect). Pasch, Bolker, and Phelps (2013) gives one example where random effects were dropped because the variance components were consistently estimated as zero. Conversely, if one chooses for philosophical grounds to retain these parameters, it won’t change any of the answers."
##### I ran the same model without the random effect and the conditional model was exactly the same.  Therefore, keeping the random effects in on philosophical grounds.  

# back transformed
bt.pl_fit <- fitted(btp.glmm2)

# log scale
bt.pl_pred <- as.data.frame(predict(btp.glmm2, se.fit = T))

# random effect
bt.pl_rdm <- ranef(btp.glmm2)

# fitted effects design matrix and transpose
mm <- as.matrix(model.matrix(~Time, data = bt.pl))
mmt <- as.matrix(t(mm[1:33, 1:2]))


# variance- covariance
vcov(btp.glmm2)
vc <- vcov(btp.glmm2, full = T)
vc$cond[1:2, 1:2]

VarCorr(btp.glmm2) # this is just variance of the random effect
VarCorr(btp.glmm2)[[c("cond", "Year")]]

# SE formula - from ChatGPT for the linear predictor and only for fixed effects
mm%*%vc$cond%*%mmt
diag(mm%*%vc$cond%*%mmt)
sqrt(diag(mm%*%vc$cond%*%mmt)) # this almost perfectly matches the SE of the predicted values but I think its because the variance for the random effect is so small, i.e, if the variance for the randome effect was larger, it would be a problem.  The next step is SE for the fitted values which is the delta method - derivative of the fitted value X SE of the linear preditor.

# for random effects - its not clear to me how to extract the vc matrix but then, it needs to be added to the fitted values.  Then, the 


summary(glmmTMB(Biomass_100~Time, 
        family = Gamma(link=log),
        REML = TRUE,
        data=bt.pl)
)


# combine fitted and predicted values with the dataframe
df_bt.pl <- as.data.frame(cbind(bt.pl, bt.pl_fit, bt.pl_pred))
df_bt.pl[, c(1:2, 6, 12:13, 17, 23:25)]
df1_bt.pl <- df_bt.pl[, c(1:2, 6, 12:13, 17, 23:25)] |>
  filter(Time == "After") 
df1_bt.pl

# filter the data frame for mean value by year
df2_bt.pl <- df_bt.pl |>
  filter(Time == "After") |>
  group_by(Year) |>
  summarise(mean = mean(Biomass_100), # this is the mean by year
            fit = mean(fit), # this is teh fixed effect for that year
            se = mean(se.fit) # this is teh SE for that year
  )
# see scratch_pad.glm for notes about calculating standard error

# plot with predicted line, and confidence intervals and predicted value for the Before:impact period

png("output/fade_pool_BTbiomass.png", pointsize=10, width=2800, height=2000, res=600)
plot(as.numeric(as.character(df2_bt.pl$Year)), df2_bt.pl$mean, pch = 16, ylim = c(200, 2000), xlab = "Year", ylab = "Mean Biomass Estimate (g/100 sq. m)")
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit), col="black")
# note that i'm not 100% sure if this is calculated correctly as i've just taken the value of se as an average.  There may be some delta method approach involved.
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit+1.96*df2_bt.pl$se), col="black", lty=2)
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit-1.96*df2_bt.pl$se), col="black", lty=2)

yr_1988 <- df_bt.pl$bt.pl_fit[1]
yr_1989 <- df_bt.pl$bt.pl_fit[8]
abline(a=(yr_1988 + yr_1989)/2, b=0,col="blue")

dev.off()

# END ----

