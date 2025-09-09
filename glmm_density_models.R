# source ----
source("glmm_data.R")
source("glmm_fun.R")

#source("glmm_biomass_models.R")
# library ----

library(nlme)
library(glmmTMB)
library(DHARMa)
library(readr)
library(tidyr)
#library(dplyr)
library(sf)
library(ggplot2)


# riffles ----
## BT ----
# ## glm ----
# # Cote approach - biomass pooled
# BT1_den.glm.not.pool.full <- glm(Density_100~Time*Treatment, family=Gamma(link=log), data=bt.np)
# summary(BT1_den.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(BT1_den.glm.not.pool.full) #  variance is not homogeneous and normality of resids is poor
# graphics.off()
# 
# 
# ## gls ----
# # the Cote way
# bt_den.gls1 <- gls(mean~Time*Treatment, data=BTbyhabitat.not.pool)
# bt_den.gls2 <- gls(mean~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=BTbyhabitat.not.pool)
# anova(bt_den.gls1, bt_den.gls2) # suggests no improvement with this variance structure
# 
# # using all the data
# bt_den.gls3 <- gls(Density_100~Time*Treatment, data=bt.np)
# bt_den.gls4 <- gls(Density_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=bt.np)
# anova(bt_den.gls3, bt_den.gls4) # suggests Big improvement with this variance structure
# plot(bt_den.gls4)        # but variance is still heterogeneous
# 
# ## lmm ----
# bt_den.lme1 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=bt.np)
# bt_den.lme2 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, data=bt.np)
# anova(bt_den.lme1, bt_den.lme2)
# # the anova suggests that the weights really help here 
# summary(bt_den.lme1)
# plot(bt_den.lme1)
# # heterogeneity looks great but variance increases slightly with increased fitted value
# 
# # test normality sucks
# qqnorm(bt_den.lme1$resid)
# qqline(bt_den.lme1$resid)
# 
# # the following is to try and understand why the fit v res plot has so many values - Basically, we have 11 years and two categorical variables - time (before/after) and trt (control/impact).  So, 11x2 =22 but in 1990, only the controls were sampled so 21.
# bt.np$fits_den <- fitted(bt_den.lme1)
# unique(fitted(bt_den.lme1))
# bt.np$resid_den <- resid(bt_den.lme1, type = "normalized")
# #mm <- model.matrix(~Time*Treatment, data = bt.np)
# #write.csv(mm[1:98, 1:4], "mm.csv")
# #View(bt.np[, c(2,4,6, 7, 13, 14, 18:20)])
# 

## glmm ----
#### Gamma is OK here because there are no zeros
bt_den.glmm1 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  #Density_100 ~ Time * Treatment + (1 | Station_new) + (1 | Year),
  dispformula = ~ Int,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.np
)

bt1 <- summary(bt_den.glmm1)
bt2 <- summary(bt_den.glmm1)
#str(bt_den.glmm1)
model.matrix(bt_den.glmm1)

plot(bt.np$Year, bt.np$Density_100)
p <- ggplot(data = bt.np, aes(x = Year, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(vars(Treatment), vars(Time))
p


## Compre the results of: ### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt_den.glmm2 <- glmmTMB(
  Density_100~Time*Treatment + (1|Year),
  #Density_100 ~ Time * Treatment + (1 | Station_new) + (1 | Year),
    #dispformula = ~ Int,
    family = Gamma(link=log),
    REML = TRUE,
  data=bt.np)
summary(bt_den.glmm2)
#str(bt_den.glmm2)
anova(bt_den.glmm1, bt_den.glmm2) # this suggests that model bt_den.glmm1 with dispersion is slightly better than without (bt_den.glmm2)


## diagnostics ----
bt_den.glmm1_simres <- simulateResiduals(bt_den.glmm2)
# str(bt_den.glmm1_simres,1)
plot(bt_den.glmm1_simres)
# The normality and homogeneity of variance look great.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt_den.glmm1_simres_recalc <- recalculateResiduals(bt_den.glmm1_simres, group = bt.np$Year)
testTemporalAutocorrelation(bt_den.glmm1_simres_recalc, time = unique(bt.np$Year))
# resids look OK: Third value is over the line but can't see what a lag at 3 time slots would mean for this time series.  conclude no temproal issues
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# join coordinates with data set - may not be required
#bt.np <- left_join(bt.np, coords, by = c("Station_new" = "Station"))
#str(bt.np)
#bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]

# just coords for non-pool
# coords.np <- as.data.frame(coords[c(1:4, 7:8, 10, 12:13, 15:18) ,]) 
# nrow(coords.np)

# recalculate resids with stations as the grouping variable
bt_den.glmm1_simres_recalcSpace <- recalculateResiduals(bt_den.glmm1_simres, group = as.factor(bt.np$Station_new))
unique(bt.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt_den.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(bt_den.glmm1_simres_recalcSpace, x = unique(bt.np$X), y = unique(bt.np$Y))

spatialAutoCorrBase_fun(bt.np, bt_den.glmm1_simres_recalcSpace)  
bt.np.density.all <- spatialData_join(bt.np.density.station[-4,], bt_den.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.density.all)

# Diagnostics look fantastic for this model expect for a slight problem with temp autocorrelation at lag 3.  Proceed with this model - bt_den.glmm1. 

summary(bt_den.glmm1)
car::Anova(bt_den.glmm1)
mean_by_site(bt.np.density.station, "no", "d")
baci.plot(bt.np.density.baci, "d")

ggplot(bt.pl.density.station, aes(as.factor(Station_new), mean)) +
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  #facet_grid(~Treatment) +
#  facet_grid(forcats::fct_rev(Treatment))
  facet_grid(~factor(Time, levels = c("Before", "After")))


## create CIs ----
tab.ci(bt_den.glmm1, "bt_den") 

tmp <- confint(bt_den.glmm1)
tmp[, c(3, 1:2)]
(exp(tmp[1,3] + tmp[3,3]))/exp(tmp[1,3])*100
((exp(tmp[1,3] + tmp[3,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

(exp(tmp[3,3]) -1)*100




# BTYOY ----
## Cote approach
# BTYOY1.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data = BTYOYdensitybyhabitat.not.pool)
# summary(BTYOY1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(BTYOY1.glm.not.pool.full) # normality is not great and homogeneity is pretty bad
# graphics.off()
# 
# for btyoy.np and btyoy.np.001, see glmm_biomass_models and also for check of zeros

# 4 of 98 values are 0.  So, there are a number of options.
## #1 inflate the zeros slightly
## #2 fall back on lmm with a non-gamma distribution
## #3 zero inflated or hurdle
## #4 tweadie
## # This website gives a great discussion on zi v hurdle and when to use them.  Basically, gamma has to be positive real number.  Is the zero there due to sampling (they just weren't there) or structure (you could well have no btyoy in some places).  This website advises that zi models are a mix while hurdle models have only structural zeros. Clarke suggested in an email that these are structural zeros as larger conspecifics will eat the smaller fish, ergo use a hurdle model.
### https://biol609.github.io/lectures/13_zinf.html#53_the_error_generating_process


# ## glm ----
# ### same as Cote but with biomass for all sites AND biomass adjustment
# BTYOY1.glm.not.pool.full <- glm(Density_100~Time*Treatment, family=Gamma(link=log), data=btyoy.np.001)
# summary(BTYOY1.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(BTYOY1.glm.not.pool.full) # resids are awful
# graphics.off()
# 
# ## gls ----
# # using all the data
# btyoy.gls1 <- gls(Density_100~Time*Treatment, data=btyoy.np)
# btyoy.gls2 <- gls(Density_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=btyoy.np)
# anova(btyoy.gls1, btyoy.gls2) # suggests massive improvement with this variance structure
# plot(btyoy.gls1)        # but variance is still heterogeneous
# 
# ## lmm ----
# # btyoy.gls2 helps the variance issue a bit - let's try random effect
# btyoy.lme1 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=btyoy.np)
# btyoy.lme2 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, data=btyoy.np)
# anova(btyoy.lme1, btyoy.lme2)
# # the anova suggests a massive improvement with the weights 
# summary(btyoy.lme1)
# plot(btyoy.lme1)
# # heterogeneity looks pretty darn good although a few low variance at low fitted values and some curving in the middle.  But let's see what a more appropriate model will do.

## glmm -----
### with biomass adjustment - just running this to see if it makes much difference as it isn't "right"
# btyoy.glmm1 <- glmmTMB(
#   Density_100 ~ Time * Treatment + (1 | Year),
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
# ## Compre the results of: glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
# ### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
# btyoy.glmm2 <- glmmTMB(Density_100~Time*Treatment + (1|Year), 
#                        #dispformula = ~ Int,
#                        family = Gamma(link=log),
#                        REML = TRUE,
#                        data=btyoy.np.001)
# summary(btyoy.glmm2)
# # str(btyoy.glmm2)
# anova(btyoy.glmm1, btyoy.glmm2) # this suggests that model with dispersion is quite a bit better than the model without.
## The estimates are virtually identical but the Std. Errors differ and could affect inferences.  Further, the diagnostics look better much better for this model, especially for homogeneity of variance and this model has a much better AIC (note that this code has been deleted as I think the "alt distributions approach is best").


## alt distributions ----
### I did zi gamma and tweadie before I did the hurdle.  But it seems like hurdle models are most appropriate. 

### zero-inflated gamma - not converging
# btyoy.glmm3 <- glmmTMB(
#   Density_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = TRUE,
#   data = btyoy.np
# )
# summary(btyoy.glmm3)

### hurdle gamma without dispersion
btyoy.glmm3a <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~1,
  REML = TRUE,
  data = btyoy.np
)
summary(btyoy.glmm3a)
#anova(btyoy.glmm3, btyoy.glmm3a)

# this shows that the model with dispersion is MUCH better - compare to tweadie below

### tweadie
btyoy.glmm4 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  dispformula = ~ Int,
  #  family = inverse.gaussian(link = "1/mu^2"),
  #  family = Gamma(link="identity"),
  # family=ziGamma(link="log"), ziformula = ~1,
  family = tweedie, # link is log
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim, 
    optArgs=list(method = "BFGS")),
  data = btyoy.np
)
summary(btyoy.glmm4)
anova(btyoy.glmm3a, btyoy.glmm4) # Tweedie looks alot better but only with the dispformula term - proceed to check diagnostics - btyoy.glmm4


## diagnostics ----
# btyoy.glmm4 model is best but is it valid.  
btyoy.glmm4_simres <- simulateResiduals(btyoy.glmm4)
plot(btyoy.glmm4_simres)
# The normality and homogeneity of variance look great.  

# test zero inflation which looks fine.
testZeroInflation(btyoy.glmm4_simres)


# temporal independence
## Because this is a nested model with multiple measurements by Year, we can't just do the below.
# rather, we need to recalculate them with Year as a grouping variable
btyoy.glmm4_simres_recalc <- recalculateResiduals(btyoy.glmm4_simres, group = btyoy.np$Year)
testTemporalAutocorrelation(btyoy.glmm4_simres_recalc, time = unique(btyoy.np$Year))
# somewhat curvilinear pattern and autocorrelation is close to alpa - revise


# spatial independence
# recalculate resids with stations as the grouping variable
btyoy.glmm4_simres_recalcSpace <- recalculateResiduals(btyoy.glmm4_simres, group = as.factor(btyoy.np$Station_new))

testSpatialAutocorrelation(btyoy.glmm4_simres_recalcSpace, x = unique(btyoy.np$X), y = unique(btyoy.np$Y)) # spatial pattern in pretty bad

# Some question on temporal pattern and spatial pattern is pretty bad for btyoy.glmm4 and some p-vales are close to alpha- explore new model 

### New model ----


# values decline after a high point for control

btyoy.np1 <- btyoy.np
btyoy.np1$Year <- as.numeric(as.character(btyoy.np$Year))
btyoy.np1$Station_new <- as.factor(btyoy.np$Station_new)

plot(btyoy.np$Year, btyoy.np$Density_100)
ggplot(data = btyoy.np, aes(x = Year, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Time))

ggplot(data = btyoy.np1, aes(x = Station_new, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Time))

ggplot(data = btyoy.np1, aes(x = Station_new, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Habitat))

# tried numerous models with various correlations structures (us, cs, ar1) and random effects - found that it was best to use different convergence methods as model often wouldn't converge but it still produced poor temporal resids.  First, try random slope/intercept model.  Then, try a covariate

btyoy.glmm4 <- glmmTMB(
  Density_100 ~ Time * Treatment +  (1 + Treatment| Year),
  dispformula = ~ Int,
  family = tweedie, # link is log
  REML = TRUE,
  data = btyoy.np
)
btyoy.glmm4_optim <- update(btyoy.glmm4, 
       control = glmmTMBControl(
         optimizer = optim, 
         optArgs=list(method = "BFGS"))
)

# trying the covariate - using as.numeric(Year) helps with temporal correlation although there are bands but spatial is awful.  

btyoy.np$pos <- numFactor(btyoy.np$X, btyoy.np$Y)
btyoy.np1$pos <- numFactor(btyoy.np$X, btyoy.np$Y)

# just a covariate
btyoy.glmm4_optim <- glmmTMB(
  # Density_100 ~ Time * Treatment + Time*as.numeric(Year)  + (1 | Year), # great except for spatial AIC = 739.8
#    Density_100 ~ Time * Treatment + as.numeric(Year)  + (1 | Year), # great except for spatial; AIC = 737
    Density_100 ~ Time * Treatment + numYear + (1 | Year), # spatial resids aren't great
    # Density_100 ~ Time * Treatment + Y + (1 | Year), # NAs
    #    Density_100 ~ Time * Treatment + I(numYear^2) + (1 | Year), # NA
    
        # Density_100 ~ Time * Treatment + I(as.numeric(Year)^2)  + (1 | Year), # doesn't converge
  # Density_100 ~ Time * Treatment + Treatment*I(as.numeric(Year)^2)  + (1 | Year), # great except for spatial AIC = 749
  # Density_100 ~ 1 + exp(pos + 0 | Treatment), # can't get convergence with pos
  dispformula = ~ Int, # removing dispformula for ziGamma - spatial autocorrelation < 0.05
  family = tweedie, # link is log
 # family=ziGamma(link="log"), ziformula = ~1, # spatial autocorrelation < 0.05
  #family=ziGamma(link="log"), ziformula = ~1 + Type, # spatial autocorrelation < 0.05
   REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim, 
    optArgs=list(method = "BFGS")),
  data = btyoy.np
)
# The best AIC is this: Density_100 ~ Time * Treatment + as.numeric(Year)  + (1 | Year)
summary(btyoy.glmm4_optim)       

btyoy.glmm4_optim_simres <- simulateResiduals(btyoy.glmm4_optim)
plot(btyoy.glmm4_optim_simres)
# The normality and homogeneity of variance look great.  

# test zero inflation which looks fine.
testZeroInflation(btyoy.glmm4_optim_simres)


# temporal independence
btyoy.glmm4_optim_simres_recalc <- recalculateResiduals(btyoy.glmm4_optim_simres, group = btyoy.np$Year)
testTemporalAutocorrelation(btyoy.glmm4_optim_simres_recalc, time = unique(btyoy.np$Year))

# temp resis look great although maybe some pattern in last few years, it doesn't look serious

# spatial independence
btyoy.glmm4_optim_simres_recalcSpace <- recalculateResiduals(btyoy.glmm4_optim_simres, group = as.factor(bt.np$Station_new))

testSpatialAutocorrelation(btyoy.glmm4_optim_simres_recalcSpace, x = unique(btyoy.np$X), y = unique(btyoy.np$Y)) # spatial pattern in pretty bad

spatialAutoCorrBase_fun(btyoy.np, btyoy.glmm4_optim_simres_recalcSpace)   

btyoy.np.density.all <- spatialData_join(bt.np.density.station[-4,], btyoy.glmm4_optim_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(btyoy.np.density.all)


# Can't get this one to look good - use btyoy.glmm4_optim as this at least fixes the temporal resids.
summary(btyoy.glmm4_optim)   
mean_by_site(btyoy.np.density.station, "no", "d")
baci.plot(btyoy.np.density.baci, "d")
ggplot(btyoy.np, aes(x = Year, y = Density_100)) + geom_point()

# tmp <- summary(btyoy.glmm4_optim)
# tmp$coefficients$cond
# tmp$coefficients
# 
# tmp2 <- predict(btyoy.glmm4_optim)
# 
# test <- edited_cs_estimates_by_site |>
#   filter(Pool == "No" & Species == "BTYOY") |>
#   group_by(Year) |>
#   summarise(N  = length(Density_100),
#             mean = mean(Density_100),
#             sd   = sd(Density_100),
#             se   = sd / sqrt(N))
# 
# plot(test$Year, test$mean, pch = 4)
# lines(test$Year, )

## create CIs ----
tab.ci(btyoy.glmm4_optim, "btyoy_den")
tmp <- confint(btyoy.glmm4_optim)
tmp[1:5, c(3, 1:2)]

# tested whether using numYear made a difference when calculating the percent change - it appears that they do not. I was worried about this because (see ReadMe for greater detail) including numYear massively changes the Intercept term and does this affect the percent change?  I calculated the mean value 
year <- as.data.frame(unique(btyoy.np$Year))
colnames(year) <- "year"
year$year <- as.numeric(as.character(year$year))

yearB <- mean(tmp[4,3]*year$year[1:3])
yearA <- mean(tmp[4,3]*year$year[4:11])

# just parm for TIME
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100
# average value of Before is exact same as above
(exp(tmp[1,3] + tmp[2,3] + yearB)-exp(tmp[1,3] + yearB))/exp(tmp[1,3] + yearB)*100
# as is this with just one year - so clearly, the numYears cancel each other out
(exp(tmp[1,3] + tmp[4,3]*1990 + tmp[2,3]) - exp(tmp[1,3] + tmp[4,3]*1990))/exp(tmp[1,3]+ tmp[4,3]*1990)*100

# Treatment
((exp(tmp[1,3] + tmp[3,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

# test against simpler model and results are close enough to be getting on with
tmp1 <- confint(btyoy.glmm3a)
((exp(tmp1[1,3] + tmp1[2,3]))-exp(tmp1[1,3]))/exp(tmp1[1,3])*100
((exp(tmp1[1,3] + tmp1[3,3]))-exp(tmp1[1,3]))/exp(tmp1[1,3])*100



# AS ----
## resids in Cote analysis aren't bad but can we improve them?
# ASdensitybyhabitat.not.pool$mean <- ifelse(ASdensitybyhabitat.not.pool$mean == 0, 0.01, ASdensitybyhabitat.not.pool$mean)
# AS1_den.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data= ASdensitybyhabitat.not.pool)
# summary(AS1_den.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(AS1_den.glm.not.pool.full) # heterogeneity is not awful nor is normality
# graphics.off()
# 
# 
# # subset data
# # for btyoy.np and btyoy.np.001, see glmm_biomass_models and also for check of zeros
# # 15 of 99 values are 0. See above options.
# 
# 
# ## glm ----
# ### same as Cote but with biomass for all sites
# AS1_den.glm.not.pool.full <- glm(Density_100~Time*Treatment, family=Gamma(link=log), data=subset(as.np, Density_100 > 0))
# summary(AS1_den.glm.not.pool.full)
# 
# par(mfrow=c(2,2))
# plot(AS1_den.glm.not.pool.full) # resids awful
# graphics.off()
# 
# ## gls ----
# # using all the data
# as_den.gls1 <- gls(Density_100~Time*Treatment, data=as.np)
# as_den.gls2 <- gls(Density_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=as.np)
# anova(as_den.gls1, as_den.gls2) # suggests no improvement with this variance structure
# plot(as_den.gls1)        # and variance is still heterogeneous
# 
# ## lmm ----
# # as_den.gls2 helps the variance issue a bit - let's try random effect
# as_den.lme1 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=as.np)
# as_den.lme2 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, data=as.np)
# anova(as_den.lme1, as_den.lme2)
# # the anova suggests that the weights help a lot
# summary(as_den.lme1)
# plot(as_den.lme1)
# # heterogeneity doesn't look great - definitely less on the left
# 

## glmm -----
as_den.glmm1 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  dispformula = ~ Int,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = as.np
)


#summary(as_den.glmm1)
# str(as_den.glmm1)
# I get the error "Error in solve.default(as.matrix(Qm)) : system is computationally singular: reciprocal condition number = 1.37471e-19".  A brief google search suggests multicollinearity

## Compre the results of glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as_den.glmm2 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = as.np
)

summary(as_den.glmm2) # can't compare as the model with dispersion doesn't converge.  Go to Tweedie

### Tweedie
as_den.glmm3 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  family = tweedie, # link is log
  REML = TRUE,
  data = as.np
)
summary(as_den.glmm3)
anova(as_den.glmm2, as_den.glmm3) # tweadie looks only slightly better but proceed with it - as_den.glmm3


## diagnostics ----

# OK, the as_den.glmm3 model is best.  Is it valid.  
as_den.glmm3_simres <- simulateResiduals(as_den.glmm3)
# str(as_den.glmm3_simres,1)
plot(as_den.glmm3_simres)
# The normality is great - homogeneity of variance isn't too bad but not significant.  
# test zero inflation which looks fine.
testZeroInflation(as_den.glmm3_simres)

# temporal independence
## Because this is a nested model with multiple measurements by Year, we can't just do the below.
# rather, we need to recalculate them with Year as a grouping variable
as_den.glmm3_simres_recalc <- recalculateResiduals(as_den.glmm3_simres, group = as.np$Year)
testTemporalAutocorrelation(as_den.glmm3_simres_recalc, time = unique(as.np$Year))
# resids aren't great - temporal trend funny but acf looks great.


# spatial independence
# recalculate resids with stations as the grouping variable
as_den.glmm3_simres_recalcSpace <- recalculateResiduals(as_den.glmm3_simres, group = as.factor(as.np$Station_new))

testSpatialAutocorrelation(as_den.glmm3_simres_recalcSpace, x = unique(as.np$X), y = unique(as.np$Y))

# autocorrelation is great but temp not so good

### Explore---- 
plot(as.np$Year, as.np$Density_100) # clear linear increase
p <- ggplot(data = as.np, aes(x = Year, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Treatment))
p # especially on impact sites

# this seems to suggest a real temporal trend in the data so add Year as a numeric variable
as_den.glmm3_new <- glmmTMB(
  Density_100 ~ Time * Treatment + numYear + (1 | Year),
  #Density_100 ~ Time * Treatment + as.numeric(Year) + (1 | Year),
#  Density_100 ~ Time * Treatment + as.numeric(Year) + (1 | Year) + (1 | Station_new),
  #Density_100 ~ Time * Treatment +  (1 | Year),
  family = tweedie, # link is log
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim, 
    optArgs=list(method = "BFGS")),
  data = as.np
)
summary(as_den.glmm3_new)


# redo Diagnostics
as.glmm4_new_simres <- simulateResiduals(as_den.glmm3_new)
plot(as.glmm4_new_simres)
# The normality and homogeneity of variance look great.  

# test zero inflation which looks fine.
testZeroInflation(as.glmm4_new_simres)

# temporal independence
as.glmm4_new_simres_recalc <- recalculateResiduals(as.glmm4_new_simres, group = btyoy.np$Year)
testTemporalAutocorrelation(as.glmm4_new_simres_recalc, time = unique(btyoy.np$Year))
# some temporal pattern and acf look great

# spatial independence
as.glmm4_new_simres_recalcSpace <- recalculateResiduals(as.glmm4_new_simres, group = as.factor(bt.np$Station_new))
testSpatialAutocorrelation(as.glmm4_new_simres_recalcSpace, x = unique(as.np$X), y = unique(as.np$Y)) # spatial pattern looks great

spatialAutoCorrBase_fun(as.np, as.glmm4_new_simres_recalcSpace)   

as.np.density.all <- spatialData_join(as.np.density.station[-4,], as.glmm4_new_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(as.np.density.all)

# Accept this model -as_den.glmm3_new
summary(as_den.glmm3_new)
mean_by_site(as.np.density.station, "no", "d")
baci.plot(as.np.density.baci, "d")


## create CIs ----
tab.ci(as_den.glmm3_new, "as_den")
tmp <- confint(as_den.glmm3_new)
tmp[, c(3, 1:2)]
((exp(tmp[1,3] + tmp[3,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100



## ASYOY ----
## resids in Cote analysis aren't bad but can we improve them?
# ASYOY1_den.glm.not.pool.full <- glm(mean~Time*Treatment, family=Gamma(link=log), data= ASYOYdensitybyhabitat.not.pool)
# summary(ASYOY1_den.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(ASYOY1_den.glm.not.pool.full) # resids aren't great.  Heterogeneity and some issues iwth normality
# graphics.off()
# 
# # examine distribution and check for zeros
# hist(asyoy.np$Density_100)
# # length(asyoy.np$Density_100)
# # length(asyoy.np$Density_100[asyoy.np$Density_100 == 0])
# # asyoy.np[asyoy.np$Density_100 == 0,]
# # asyoy.np[asyoy.np$Density_100 < 0,]
# plot(density(asyoy.np$Density_100))
# 
# # 20 of 130 values are 0. See above options.
# 
# 
# ## glm ----
# ### same as Cote but with biomass for all sites
# ASYOY1_den.glm.not.pool.full <- glm(Density_100~Time*Treatment, family=Gamma(link=log), data=subset(asyoy.np, Density_100 > 0))
# summary(ASYOY1_den.glm.not.pool.full)
# par(mfrow=c(2,2))
# plot(AS1_den.glm.not.pool.full) # resids are awful
# graphics.off()
# 
# 
# ## gls ----
# # using all the data
# asyoy_den.gls1 <- gls(Density_100~Time*Treatment, data=asyoy.np)
# asyoy_den.gls2 <- gls(Density_100~Time*Treatment, weights = varIdent(form = ~ 1|Int), data=asyoy.np)
# anova(asyoy_den.gls1, asyoy_den.gls2) # suggests massive improvement with this variance structure
# plot(asyoy_den.gls2)        # and variance is still heterogeneous but not bad
# 
# ## lmm ----
# # as.gls2 helps the variance issue a bit - let's try random effect
# asyoy.lme1 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=asyoy.np)
# asyoy.lme2 <- lme(Density_100~Time*Treatment, random =  ~ 1|Year, data=asyoy.np)
# anova(asyoy.lme1, asyoy.lme2)
# # the anova suggests that the weights help massively
# summary(asyoy.lme1)
# plot(asyoy.lme1)
# # heterogeneity looks OK for lower and middle fitted values 
# 

## glmm -----
asyoy_den.glmm1 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
    dispformula = ~ Int,
    family = ziGamma(link = "log"),
    ziformula = ~1,
    REML = TRUE,
    control = glmmTMBControl(
      optimizer = optim, 
      optArgs=list(method = "BFGS")),
  data = asyoy.np
)
summary(asyoy_den.glmm1) 


## Compre the results of glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
asyoy_den.glmm2 <- glmmTMB(
  Density_100~Time*Treatment + (1|Year), 
       family = ziGamma(link = "log"),
       ziformula = ~1,
       REML = TRUE,
     data=asyoy.np)
summary(asyoy_den.glmm2)
#str(asyoy_den.glmm2)

## The estimates are virtually identical as are the Std. Errors .  Further, the diagnostics look better much better for this model, especially for homogeneity of variance and this model has a much better AIC.

# OK, the asyoy.glmm1 model is not valid.  Try a Tweedie

asyoy_den.glmm3 <- glmmTMB(
  Density_100 ~ Time * Treatment + (1 | Year),
  family = tweedie,
  REML = TRUE,
  data = asyoy.np
)
summary(asyoy_den.glmm3)
anova(asyoy_den.glmm1, asyoy_den.glmm2)
anova(asyoy_den.glmm2, asyoy_den.glmm3)
anova(asyoy_den.glmm1, asyoy_den.glmm3)# ziGamma with dispersion is best.  Go with asyoy_den.glmm1

# OK - the glmm2 model is slightly better than the others but the homogeneity of variance is bad.  glmm1 is better than glmm3 so go with this one.

## diagnostics ----
asyoy_den.glmm1_simres <- simulateResiduals(asyoy_den.glmm1)
plot(asyoy_den.glmm1_simres)
# The normality is OK - homogeneity of variance is not great but we'll take it

# test zero inflation which looks fine.
testZeroInflation(asyoy_den.glmm1_simres)

# temporal independence
## Because this is a nested model with multiple measurements by Year, we need to recalculate them with Year as a grouping variable
asyoy_den.glmm1_simres_recalc <- recalculateResiduals(asyoy_den.glmm1_simres, group = asyoy.np$Year)
testTemporalAutocorrelation(asyoy_den.glmm1_simres_recalc, time = unique(asyoy.np$Year))
# resids look great but maybe a little low in last three years

# spatial independence
## Need UTMs of the sites.  However, the unique stations are confusing. 
asyoy_den.glmm1_simres_recalcSpace <- recalculateResiduals(asyoy_den.glmm1_simres, group = as.factor(asyoy.np$Station_new))
testSpatialAutocorrelation(asyoy_den.glmm1_simres_recalcSpace, x = unique(asyoy.np$X), y = unique(asyoy.np$Y))

spatialAutoCorrBase_fun(asyoy.np, asyoy_den.glmm1_simres_recalcSpace)   

asyoy.np.density.all <- spatialData_join(asyoy.np.density.station[-4,], asyoy_den.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(asyoy.np.density.all)

# spatial indenendence is fine - proceed with model - asyoy_den.glmm3
summary(asyoy_den.glmm1)
mean_by_site(asyoy.np.density.station, "no", "d")
baci.plot(asyoy.np.density.baci, "d")


temp <- asyoy.np |>
  filter(Time == "Before") |>
  group_by(Station_new) |>
    summarise(mean = mean(Density_100))

temp1 <- asyoy.np |>
  filter(Time == "Before") |>
  group_by(Treatment) |>
  summarise(mean = mean(Density_100))


## create CIs ----
tab.ci(asyoy_den.glmm1, "asyoy_den")

tmp <- confint(asyoy_den.glmm1)
tmp[1:5, c(3, 1:2)]
# TRT
((exp(tmp[1,3] + tmp[3,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100
# Int
((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

# POOLS ----
# POOLS ----
## data ----
### create data sets for all species

# BT ----
# # using all the data
# btp_den.gls3 <- gls(Density_100~Time, data=bt.pl)
# btp_den.gls4 <- gls(Density_100~Time, weights = varIdent(form = ~ 1|Int), data=bt.pl)
# anova(btp_den.gls3, btp_den.gls4) # suggests no improvement with this variance structure
# plot(btp_den.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# btp_den.lme1 <- lme(Density_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=bt.pl)
# btp_den.lme2 <- lme(Density_100~Time, random =  ~ 1|Year, data=bt.pl)
# anova(btp_den.lme1, btp_den.lme2)
# # the anova suggests that the weights really don't help much here 
# summary(btp_den.lme1)
# plot(btp_den.lme1)
# # heterogeneity looks poor plus variance increases with increased fitted value
# 
# # normality pretty bad
# qqnorm(btp_den.lme1$resid)
# qqline(btp_den.lme1$resid)


### glmm ----
#### Gamma is OK here because there are no zeros
btp_den.glmm1 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  dispformula = ~ Time,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.pl
)
summary(btp_den.glmm1)


## Compre the results of glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
btp_den.glmm2 <- glmmTMB(Density_100~Time + (1|Year), 
                     #dispformula = ~ Int,
                     family = Gamma(link=log),
                     REML = TRUE,
                     data=bt.pl)
summary(btp_den.glmm2)

anova(btp_den.glmm1, btp_den.glmm2) # this suggests that model btp_den.glmm2 without dispersion is slightly better than btp_den.glmm1.  Proceed with btp_den.glmm2


### diagnostics ----
btp_den.glmm2_simres <- simulateResiduals(btp_den.glmm2)
# str(btp_den.glmm2_simres,1)
plot(btp_den.glmm2_simres)
# The normality and homogeneity of variance look OK  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btp_den.glmm2_simres_recalc <- recalculateResiduals(btp_den.glmm2_simres, group = bt.pl$Year)
testTemporalAutocorrelation(btp_den.glmm2_simres_recalc, time = unique(bt.pl$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
# btp_den.glmm2_simres_recalcSpace <- recalculateResiduals(btp_den.glmm2_simres, group = as.factor(bt.pl$Station_new))
# unique(bt.pl$Station_new) 
# 
# testSpatialAutocorrelation(btp_den.glmm2_simres_recalcSpace, x = unique(bt.pl$X), y = unique(bt.pl$Y))

# can't really say much with 4 pools but dosn't appear to be clustering

# Proceed with btp_den.glmm2
summary(btp_den.glmm2)
mean_by_site(bt.pl.density.station, "yes", "d")

## create CIs ----
tab.ci(btp_den.glmm2, "bt_pl_den")
tmp <- confint(btp_den.glmm2)
tmp[1:2, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100



### LUNKERS ----
btl_den.glmm1 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.lu
)
summary(btl_den.glmm1)

btl_den.glmm2 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  #  dispformula = ~ Lunker,
  family = Gamma(link = log),
  REML = TRUE,
  data = bt.lu
)
summary(btl_den.glmm2)

anova(btl_den.glmm1, btl_den.glmm2)  # the model with dispersion is slightly better - btl_den.glmm1

### diagnostics ----
btl_den.glmm1_simres <- simulateResiduals(btl_den.glmm1)
plot(btl_den.glmm1_simres)
# The normality isn't great but homogeneity of variance looks great.

### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btl_den.glmm1_simres_recalc <- recalculateResiduals(btl_den.glmm1_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btl_den.glmm1_simres_recalc, time = unique(bt.lu$Year))

# resids look great: conclude no temproal issues


# ### spatial independence
# # recalculate resids with stations as the grouping variable
# btl_den.glmm1_simres_recalcSpace <- recalculateResiduals(btl_den.glmm1_simres, group = as.factor(bt.lu$Station_new))
# testSpatialAutocorrelation(btl_den.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)

# spatial autocorrelation is awful but not sure what to do about this.  Explore
# ggplot(data = bt.lu, aes(x = as.factor(Station_new), y = Density_100)) + 
#   geom_boxplot()
# plot(bt.lu$Station_new, bt.lu$Density_100)
# 
# ggplot(data = bt.lu, aes(x = Year, y = Density_100)) + 
#   geom_boxplot() + 
#   facet_grid(rows = vars(Station_new))
# 
# plot(coords$X, coords$Y, type = 'n', xlim=c(345660.6, 345710.3))  # , xlim=rev(c(345648.6, 345770.3))
# text(coords$X, coords$Y, coords$Station) # X = north-south, Y = west-east

# go with btl_den.glmm1
summary(btl_den.glmm1)
mean_by_site(bt.lu.density.station, "lunker", "d")

## create CIs ----
tab.ci(btl_den.glmm1, "bt_lu_den")
tmp <- confint(btl_den.glmm1)
tmp[1:2, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100
22*1.85




# BTYOY ----
# using all the data
# btyoyp_den.gls3 <- gls(Density_100~Time, data=btyoy.pl)
# btyoyp_den.gls4 <- gls(Density_100~Time, weights = varIdent(form = ~ 1|Int), data=btyoy.pl)
# anova(btyoyp_den.gls3, btyoyp_den.gls4) # suggests virtually no improvement with this variance structure
# plot(btyoyp_den.gls4)        # variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# btyoyp.lme1 <- lme(Density_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=btyoy.pl)
# btyoyp.lme2 <- lme(Density_100~Time, random =  ~ 1|Year, data=btyoy.pl)
# anova(btyoyp.lme1, btyoyp.lme2)
# # the anova suggests that the weights really don't help here 
# summary(btyoyp.lme1)
# plot(btyoyp.lme1)
# # heterogeneity bad 
# 
# # normality sucks
# qqnorm(btyoyp.lme1$resid)
# qqline(btyoyp.lme1$resid)
# 

# ### glmm ----
# #### Gamma is OK here because there are no zeros but it doesn't converge

btyoyp_den.glmm0 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
#  Density_100 ~ Time + numYear + (1 | Year), #numYear and numYear^2 don't work
  dispformula = ~ Time,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.pl
)
summary(btyoyp_den.glmm0)

btyoyp_den.glmm0.5 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.pl
)
summary(btyoyp_den.glmm0.5)
anova(btyoyp_den.glmm0, btyoyp_den.glmm0.5) # problems with homogeneity and temporal autocorrelatoin


# #### Use Tweedie instead
btyoyp_den.glmm1 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
#  Density_100 ~ Time + I(numYear^2) + (1 | Year), # gives NAs
#  Density_100 ~ Time + numYear + (1 | Year), # gives NAs
  dispformula = ~ Time,
  family = tweedie,
  REML = TRUE,
  data = btyoy.pl
)

summary(btyoyp_den.glmm1)
# str(btyoyp_den.glmm1)


## Compre the results of glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.
btyoyp_den.glmm2 <- glmmTMB(
  Density_100~Time + (1|Year),
# Density_100 ~ Time + I(numYear^2) + (1 | Year), # gives NAs
      family = tweedie,
      REML = TRUE,
  data=btyoy.pl)
summary(btyoyp_den.glmm2)
anova(btyoyp_den.glmm1, btyoyp_den.glmm2) # this suggests that model btyoyp_den.glmm1 with dispersion is a bit better than btyoyp_den.glmm2, i.e., without dispersion
## The estimates are virtually identical but the Std. Errors are a fair bit smaller than the model with dispersion and p-value changes.  Proceed with btyoyp_den.glmm1

### diagnostics ----
btyoyp_den.glmm1_simres <- simulateResiduals(btyoyp_den.glmm1)
plot(btyoyp_den.glmm1_simres)
# The normality is pretty bad and homogeneity is not good.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyp_den.glmm1_simres_recalc <- recalculateResiduals(btyoyp_den.glmm1_simres, group = bt.pl$Year)
# plot(bt.glmm1_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(btyoyp_den.glmm1_simres_recalc, time = unique(btyoy.pl$Year))

# temporal pattern and maybe in acf
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# recalculate resids with stations as the grouping variable
# btyoyp_den.glmm1_simres_recalcSpace <- recalculateResiduals(btyoyp_den.glmm1_simres, group = as.factor(bt.pl$Station_new))
# testSpatialAutocorrelation(btyoyp_den.glmm1_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# spatial autocorrelation is awful but not sure how to fix.  Explore

### new model ----
#### https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
#btyoy.pl$numYear <- as.numeric(btyoy.pl$numYear)
# add year covariate
# btyoyp_den.glmm_new <- glmmTMB(
#   #Density_100 ~ Time + as.numeric(Year) + (1 | Year),
#   #Density_100 ~ Time + I(as.numeric(Year)^2) + (1 | Year),
#   # Density_100 ~ Time + I(numYear^2) + (1 | Year), # this does not work but the above does
#   #Density_100 ~ Time + numYear + (1 | Year), # this does not work
#   dispformula = ~ Time,
#   family = tweedie,
#   REML = TRUE,
#   data = btyoy.pl
# )
# 
# 
# summary(btyoyp_den.glmm_new)
# btyoyp_den.glmm_new_simres <- simulateResiduals(btyoyp_den.glmm_new)
# plot(btyoyp_den.glmm_new_simres)
# # The normality is much better but still bad and homogeneity is now good
# 
# ### temporal independence
# btyoyp_den.glmm_new_simres_recalc <- recalculateResiduals(btyoyp_den.glmm_new_simres, group = bt.pl$Year)
# testTemporalAutocorrelation(btyoyp_den.glmm_new_simres_recalc, time = unique(btyoy.pl$Year)) # this helps resids vy time but ACF is now bad for first lag.

### spatial independence
# recalculate resids with stations as the grouping variable
# btyoyp_den.glmm_new_simres_recalcSpace <- recalculateResiduals(btyoyp_den.glmm_new_simres, group = as.factor(bt.pl$Station_new))
# testSpatialAutocorrelation(btyoyp_den.glmm_new_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

#The fix helps resid patterns but makes the ACF bad.  P no where near alpha so ignore problems. Go with btyoyp_den.glmm_new
#summary(btyoyp_den.glmm_new)
summary(btyoyp_den.glmm1)
mean_by_site(btyoy.pl.density.station, "yes", "d")
ggplot(btyoy.pl, aes(x = Year, y = Density_100)) + geom_point()

## create CIs ----
tab.ci(btyoyp_den.glmm1, "btyoy_pl_den")
tmp <- confint(btyoyp_den.glmm1)
tmp[1:2, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100



## LUNKERS ----
btyoyl_den.glmm1 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  dispformula = ~ Lunker,
  # family = Gamma(link = log),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.lu
)
summary(btyoyl_den.glmm1)

btyoyl_den.glmm2 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = btyoy.lu
)
summary(btyoyl_den.glmm2)

anova(btyoyl_den.glmm1, btyoyl_den.glmm2)  # with dispersion is much better -btyoyl_den.glmm1

### diagnostics ----
btyoyl_den.glmm1_simres <- simulateResiduals(btyoyl_den.glmm1)
plot(btyoyl_den.glmm1_simres)
# The normality and homogeneity of variance look good.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoyl_den.glmm1_simres_recalc <- recalculateResiduals(btyoyl_den.glmm1_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btyoyl_den.glmm1_simres_recalc, time = unique(bt.lu$Year))

# major pattern in temporal resids and maybe in acf
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids

plot(btyoy.lu$Year, btyoy.lu$Density_100)


### spatial independence
# recalculate resids with stations as the grouping variable
# btyoyl_den.glmm1_simres_recalcSpace <- recalculateResiduals(btyoyl_den.glmm1_simres, group = as.factor(bt.lu$Station_new))
# testSpatialAutocorrelation(btyoyl_den.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)
# 
# # hard to say with spatial resids

### Explore ----

plot(btyoy.lu$Year, btyoy.lu$Density_100)
p <- ggplot(data = btyoy.lu, aes(x = Year, y = Density_100)) + 
  geom_point() +
  facet_grid(rows= vars(Lunker))
p

# # tried numerous models with various correlations structures (us, cs, ar1) and random effects - found that it was best to use different convergence methods as model often wouldn't converge but it still produced poor temporal resids.  Also tried covariates but to no avail.  However, the parameter estiamte is no where near alpha so accept results and move on.  

btyoyl_den.glmm_new1 <- glmmTMB(
  #Density_100 ~ Lunker + (1 | Year),
  Density_100 ~ Lunker + numYear + (1 | Year),
  #Density_100 ~ Lunker + as.numeric(Year) + (1 | Year),
  #Density_100 ~ Lunker + as.numeric(Year) + ar1(Station_new + 0 | Year),
  dispformula = ~ Lunker,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim, 
    optArgs=list(method = "BFGS")),
  data = btyoy.lu
)

summary(btyoyl_den.glmm_new1)
# str(btyoyp_den.glmm1)


### diagnostics ----
btyoyl_den.glmm_new_simres <- simulateResiduals(btyoyl_den.glmm_new1)
plot(btyoyl_den.glmm_new_simres)
# The normality is pretty good but homogeneity is not good.
testZeroInflation(btyoyl_den.glmm_new_simres)

### temporal independence
btyoyl_den.glmm_new_simres_recalc <- recalculateResiduals(btyoyl_den.glmm_new_simres, group = bt.lu$Year)
testTemporalAutocorrelation(btyoyl_den.glmm_new_simres_recalc, time = unique(btyoy.lu$Year))


### spatial independence
# btyoyl_den.glmm_new_simres_recalcSpace <- recalculateResiduals(btyoyl_den.glmm_new_simres, group = as.factor(bt.lu$Station_new))
# testSpatialAutocorrelation(btyoyl_den.glmm_new_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)
# 
# The new model doesn't look as good for normality/homogeneity but its not awful.  But the temporal pattern in the resids has improved a bit  Spatial, its hard to tell.  Go with btyoyp_den.glmm_new as parameter estimates are no where near alpha
summary(btyoyl_den.glmm_new1)
mean_by_site(btyoy.lu.density.station, "lunker", "d")
# driven by one site, C3
ggplot(btyoy.lu, aes(x = Year, y = Density_100)) + geom_point()


## create CIs ----
tab.ci(btyoyl_den.glmm_new1, "btyoy_lu_den")
tmp <- confint(btyoyl_den.glmm_new1)
tmp[1:2, c(3, 1:2)]

# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100




# AS ----
# using all the data
# asp_den.gls3 <- gls(Density_100~Time, data=as.pl)
# asp_den.gls4 <- gls(Density_100~Time, weights = varIdent(form = ~ 1|Int), data=as.pl)
# anova(asp_den.gls3, asp_den.gls4) # suggests minor improvement with this variance structure
# plot(asp_den.gls3)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# asp.lme1 <- lme(Density_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=as.pl)
# asp.lme2 <- lme(Density_100~Time, random =  ~ 1|Year, data=as.pl)
# anova(asp.lme1, asp.lme2)
# # the anova suggests that the weights help a little
# summary(asp.lme2)
# plot(asp.lme2)
# # heterogeneity pretty bad and variance increases with increased fitted value
# 
# # normality sucks
# qqnorm(asp.lme2$resid)
# qqline(asp.lme2$resid)


### glmm ----
#### lots of zeros so use ziGamma

asp_den.glmm1 <- glmmTMB(Density_100 ~ Time + (1|Year), # adding year doesn't help resids!
                         dispformula = ~ Time,
                         family = ziGamma(link = "log"),
                         ziformula = ~1,
                        #ziformula = ~Station_new,
                            REML = TRUE,
                          # control = glmmTMBControl(
                          #   optimizer = optim, 
                          #   optArgs=list(method = "BFGS")),
                         data=as.pl)
summary(asp_den.glmm1)

plot(as.pl$Year, as.pl$Density_100)
ggplot(data = as.pl, aes(x = Year, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Time))

ggplot(data = as.pl, aes(x = Station_new, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Time))

ggplot(data = as.pl, aes(x = Station_new, y = Density_100)) + 
  geom_boxplot() +
  facet_grid(rows= vars(Habitat))


#coplot(Density_100 ~ Year | Station_new, as.pl)

asp_den.glmm2 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  #  dispformula = ~ Time,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.pl
)
summary(asp_den.glmm2)

# Tweedie about the same as asp_den.glmm2 for AIC but dispformula produces NA and diagnostics for this model are poor.
asp_den.glmm3 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  dispformula = ~ Time, # produces NaN in std errors and 
  family = tweedie,
  REML = TRUE,
   control = glmmTMBControl(
     optimizer = optim, 
     optArgs=list(method = "BFGS")),
  data = as.pl
)
summary(asp_den.glmm3)


anova(asp_den.glmm1, asp_den.glmm2) # this suggests that the model with dispersion is a smidge better - asp_den.glmm1 plus diagnostics for asp_den.glmm2 look awful; asp_den.glmm1 is slightly better than Tweedie without dispersion formula and diagnostics are awful.  Tweedie with dispersion  is slightly better than asp_den.glmm1, diagnostics are fine and it all makes sense ito output.
anova(asp_den.glmm1, asp_den.glmm3)


### diagnostics ----
asp_den.glmm1_simres <- simulateResiduals(asp_den.glmm3)
plot(asp_den.glmm1_simres)
# The normality is fine but homogeneity is not great.
#coplot(Density_100 ~ Year | Station_new, as.pl)

temp <- cbind(as.pl, resids = residuals(asp_den.glmm3))
coplot(resids ~ Year | Station_new ,temp)


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asp_den.glmm1_simres_recalc <- recalculateResiduals(asp_den.glmm1_simres, group = as.pl$Year)
testTemporalAutocorrelation(asp_den.glmm1_simres_recalc, time = unique(as.pl$Year))

# some trend in resids but not awful and acf shows a trend
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# recalculate resids with stations as the grouping variable
# asp_den.glmm1_simres_recalcSpace <- recalculateResiduals(asp_den.glmm1_simres, group = as.factor(bt.pl$Station_new))
# testSpatialAutocorrelation(asp_den.glmm1_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)
# 
# spatial autocorrelation is about as good as it can be.  

## Resids not great but p-value far from alpha.  Proceed with this model -asp_den.glmm1
summary(asp_den.glmm3)
mean_by_site(as.pl.density.station, "yes", z = "d")
#ggplot(as.pl, aes(x = Year, y = Density_100)) + geom_point()

## create CIs ----
# tab.ci(asp_den.glmm3, "as_pl_den") # this is causing the computer to freeze - commenting out - I can get the confint for asp_den.glmm1 & 2 no problem

# Dave and Paul advised using debug to see where the problem was.  It fronze up with a simple rbind on L123 - not sure why but I just extracted the confint within the bebug function and pasted it into the csv file, as_pl_den.csv
# debug(confint) 
# confint(asp_den.glmm3)






### LUNKERS ----
asl_den.glmm1 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
   #Density_100 ~ Lunker + numYear + (1 | Year), # produces terrible homogeneity
  dispformula = ~ Lunker,
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.lu
)
summary(asl_den.glmm1)

asl_den.glmm2 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  family = ziGamma(link = "log"),
  ziformula = ~1,
  REML = TRUE,
  data = as.lu
)
summary(asl_den.glmm2)

anova(asl_den.glmm1, asl_den.glmm2)  # with dispesion is slightly better - asl_den.glmm1

### diagnostics ----
asl_den.glmm1_simres <- simulateResiduals(asl_den.glmm1)
residuals(asl_den.glmm1_simres)
hist(residuals(asl_den.glmm1_simres))
plot(asl_den.glmm1_simres)
plotQQunif(asl_den.glmm1_simres)
plotResiduals(asl_den.glmm1_simres)
# The normality and homogeneity of variance look great.  
testDispersion(asl_den.glmm1)
testZeroInflation(asl_den.glmm1)

### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asl_den.glmm1_simres_recalc <- recalculateResiduals(asl_den.glmm1_simres, group = as.lu$Year)
testTemporalAutocorrelation(asl_den.glmm1_simres_recalc, time = unique(as.lu$Year))

plot(as.lu$Year, as.lu$Density_100)
# some temporal pattern in resids and acf but not bad: conclude no temproal issues
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids
### spatial independence
# recalculate resids with stations as the grouping variable
# asl_den.glmm1_simres_recalcSpace <- recalculateResiduals(asl_den.glmm1_simres, group = as.factor(bt.lu$Station_new))
# testSpatialAutocorrelation(asl_den.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)

summary(asl_den.glmm1) # resids not great but its all driven by C3
mean_by_site(as.lu.density.station, "lunker", "d")

## create CIs ----
tab.ci(asl_den.glmm1, "as_lu_den")
tmp <- confint(asl_den.glmm1)
tmp[1:2, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100




# ASYOY ----
# using all the data
# asyoyp_den.gls3 <- gls(Density_100~Time, data=asyoy.pl)
# asyoyp_den.gls4 <- gls(Density_100~Time, weights = varIdent(form = ~ 1|Int), data=asyoy.pl)
# anova(asyoyp_den.gls3, asyoyp_den.gls4) # suggests no improvement with this variance structure
# plot(asyoyp_den.gls4)        # but variance is still heterogeneous
# 
# ### lmm ----
# # gls2 helps the variance issue a bit - let's try random effect
# asyoyp_den.gme1 <- lme(Density_100~Time, random =  ~ 1|Year, weights = varIdent(form = ~ 1|Int), data=asyoy.pl)
# asyoyp_den.gme2 <- lme(Density_100~Time, random =  ~ 1|Year, data=asyoy.pl)
# anova(asyoyp_den.gme1, asyoyp_den.gme2)
# # the anova suggests that the weights help just a little here 
# summary(asyoyp_den.gme1)
# plot(asyoyp_den.gme1)
# # heterogeneity looks great but variance increases with increased fitted value
# 
# # normality sucks
# qqnorm(asyoyp_den.gme1$resid)
# qqline(asyoyp_den.gme1$resid)


### glmm ----
#### Gamma is OK here because there are no zeros but it doesn't converge
#### Use Tweedie instead

# model doesn't converge
asyoyp_den.glmm1 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  # Density_100 ~ Time + numYear + (1 | Year), does not converge
  dispformula = ~ Time,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim, 
    optArgs=list(method = "BFGS")),
  data = asyoy.pl
)

summary(asyoyp_den.glmm1)


asyoyp_den.glmm2 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = asyoy.pl
)
summary(asyoyp_den.glmm2)

anova(asyoyp_den.glmm1, asyoyp_den.glmm2)


# try tweedie as other doesn't converge
asyoyp_den.glmm3 <- glmmTMB(
  Density_100 ~ Time + (1 | Year),
  family= tweedie,
  REML = TRUE,
  data = asyoy.pl
)
summary(asyoyp_den.glmm3)
anova(asyoyp_den.glmm1, asyoyp_den.glmm3) # Tweedie is slightly better - parameter estimates differ but all significant -asyoyp_den.glmm3


### diagnostics ----
asyoyp_den.glmm3_simres <- simulateResiduals(asyoyp_den.glmm3)
plot(asyoyp_den.glmm3_simres)
# The normality is not great nor is homogeneity but passable.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoyp_den.glmm3_simres_recalc <- recalculateResiduals(asyoyp_den.glmm3_simres, group = as.pl$Year)
testTemporalAutocorrelation(asyoyp_den.glmm3_simres_recalc, time = unique(asyoy.pl$Year))

# temporal pattern in resids and acf
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# recalculate resids with stations as the grouping variable
# asyoyp_den.glmm3_simres_recalcSpace <- recalculateResiduals(asyoyp_den.glmm3_simres, group = as.factor(as.pl$Station_new))
# testSpatialAutocorrelation(asyoyp_den.glmm3_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# spatial autocorrelation is awful but not sure how to fix.  $$$$CONSIDER A NEW MODEL

### Explore ----
#### https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html

asyoyp_den.glmm4 <- glmmTMB(
  #Density_100 ~ Time + I(as.numeric(Year)^2) + (1 | Year),
#  Density_100 ~ Time + I(numYear^2) + (1 | Year), # all NAs
  Density_100 ~ Time + numYear + (1 | Year), # this works - dashboard
#  Density_100 ~ Time + numYear + (1 | Year) + (1 | Station_new), # this works
#  Density_100 ~ Time + I(numYear-1990) + (1 | Year) + (1 | Station_new), # this works
  #Density_100 ~ Time + (1 | Year) + (1|Time/Station_new), # doesn't converge
#  Density_100 ~ Time + numYear^2 + (1 | Year) + (1|Station_new), # doesn't converge
  family= tweedie,
  REML = TRUE,
  data = asyoy.pl
)
summary(asyoyp_den.glmm4)


### diagnostics ----
asyoyp_den.glmm4_simres <- simulateResiduals(asyoyp_den.glmm4)
plot(asyoyp_den.glmm4_simres)
# The normality is great and homogeneity is pretty good.


### temporal independence
asyoyp_den.glmm4_simres_recalc <- recalculateResiduals(asyoyp_den.glmm4_simres, group = as.pl$Year)
testTemporalAutocorrelation(asyoyp_den.glmm4_simres_recalc, time = unique(asyoy.pl$Year))

### spatial independence
# recalculate resids with stations as the grouping variable
# asyoyp_den.glmm3_simres_recalcSpace <- recalculateResiduals(asyoyp_den.glmm3_simres, group = as.factor(as.pl$Station_new))
# testSpatialAutocorrelation(asyoyp_den.glmm3_simres_recalcSpace, x = coords.pl$X, y = coords.pl$Y)

# accept new model asyoyp_den.glmm3 <- glmmTMB(Density_100 ~ Time + as.numeric(Year)^2 + (1 | Year)

summary(asyoyp_den.glmm4)
mean_by_site(asyoy.pl.density.station, "yes", "d")
# this makes some sense now
ggplot(asyoy.pl, aes(x = Year, y = Density_100)) + geom_point()

## create CIs ----
tab.ci(asyoyp_den.glmm4, "asyoy_pl_den")
tmp <- confint(asyoyp_den.glmm4)
tmp[1:2, c(3, 1:2)]
# percent increase - included only because differences appear very large but numYear must be soaking up a lot of variance and CIs bound zero.
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100



### LUNKERS ----
asyoyl_den.glmm1 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
#  Density_100 ~ Lunker + numYear + (1 | Year),
  dispformula = ~ Lunker,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = asyoy.lu
)

summary(asyoyl_den.glmm1)

asyoyl_den.glmm2 <- glmmTMB(
  Density_100 ~ Lunker + (1 | Year),
  #  dispformula = ~ Lunker,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = asyoy.lu
)

summary(asyoyl_den.glmm2)

anova(asyoyl_den.glmm1, asyoyl_den.glmm2)  # with dispesion is better - asyoyl_den.glmm1
ggplot(asyoy.lu, aes(x = Year, y = Density_100)) + geom_point()

### diagnostics ----
asyoyl_den.glmm1_simres <- simulateResiduals(asyoyl_den.glmm1)
plot(asyoyl_den.glmm1_simres)
# The normality and homogeneity of variance look great.  


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoyl_den.glmm1_simres_recalc <- recalculateResiduals(asyoyl_den.glmm1_simres, group = as.lu$Year)
testTemporalAutocorrelation(asyoyl_den.glmm1_simres_recalc, time = unique(as.lu$Year))


# temporal trend in resids and acf but not awful
# see https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#workflow-in-dharma for how to intepret resids


### spatial independence
# recalculate resids with stations as the grouping variable
# asyoyl_den.glmm1_simres_recalcSpace <- recalculateResiduals(asyoyl_den.glmm1_simres, group = as.factor(as.lu$Station_new))
# testSpatialAutocorrelation(asyoyl_den.glmm1_simres_recalcSpace, x = coords.lu$X, y = coords.lu$Y)

# spatial autocorrelation hard to tell.  

# I tried some covariates and covariance structures to no avail.  But p = 0.004 so proceed

summary(asyoyl_den.glmm2)
mean_by_site(asyoy.lu.density.station, "lunker", "d")
# Driven by C3

## create CIs ----
tab.ci(asyoyl_den.glmm2, "asyoy_lu_den")
tmp <- confint(asyoyl_den.glmm2)
tmp[1:2, c(3, 1:2)]

# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100




# fading plots ----
## riffles ----
### see scratch_pad_glm.R for all the rationale and justification for why this works

summary(bt_den.glmm1)

# back transformed
bt.np_fit <- fitted(bt_den.glmm1, se.fit=T)

# log scale
bt.np_pred <- as.data.frame(predict(bt_den.glmm1, se.fit = T))

# random effect
bt.np_rdm <- ranef(bt_den.glmm1)


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
  summarise(mean = mean(Density_100), # this is the mean by year
            fit = mean(fit), # this is teh fixed effect for that year
            se = mean(se.fit) # this is teh SE for that year
  )
# see scratch_pad.glm for notes about calculating standard error

# website on rescaling png: https://climate-cms.org/posts/2019-03-06-generating-print-quality-plots.html
# plot with predicted line, and confidence intervals and predicted value for the Before:impact period

png("output/fade_np_BTdensity.png", pointsize=10, width=2800, height=2000, res=600)

plot(as.numeric(as.character(df2_bt.np$Year)), df2_bt.np$mean, pch = 16, ylim = c(10, 40), xlab = "Year", ylab = "Density Estimate (g/100 sq. m)")
lines(as.numeric(as.character(df2_bt.np$Year)), exp(df2_bt.np$fit), col="black")
# note that i'm not 100% sure if this is calculated correctly as i've just taken the value of se as an average.  There may be some delta method approach involved.
lines(as.numeric(as.character(df2_bt.np$Year)), exp(df2_bt.np$fit+1.96*df2_bt.np$se), col="black", lty=2)
lines(as.numeric(as.character(df2_bt.np$Year)), exp(df2_bt.np$fit-1.96*df2_bt.np$se), col="black", lty=2)

yr_1988 <- df_bt.np$bt.np_fit[1]
yr_1989 <- df_bt.np$bt.np_fit[8]
abline(a=(yr_1988 + yr_1989)/2, b=0,col="blue")
dev.off()




## pools ----
# back transformed
bt.pl_fit <- fitted(btp_den.glmm2, se.fit=T)

# log scale
bt.pl_pred <- as.data.frame(predict(btp_den.glmm2, se.fit = T))

# random effect
bt.pl_rdm <- ranef(btp_den.glmm2)


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
  summarise(mean = mean(Density_100), # this is the mean by year
            fit = mean(fit), # this is teh fixed effect for that year
            se = mean(se.fit) # this is teh SE for that year
  )
# see scratch_pad.glm for notes about calculating standard error

# plot with predicted line, and confidence intervals and predicted value for the Before:impact period


png("output/fade_pool_BTdensity.png", pointsize=10, width=2800, height=2000, res=600)

plot(as.numeric(as.character(df2_bt.pl$Year)), df2_bt.pl$mean, pch = 16, ylim = c(10, 60), xlab = "Year", ylab = "Density Estimate (g/100 sq. m)")
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit), col="black")
# note that i'm not 100% sure if this is calculated correctly as i've just taken the value of se as an average.  There may be some delta method approach involved.
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit+1.96*df2_bt.pl$se), col="black", lty=2)
lines(as.numeric(as.character(df2_bt.pl$Year)), exp(df2_bt.pl$fit-1.96*df2_bt.pl$se), col="black", lty=2)

yr_1988 <- df_bt.pl$bt.pl_fit[1]
yr_1989 <- df_bt.pl$bt.pl_fit[8]
abline(a=(yr_1988 + yr_1989)/2, b=0,col="blue")

dev.off()


# END ----

