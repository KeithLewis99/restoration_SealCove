test1<-glm(Biomass_round~Type*Pool, family=Gamma(link=identity), data=BT2016)
summary(test1)

###Output
# glm(formula = Biomass_round ~ Type * Pool, family = Gamma(link = identity), 
 #     data = BT2016)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.2215  -0.3691  -0.1411   0.2989   0.7814  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)              537.0      149.2   3.599  0.00575 **
#  TypeDestroyed           -171.0      219.3  -0.780  0.45561   
#PoolYes                 1101.5      530.4   2.077  0.06760 . 
#TypeDestroyed:PoolYes  -1130.5      573.6  -1.971  0.08024 . 
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for Gamma family taken to be 0.3859242)

#Null deviance: 9.8922  on 12  degrees of freedom
#Residual deviance: 4.4501  on  9  degrees of freedom
#AIC: 198.21

#Number of Fisher Scoring iterations: 3



plot(test1)
residtest1<-resid(test1)

test2<-glm(Biomass_round~Type+Pool, family=Gamma(link=identity), data=BT2016)
summary(test1)

####Do two models differ
anova(test1, test2, test="Chisq")
  Analysis of Deviance Table
  
  Model 1: Biomass_round ~ Type * Pool
  Model 2: Biomass_round ~ Type + Pool
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
  1         9     4.4501                       
  2        10     6.4133 -1  -1.9633   0.0241 *
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  
  
  