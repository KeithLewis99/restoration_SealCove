
######One way ANOVA with pairwise comparisons


###First remove destroyed data to compare only comp, control, and downstream

scr_treat<-biomass_scr_graph[-c(1,2),]

#####Test adult brook trout biomass for normality

shapiro.test(scr_treat$adut_bt)

###Data not normally distributed so log transform

shapiro.test(log(scr_treat$adut_bt))

Shapiro-Wilk normality test

data:  log(scr_treat$adut_bt)
W = 0.97247, p-value = 0.728

####Remove 1988, 1989, and 1999 from control so all sites have same number of years.

test_scr<-scr_treat[-c(8,9,10),]


####log the biomass and add it as a column to dataset

log_adultbtbio<-log(test_scr$adut_bt)

test_scr<-cbind(test_scr,log_adultbtbio)

####Run one-way anova on log-transformed data


> results=aov(log_adultbtbio~type, data=test_scr)
> summary(results)
Df Sum Sq Mean Sq F value   Pr(>F)    
type         2  4.252  2.1258   14.21 0.000198 ***
  Residuals   18  2.693  0.1496                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#####The biomass means for the threee site types (compensation, control, downstream) differ.
####Pairwise t-tests needed to explore differences

pairwise.t.test(test_scr$log_adultbtbio, test_scr$type, p.adjust="none")

Pairwise comparisons using t tests with pooled SD 

data:  test_scr$log_adultbtbio and test_scr$type 

Compensation Control
Control    0.00019      -      
  Downstream 0.00025      0.90358

P value adjustment method: none 

#####There is no difference in adult bt biomass between the control and downstream sites but they are both significantly
###different from the compensation sites.

pairwise.t.test(test_scr$log_adultbtbio, test_scr$type, p.adjust="bonferroni")

Pairwise comparisons using t tests with pooled SD 

data:  test_scr$log_adultbtbio and test_scr$type 

Compensation Control
Control    0.00056      -      
  Downstream 0.00074      1.00000

P value adjustment method: bonferroni 


#####Boxplots

boxplot(test_scr$log_adultbtbio~test_scr$type)

####YOY Brook Trout Anova



log_yoybtbio<-log(test_scr$yoy_bt)

test_scr<-cbind(test_scr,log_yoybtbio)

btyoyresults=aov(log_yoybtbio~type, data=test_scr)

summary(btyoyresults)

Df Sum Sq Mean Sq F value   Pr(>F)    
type         2 16.438   8.219   27.61 3.28e-06 ***
  Residuals   18  5.358   0.298                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

pairwise.t.test(test_scr$log_yoybtbio, test_scr$type, p.adjust="bonferroni")

Pairwise comparisons using t tests with pooled SD 

data:  test_scr$log_yoybtbio and test_scr$type 

         Compensation Control
Control    0.00038      -      
Downstream 0.07637    2.6e-06

P value adjustment method: bonferroni 
> boxplot(test_scr$log_yoybtbio~test_scr$type)
> 
##The control site biomass of young of year is significantly less than the biomass in the compensation and downstream sites.
####The downstream sites have higher biomass than the compensation sites but this is not signficantly different.
  
####Adult salmon  

log_adultasbio<-log(test_scr$adult_as)

shapiro.test(log_adultasbio)

test_scr<-cbind(test_scr,log_adultasbio)

adultasresults=aov(log_adultasbio~type, data=test_scr)

summary(adultasresults)

             Df Sum Sq Mean Sq   F value Pr(>F)  
type         2  2.540  1.2702    4.46    0.0267 *
  Residuals 18  5.126  0.2848                 
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


pairwise.t.test(test_scr$log_adultasbio, test_scr$type, p.adjust="bonferroni")  

Pairwise comparisons using t tests with pooled SD 

data:  test_scr$log_adultasbio and test_scr$type 

Compensation Control
Control    1.000        -      
  Downstream 0.028        0.172  

P value adjustment method: bonferroni 


boxplot(test_scr$log_adultasbio~test_scr$type)

####Biomass in the downstream site is significantly less than in the compensation site.   Compensation and control have similar biomass.
