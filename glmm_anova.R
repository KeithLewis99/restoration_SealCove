# source("glmm_biomass_models.R")
source("glmm_density_models.R")

library(ggplot2)
library(cowplot)

# bt-biomass ----
## compare models ----
# First make sure that the model -bt.glmm1- is OK without REML
## Note that models will be renamed in sequence xm, e.g., bt.glmm1m to indicate that these are ML, not REML, so bt.glmm1 -> bt.glmm1m
# bt.glmm1m <- glmmTMB(
#   Biomass_100 ~ Time*Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt.glmm1m)
# 
# bt.glmm1m_simres <- simulateResiduals(bt.glmm1m)
# plot(bt.glmm1m_simres)
# 
# bt.glmm1m_simres_recalc <- recalculateResiduals(bt.glmm1m_simres, group = bt.np$Year)
# testTemporalAutocorrelation(bt.glmm1_simres_recalc, time = unique(bt.np$Year))
# 
# bt.glmm1m_simres_recalcSpace <- recalculateResiduals(bt.glmm1m_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(bt.glmm1m_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# 
# # Diagnostics look fine
# # Now for nested models to test model terms.
# bt.glmm4 <- glmmTMB(
#   Biomass_100 ~ Time + Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt.glmm4)
# 
# anova(bt.glmm1m, bt.glmm4, test="F") # OK - so no interaction term
# 
# bt.glmm5 <- glmmTMB(
#   Biomass_100 ~ Time + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt.glmm5)
# 
# anova(bt.glmm4, bt.glmm5, test="F") # suggests treatment effect
# 
# bt.glmm6 <- glmmTMB(
#   Biomass_100 ~ Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# 
# anova(bt.glmm4, bt.glmm6) # but no time effect
# 
# bt.glmm7 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt.glmm6)
# 
# anova(bt.glmm4, bt.glmm7, test="F")
# 
# # compare Time and Treatment to the intercept - not sure what this shows
# anova(bt.glmm5, bt.glmm7, test="F")
# anova(bt.glmm6, bt.glmm7, test="F")
# 
# ## cons v destroy ----
# #View(bt.np)
# 
# bt.glmm8 <- glmmTMB(
#   Biomass_100 ~ Type + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np.imp
# )
# summary(bt.glmm8)
# 
# bt.glmm8_simres <- simulateResiduals(bt.glmm8)
# residuals(bt.glmm8_simres)
# plot(bt.glmm8_simres)
# 
# bt.glmm8_simres_recalc <- recalculateResiduals(bt.glmm8_simres, group = bt.np.imp$Year)
# residuals(bt.glmm8_simres_recalc)
# testTemporalAutocorrelation(bt.glmm8_simres_recalc, time = unique(bt.np.imp$Year))
# 
# bt.glmm8__simres_recalcSpace <- recalculateResiduals(bt.glmm8_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(bt.glmm8__simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# 
# # resids good - proceed
# 
# 
# bt.glmm9 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np.imp
# )
# 
# anova(bt.glmm8, bt.glmm9) # suggests no difference between the destroyed and constructed riffles

## figs ----
## by Riffles

bt.np.biomass.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


# bt.np.biomass.summary
# str(bt.np.biomass.summary, give.attr = F)
bt.np.biomass.summary$Type <- as.factor(bt.np.biomass.summary$Type)
levels(bt.np.biomass.summary$Type)
#levels(bt.np.biomass.summary$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
levels(bt.np.biomass.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")


p1 <- ggplot(bt.np.biomass.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p1


### by Pools ----
bt.pl.mean <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BT") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )
# head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
# bt.pl.mean

# bt.pl.mean[bt.pl.mean$Type == "Downstream",]
# bt.pl.mean[bt.pl.mean$Type == "Compensation",]
# edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "BT",]


bt.pl.mean$Type <- as.factor(bt.pl.mean$Type)
#levels(bt.pl.mean$Type) <- c("Compensation", "Compensation")
levels(bt.pl.mean$Type) <- c("Treatment", "Treatment")
tmp_row <- bt.pl.mean[1,]
tmp_row[1,] <- NA
tmp_row$Year <- as.factor(1990)
#tmp_row$Type <- as.factor("Compensation")
tmp_row$Type <- as.factor("Treatment")

bt.pl.mean <- rbind(bt.pl.mean, tmp_row)

# pools
p2 <- ggplot(bt.pl.mean, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p2
### assemble figures ----
bottom_row <- plot_grid(p2, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p1, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BT_biomass.png"), width=10, height=8, units="in")


# biomass of pools
p3 <- ggplot(data = bt.pl, aes(x = Station_new, y = Biomass_100)) + 
  geom_point()
p3 # clearly, some pools have higher biomass than others - good reason to check with density


## LUNKERS ----
# btl.glmm3 <- glmmTMB(
#   Biomass_100 ~ Lunker + (1 | Year),
#   #  dispformula = ~ Lunker,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.lu
# )
# 
# 
# btl.glmm4 <- glmmTMB(
#   Biomass_100 ~ (1 | Year),
#   #  dispformula = ~ Lunker,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.lu
# )
# anova(btl.glmm3, btl.glmm4)  # suggests biomass much higher in LUNKERS
# 
# 
# 
bt.lu.summ <- bt.lu |>
  select(Year, Species, Lunker, Station_new, Biomass_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )

bt.lu.summ


p4 <- ggplot(bt.lu.summ, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("LUNKER present") +
  
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p4


ggsave("output/BT_LU_biomass.png", width=10, height=8, units="in")


# btyoy-biomass ----
## compare models ----
# First make sure that the model - btyoy.glmm4 - is OK without REML

# btyoy.glmm4m <- glmmTMB(
#   Biomass_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie, # link is log
#   REML = FALSE,
#   data = btyoy.np
# )
# summary(btyoy.glmm4m)
# 
# 
# btyoy.glmm4m_simres <- simulateResiduals(btyoy.glmm4m)
# plot(btyoy.glmm4m_simres) # resids look great!
# 
# btyoy.glmm4m_simres_recalc <- recalculateResiduals(btyoy.glmm4m_simres, group = btyoy.np$Year)
# testTemporalAutocorrelation(btyoy.glmm4m_simres_recalc, time = unique(btyoy.np$Year)) # some evidence of a pattern in last 6 years
# 
# btyoy.glmm4m_simres_recalcSpace <- recalculateResiduals(btyoy.glmm4m_simres, group = as.factor(btyoy.np$Station_new))
# testSpatialAutocorrelation(btyoy.glmm4m_simres_recalcSpace, x = coords.np$X, y = coords.np$Y) # spatial resids are awful
# 
# # Now for nested models to test model terms.
# btyoy.glmm7 <- glmmTMB(
#   Biomass_100 ~ Time + Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np
# )
# summary(btyoy.glmm7)
# 
# anova(btyoy.glmm4m, btyoy.glmm7, test="F") # OK - so no interaction term
# 
# # Treatment
# btyoy.glmm8 <- glmmTMB(
#   Biomass_100 ~ Time + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np
# )
# summary(btyoy.glmm8)
# 
# #anova(btyoy.glmm4m, btyoy.glmm8, test="F") # suggests treatment effect although weak
# anova(btyoy.glmm7, btyoy.glmm8, test="F") # suggests strong treatment effect 
# 
# 
# # Time
# btyoy.glmm9 <- glmmTMB(
#   Biomass_100 ~ Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np
# )
# 
# #anova(btyoy.glmm4m, btyoy.glmm9) # but no time effect
# anova(btyoy.glmm7, btyoy.glmm9) # but no time effect
# 
# # Intercept
# btyoy.glmm10 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np
# )
# summary(btyoy.glmm10)
# 
# anova(btyoy.glmm4m, btyoy.glmm10, test="F")  # may not be informative
# 
# ## cons v destroy ----
# 
# btyoy.glmm11 <- glmmTMB(
#   Biomass_100 ~ Type + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np.imp
# )
# summary(btyoy.glmm11)
# 
# btyoy.glmm11_simres <- simulateResiduals(btyoy.glmm11)
# plot(btyoy.glmm11_simres)
# 
# btyoy.glmm11_simres_recalc <- recalculateResiduals(btyoy.glmm11_simres, group = bt.np.imp$Year)
# testTemporalAutocorrelation(btyoy.glmm11_simres_recalc, time = unique(bt.np.imp$Year))
# 
# btyoy.glmm11_simres_recalcSpace <- recalculateResiduals(btyoy.glmm11_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(btyoy.glmm11_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# # resids great except for spatial resids but p-value is far from alpha (see below) - proceed
# 
# btyoy.glmm12 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = btyoy.np.imp
# )
# 
# anova(btyoy.glmm11, btyoy.glmm12) # suggests no difference

## figs ----
## by Riffles

btyoy.np.biomass.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


btyoy.np.biomass.summary
str(btyoy.np.biomass.summary, give.attr = F)
btyoy.np.biomass.summary$Type <- as.factor(btyoy.np.biomass.summary$Type)
levels(btyoy.np.biomass.summary$Type)
#levels(btyoy.np.biomass.summary$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
#levels(btyoy.np.biomass.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(btyoy.np.biomass.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")


p5 <- ggplot(btyoy.np.biomass.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p5


### by Pools ----
btyoy.pl.mean <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BTYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
btyoy.pl.mean

# btyoy.pl.mean[btyoy.pl.mean$Type == "Downstream",]
# btyoy.pl.mean[btyoy.pl.mean$Type == "Compensation",]
# edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "BTYOY",]


btyoy.pl.mean$Type <- as.factor(btyoy.pl.mean$Type)
#levels(btyoy.pl.mean$Type) <- c("Compensation", "Compensation")
levels(btyoy.pl.mean$Type) <- c("Treatment", "Treatment")

btyoy.pl.mean <- rbind(btyoy.pl.mean, tmp_row)

p6 <- ggplot(btyoy.pl.mean, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p6


# with Lunkers
edited_cs_estimates_by_site$Lunker <- NA
for (i in seq_along(edited_cs_estimates_by_site$Station)){
  if(edited_cs_estimates_by_site$Station[[i]] == "7"|edited_cs_estimates_by_site$Station[[i]] == "5"){
    edited_cs_estimates_by_site$Lunker[[i]] <- "Yes"
  } else {
    edited_cs_estimates_by_site$Lunker[[i]] <- "No"
  }
}

btyoy.pl.meanL <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BTYOY") |>
  group_by(Year, Type, Lunker) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
btyoy.pl.meanL

# btyoy.pl.mean[btyoy.pl.mean$Type == "Downstream",]
# btyoy.pl.mean[btyoy.pl.mean$Type == "Compensation",]
# edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "BTYOY",]


btyoy.pl.meanL$Type <- as.factor(btyoy.pl.meanL$Type)
#levels(btyoy.pl.mean$Type) <- c("Compensation", "Compensation")
levels(btyoy.pl.meanL$Type) <- c("Treatment", "Treatment")

btyoy.pl.meanL <- rbind(btyoy.pl.meanL, tmp_row)

p6a <- ggplot(btyoy.pl.meanL, aes(as.factor(Year), mean, group = Lunker, colour = Lunker)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p6a



### assemble figures ----
bottom_row <- plot_grid(p6, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p5, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BTYOY_biomass.png"), width=10, height=8, units="in")


# biomass of pools
p7 <- ggplot(data = btyoy.pl, aes(x = Station_new, y = Biomass_100)) + 
  geom_point()
p7 # clearly, some pools have higher biomass than others - good reason to check with density

## LUNKERS ----
# btyoyl.glmm3 <- glmmTMB(
#   Biomass_100 ~ Lunker + (1 | Year),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = btyoy.lu
# )
# 
# 
# btyoyl.glmm4 <- glmmTMB(
#   Biomass_100 ~ (1 | Year),
#   #  dispformula = ~ Lunker,
#   #family = Gamma(link = log),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = btyoy.lu
# )
# anova(btyoyl.glmm3, btyoyl.glmm4) # suggests a difference


# LUNKERS
btyoy.lu.summ <- btyoy.lu |>
  select(Year, Species, Lunker, Station_new, Biomass_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )

btyoy.lu.summ
p8 <- ggplot(btyoy.lu.summ, aes(as.factor(Lunker), mean, group = Station_new, label= Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p8
ggsave("output/BTYOY_LU_biomass.png", width=10, height=8, units="in")



# as-biomass ----
## compare models ----
# First make sure that the model - as.glmm3 - is OK without REML
# as.glmm3m <- glmmTMB(
#   Biomass_100 ~ Time * Treatment + (1 | Year),
#   family = tweedie, # link is log
#   REML = FALSE,
#   data = as.np
# )
# summary(as.glmm3m)
# 
# as.glmm3m_simres <- simulateResiduals(as.glmm3m)
# plot(as.glmm3m_simres) # resids look great
# 
# as.glmm3m_simres_recalc <- recalculateResiduals(as.glmm3m_simres, group = as.np$Year)
# testTemporalAutocorrelation(as.glmm3m_simres_recalc, time = unique(as.np$Year))
# # temporal resids look great
# 
# as.glmm3m_simres_recalcSpace <- recalculateResiduals(as.glmm3m_simres, group = as.factor(as.np$Station_new))
# testSpatialAutocorrelation(as.glmm3m_simres_recalcSpace, x = coords.np$X, y = coords.np$Y) # spatial resids look fine
# 
# # Now for nested models to test model terms.
# as.glmm5 <- glmmTMB(
#   Biomass_100 ~ Time + Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np
# )
# summary(as.glmm5)
# 
# anova(as.glmm3m, as.glmm5, test="F") # OK - so no interaction term
# 
# # Treatment
# as.glmm6 <- glmmTMB(
#   Biomass_100 ~ Time + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np
# )
# summary(as.glmm6)
# 
# anova(as.glmm5, as.glmm6, test="F") # suggests treatment effect although weak
# 
# # Time
# as.glmm7 <- glmmTMB(
#   Biomass_100 ~ Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np
# )
# 
# anova(as.glmm5, as.glmm7) # but no time effect
# 
# # Intercept
# as.glmm8 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np
# )
# summary(as.glmm8)
# 
# anova(as.glmm5, as.glmm8, test="F")  # may not be informative
# 
# ## cons v destroy ----
# as.glmm11 <- glmmTMB(
#   Biomass_100 ~ Type + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np.imp
# )
# summary(as.glmm11)
# 
# as.glmm11_simres <- simulateResiduals(as.glmm11)
# plot(as.glmm11_simres)
# 
# as.glmm11_simres_recalc <- recalculateResiduals(as.glmm11_simres, group = bt.np.imp$Year)
# testTemporalAutocorrelation(as.glmm11_simres_recalc, time = unique(bt.np.imp$Year))
# 
# as.glmm11_simres_recalcSpace <- recalculateResiduals(as.glmm11_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(as.glmm11_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# # resids great - proceed
# 
# as.glmm12 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = tweedie,
#   REML = FALSE,
#   data = as.np.imp
# )
# 
# anova(as.glmm11, as.glmm12) # suggests a weak difference

## figs ----
## by Riffles

as.np.biomass.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


as.np.biomass.summary
str(as.np.biomass.summary, give.attr = F)
as.np.biomass.summary$Type <- as.factor(as.np.biomass.summary$Type)
levels(as.np.biomass.summary$Type)
#levels(as.np.biomass.summary$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
#levels(as.np.biomass.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(as.np.biomass.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")

p9 <- ggplot(as.np.biomass.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p9


### by Pools ----
as.pl.mean <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "AS ") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
as.pl.mean

# as.pl.mean[as.pl.mean$Type == "Downstream",]
# as.pl.mean[as.pl.mean$Type == "Compensation",]
# edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "AS",]


as.pl.mean$Type <- as.factor(as.pl.mean$Type)
#levels(as.pl.mean$Type) <- c("Compensation", "Compensation")
levels(as.pl.mean$Type) <- c("Treatment", "Treatment")

as.pl.mean  <- rbind(as.pl.mean, tmp_row)

p10 <- ggplot(as.pl.mean, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p10


### assemble figures ----
bottom_row <- plot_grid(p10, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p9, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/AS_biomass.png"), width=10, height=8, units="in")


#biomass by station
p11 <- ggplot(data = as.pl, aes(x = Station_new, y = Biomass_100)) + 
  geom_point()
p11 # clearly, some pools have higher biomass than others - good reason to check with density

## LUNKERS ----
# asl.glmm3 <- glmmTMB(
#   Biomass_100 ~ Lunker + (1 | Year),
#   #  dispformula = ~ Lunker,
#   # family = Gamma(link = log),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = as.lu
# )
# summary(asl.glmm3)
# 
# asl.glmm4 <- glmmTMB(
#   Biomass_100 ~ (1 | Year),
#   #  dispformula = ~ Lunker,
#   #family = Gamma(link = log),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = as.lu
# )
# anova(asl.glmm3, asl.glmm4) # suggests no difference


# LUNKERS
as.lu.summ <- as.lu |>
  select(Year, Species, Lunker, Station_new, Biomass_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )

as.lu.summ
p12 <- ggplot(as.lu.summ, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  theme_bw() + 
  geom_text() +
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p12

ggsave("output/AS_LU_biomass.png", width=10, height=8, units="in")

# asyoy-biomass ----

## compare models ----
# First make sure that the model is - asyoy.glmm2 - OK without REML
# asyoy.glmm2m <- glmmTMB(Biomass_100 ~ Time*Treatment + (1|Year), 
#                        family = ziGamma(link = "log"),
#                        ziformula = ~1,
#                        REML = FALSE,
#                        data=asyoy.np)
# 
# 
# asyoy.glmm2m_simres <- simulateResiduals(asyoy.glmm2m)
# plot(asyoy.glmm2m_simres) # resids are great
# 
# asyoy.glmm2m_simres_recalc <- recalculateResiduals(asyoy.glmm2m_simres, group = asyoy.np$Year)
# testTemporalAutocorrelation(asyoy.glmm2m_simres_recalc, time = unique(asyoy.np$Year))
# # Resids are great
# 
# asyoy.glmm2m_simres_recalcSpace <- recalculateResiduals(asyoy.glmm2m_simres, group = as.factor(asyoy.np$Station_new))
# testSpatialAutocorrelation(asyoy.glmm2m_simres_recalcSpace, x = coords.np$X, y = coords.np$Y) # spatial resids are OK - proceed
# 
# # Now for nested models to test model terms.
# asyoy.glmm5 <- glmmTMB(
#   Biomass_100 ~ Time + Treatment + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np
# )
# summary(asyoy.glmm5)
# 
# anova(asyoy.glmm2m, asyoy.glmm5, test="F") # no interaction term
# 
# # Treatment
# asyoy.glmm6 <- glmmTMB(
#   Biomass_100 ~ Time + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np
# )
# summary(asyoy.glmm6)
# 
# anova(asyoy.glmm5, asyoy.glmm6, test="F") # suggests treatment effect and large difference
# 
# # Time
# asyoy.glmm7 <- glmmTMB(
#   Biomass_100 ~ Treatment + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np
# )
# 
# anova(asyoy.glmm5, asyoy.glmm7) # but no time effect
# 
# # Intercept
# asyoy.glmm8 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np
# )
# summary(asyoy.glmm8)
# 
# anova(asyoy.glmm5, asyoy.glmm8, test="F")  # may not be informative
# 
# ## cons v destroy ----
# asyoy.glmm11 <- glmmTMB(
#   Biomass_100 ~ Type + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np.imp
# )
# summary(asyoy.glmm11)
# 
# 
# asyoy.glmm11_simres <- simulateResiduals(asyoy.glmm11)
# plot(asyoy.glmm11_simres)
# 
# asyoy.glmm11_simres_recalc <- recalculateResiduals(asyoy.glmm11_simres, group = bt.np.imp$Year)
# testTemporalAutocorrelation(asyoy.glmm11_simres_recalc, time = unique(bt.np.imp$Year))
# 
# asyoy.glmm11_simres_recalcSpace <- recalculateResiduals(asyoy.glmm11_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(asyoy.glmm11_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# # resids great - proceed
# 
# asyoy.glmm12 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   family = ziGamma(link = "log"),
#   ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.np.imp
# )
# 
# anova(asyoy.glmm11, asyoy.glmm12) # suggests no difference

## figs ----
## by Riffles


asyoy.np.biomass.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


asyoy.np.biomass.summary
str(asyoy.np.biomass.summary, give.attr = F)
asyoy.np.biomass.summary$Type <- as.factor(asyoy.np.biomass.summary$Type)
levels(asyoy.np.biomass.summary$Type)
#levels(asyoy.np.biomass.summary$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
#levels(asyoy.np.biomass.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(asyoy.np.biomass.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")


p13 <- ggplot(asyoy.np.biomass.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p13


### by Pools ----
asyoy.pl.mean <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "ASYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
asyoy.pl.mean

# asyoy.pl.mean[asyoy.pl.mean$Type == "Downstream",]
# asyoy.pl.mean[asyoy.pl.mean$Type == "Compensation",]
# edited_cs_estimates_by_site[edited_cs_estimates_by_site$Type == "Downstream" & edited_cs_estimates_by_site$Species == "ASYOY",]


asyoy.pl.mean$Type <- as.factor(asyoy.pl.mean$Type)
#levels(asyoy.pl.mean$Type) <- c("Compensation", "Compensation")
levels(asyoy.pl.mean$Type) <- c("Treatment", "Treatment")

asyoy.pl.mean  <- rbind(asyoy.pl.mean, tmp_row)

p14 <- ggplot(asyoy.pl.mean, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p14

### assemble figures ----
bottom_row <- plot_grid(p14, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)
#plot_grid(p1, bottom_row, nrow = 2)

plot_grid(plot_grid(p13, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean biomass estimate (g/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Biomass (g/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/ASYOY_biomass.png"), width=10, height=8, units="in")



# just check the stations
# tmp2 <- edited_cs_estimates_by_site |>
#   filter(Pool == "Yes" & Species == "BT")

p15 <- ggplot(data = asyoy.pl, aes(x = Station_new, y = Biomass_100)) + 
  geom_point()
p15 # clearly, some pools have higher biomass than others - good reason to check with density

## LUNKERS ----
# asl.glmm3 <- glmmTMB(
#   Biomass_100 ~ Lunker + (1 | Year),
#   #dispformula = ~ Lunker,
#   # family = Gamma(link = log),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.lu
# )
# summary(asl.glmm3)
# 
# 
# asl.glmm4 <- glmmTMB(
#   Biomass_100 ~ (1 | Year),
#   #  dispformula = ~ Lunker,
#   #family = Gamma(link = log),
#   family=ziGamma(link="log"), ziformula = ~1,
#   REML = FALSE,
#   data = asyoy.lu
# )
# anova(asl.glmm3, asl.glmm4) # suggests no difference


# LUNKERS
asyoy.lu.summ <- asyoy.lu |>
  select(Year, Species, Lunker, Station_new, Biomass_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N)
  )

asyoy.lu.summ
p16 <- ggplot(asyoy.lu.summ, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p16
ggsave("output/ASYOY_LU_biomass.png", width=10, height=8, units="in")

# Lunkers - all ----
plot_grid(p4, p12, p8, p16, nrow = 2, ncol = 2)




# DENSITY ----
# bt-density ----
### by Riffles

## compare models ----
# First make sure that the model -bt.glmm1- is OK without REML
## Note that models will be renamed in sequence xm, e.g., bt.glmm1m to indicate that these are ML, not REML, so bt.glmm1 -> bt.glmm1m
# bt_den.glmm1m <- glmmTMB(
#   Density_100 ~ Time * Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt_den.glmm1m)
# 
# bt_den.glmm1m_simres <- simulateResiduals(bt_den.glmm1m)
# plot(bt_den.glmm1m_simres)
# 
# bt_den.glmm1m_simres_recalc <- recalculateResiduals(bt_den.glmm1m_simres, group = bt.np$Year)
# testTemporalAutocorrelation(bt_den.glmm1m_simres_recalc, time = unique(bt.np$Year))
# 
# bt_den.glmm1m_simres_recalcSpace <- recalculateResiduals(bt_den.glmm1m_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(bt_den.glmm1m_simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# 
# # Diagnostics look OK
# # Now for nested models to test model terms.
# bt_den.glmm4 <- glmmTMB(
#   Density_100 ~ Time + Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt_den.glmm4)
# 
# anova(bt_den.glmm1m, bt_den.glmm4, test="F") # OK - so no interaction term
# 
# bt_den.glmm5 <- glmmTMB(
#   Density_100 ~ Time + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt_den.glmm5)
# 
# anova(bt_den.glmm1m, bt_den.glmm5, test="F") # suggests treatment effect
# 
# bt_den.glmm6 <- glmmTMB(
#   Density_100 ~ Treatment + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt_den.glmm6)
# 
# anova(bt_den.glmm1m, bt_den.glmm6) # but no time effect
# 
# bt_den.glmm7 <- glmmTMB(
#   Density_100 ~  (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np
# )
# summary(bt.glmm7)
# 
# anova(bt_den.glmm1m, bt_den.glmm7, test="F")
# 
# # compare Time and Treatment to the intercept - not sure what this shows
# anova(bt.glmm5, bt.glmm7, test="F")
# anova(bt.glmm6, bt.glmm7, test="F")
# 
# ## cons v destroy ----
# bt_den.glmm8 <- glmmTMB(
#   Density_100 ~ Type + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np.imp
# )
# summary(bt_den.glmm8)
# 
# bt_den.glmm8_simres <- simulateResiduals(bt_den.glmm8)
# residuals(bt_den.glmm8_simres)
# plot(bt_den.glmm8_simres)
# 
# bt_den.glmm8_simres_recalc <- recalculateResiduals(bt_den.glmm8_simres, group = bt.np.imp$Year)
# residuals(bt_den.glmm8_simres_recalc)
# testTemporalAutocorrelation(bt_den.glmm8_simres_recalc, time = unique(bt.np.imp$Year))
# 
# bt_den.glmm8__simres_recalcSpace <- recalculateResiduals(bt_den.glmm8_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(bt_den.glmm8__simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# 
# # resids good - proceed
# 
# bt_den.glmm9 <- glmmTMB(
#   Biomass_100 ~ 1 + (1 | Year),
#   dispformula = ~ Int,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.np.imp
# )
# 
# anova(bt_den.glmm8, bt_den.glmm9) # suggests large difference between the destroyed and constructed riffles

## figs ----
## by Riffles

bt.np.density.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


bt.np.density.summary$Type <- as.factor(bt.np.density.summary$Type)
# levels(bt.np.density.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(bt.np.density.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")

p20 <- ggplot(bt.np.density.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p20



### by Pools ----
bt.pl.mean.den <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BT") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
bt.pl.mean.den


bt.pl.mean.den$Type <- as.factor(bt.pl.mean.den$Type)
levels(bt.pl.mean.den$Type) <- c("Treatment", "Treatment")
tmp_row <- bt.pl.mean.den[1,]
tmp_row[1,] <- NA
tmp_row$Year <- as.factor(1990)
tmp_row$Type <- as.factor("Treatment")

bt.pl.mean.den <- rbind(bt.pl.mean.den, tmp_row)

p21 <- ggplot(bt.pl.mean.den, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)

## assemble figures ----
bottom_row <- plot_grid(p21, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)

plot_grid(plot_grid(p20, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean Density Estimate (#/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (#/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BT_density.png"), width=10, height=8, units="in")



# just check the stations
p22 <- ggplot(data = bt.pl, aes(x = Station_new, y = Density_100)) + 
  geom_point()
p22 # clearly, some pools have higher density than others - good reason to check with density


## LUNKERS ----

# btl_den.glmm3 <- glmmTMB(
#   Biomass_100 ~ Lunker + (1 | Year),
#   #  dispformula = ~ Lunker,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.lu
# )
# 
# btl_den.glmm3_simres <- simulateResiduals(btl_den.glmm3)
# plot(btl_den.glmm3_simres)
# 
# btl_den.glmm3_simres_recalc <- recalculateResiduals(btl_den.glmm3_simres, group = bt.np.imp$Year)
# testTemporalAutocorrelation(btl_den.glmm3_simres_recalc, time = unique(bt.lu$Year))
# 
# btl_den.glmm3__simres_recalcSpace <- recalculateResiduals(btl_den.glmm3_simres, group = as.factor(bt.np$Station_new))
# testSpatialAutocorrelation(btl_den.glmm3__simres_recalcSpace, x = coords.np$X, y = coords.np$Y)
# 
# # resids good - proceed
# 
# btl.glmm4 <- glmmTMB(
#   Biomass_100 ~ (1 | Year),
#   #  dispformula = ~ Lunker,
#   family = Gamma(link = log),
#   REML = FALSE,
#   data = bt.lu
# )
# anova(btl.glmm3, btl.glmm4)  # suggests biomass much higher in LUNKERS

bt.lu.summ.den <- bt.lu |>
  select(Year, Species, Lunker, Station_new, Density_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )

bt.lu.summ.den
p23 <- ggplot(bt.lu.summ.den, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p23
ggsave(paste0("output/BT_LU_density.png"), width=10, height=8, units="in")




# btyoy-density ----
## figs ----
## by Riffles

btyoy.np.density.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


btyoy.np.density.summary$Type <- as.factor(btyoy.np.density.summary$Type)
#levels(btyoy.np.density.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(btyoy.np.density.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")

p24 <- ggplot(btyoy.np.density.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p24



### by Pools ----
btyoy.pl.mean.den <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BTYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
btyoy.pl.mean.den


btyoy.pl.mean.den$Type <- as.factor(btyoy.pl.mean.den$Type)
levels(btyoy.pl.mean.den$Type) <- c("Treatment", "Treatment")
tmp_row <- btyoy.pl.mean.den[1,]
tmp_row[1,] <- NA
tmp_row$Year <- as.factor(1990)
tmp_row$Type <- as.factor("Treatment")

btyoy.pl.mean.den <- rbind(btyoy.pl.mean.den, tmp_row)

p25 <- ggplot(btyoy.pl.mean.den, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)

## assemble figures ----
bottom_row <- plot_grid(p25, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)

plot_grid(plot_grid(p24, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean Density Estimate (#/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (#/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/BTYOY_density.png"), width=10, height=8, units="in")



# just check the stations
p26 <- ggplot(data = btyoy.pl, aes(x = Station_new, y = Density_100)) + 
  geom_point()
p26 # clearly, some pools have higher density than others - good reason to check with density


## LUNKERS ----

btyoy.lu.summ.den <- btyoy.lu |>
  select(Year, Species, Lunker, Station_new, Density_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )

bt.lu.summ.den

p27 <- ggplot(btyoy.lu.summ.den, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p27

ggsave("output/BTYOY_LU_density.png", width=10, height=8, units="in")


# as-density ----
## figs ----
## by Riffles

as.np.density.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


as.np.density.summary$Type <- as.factor(as.np.density.summary$Type)
#levels(as.np.density.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(as.np.density.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")

p28 <- ggplot(as.np.density.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p28



### by Pools ----
as.pl.mean.den <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "AS ") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
as.pl.mean.den


as.pl.mean.den$Type <- as.factor(as.pl.mean.den$Type)
levels(as.pl.mean.den$Type) <- c("Treatment", "Treatment")
tmp_row <- as.pl.mean.den[1,]
tmp_row[1,] <- NA
tmp_row$Year <- as.factor(1990)
tmp_row$Type <- as.factor("Treatment")

as.pl.mean.den <- rbind(as.pl.mean.den, tmp_row)

p29 <- ggplot(as.pl.mean.den, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)

## assemble figures ----
bottom_row <- plot_grid(p29, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)

plot_grid(plot_grid(p28, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean Density Estimate (#/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (#/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave(paste0("output/AS_density.png"), width=10, height=8, units="in")



# just check the stations
p30 <- ggplot(data = as.pl, aes(x = Station_new, y = Density_100)) + 
  geom_point()
p30 # clearly, some pools have higher density than others - good reason to check with density


## LUNKERS ----

as.lu.summ.den <- as.lu |>
  select(Year, Species, Lunker, Station_new, Density_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )

bt.lu.summ.den

p31 <- ggplot(as.lu.summ.den, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p31

ggsave("output/AS_LU_density.png", width=10, height=8, units="in")


# asyoy-density ----
## figs ----
## by Riffles

asyoy.np.density.summary <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


asyoy.np.density.summary$Type <- as.factor(asyoy.np.density.summary$Type)
#levels(asyoy.np.density.summary$Type) <- c("Treatment", "Upstream", "Treatment", "Downstream")
levels(asyoy.np.density.summary$Type) <- c("Treatment", "Control-Upstream", "Treatment", "Control-Downstream")

p32 <- ggplot(asyoy.np.density.summary, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)
p32



### by Pools ----
asyoy.pl.mean.den <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "ASYOY") |>
  group_by(Year, Type) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )
head(edited_cs_estimates_by_site[, c(1:4, 6:7, 10:13)])
asyoy.pl.mean.den


asyoy.pl.mean.den$Type <- as.factor(asyoy.pl.mean.den$Type)
levels(asyoy.pl.mean.den$Type) <- c("Treatment", "Treatment")
tmp_row <- asyoy.pl.mean.den[1,]
tmp_row[1,] <- NA
tmp_row$Year <- as.factor(1990)
tmp_row$Type <- as.factor("Treatment")

asyoy.pl.mean.den <- rbind(asyoy.pl.mean.den, tmp_row)

p33 <- ggplot(asyoy.pl.mean.den, aes(as.factor(Year), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  facet_grid(~Type) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
  #  ylab("Mean Density Estimate (#/100 sq. m)") +
  ylab("") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  geom_vline(xintercept = 3, linetype = 3)

## assemble figures ----
bottom_row <- plot_grid(p33, NULL, NULL, rel_widths = c(1.15, 1, 1),ncol = 3)

plot_grid(plot_grid(p32, bottom_row, labels=c("", ""), nrow = 2), scale=0.9) +
  #draw_label("Mean Density Estimate (#/100 sq. m)", x=  0, y=0.5, vjust= 1.5, angle=90) +
  draw_label("Density (#/100 sq. m)", x=  0.03, y=0.5, vjust= 1.5, angle=90)
ggsave("output/ASYOY_density.png", width=10, height=8, units="in")



# just check the stations
p34 <- ggplot(data = asyoy.pl, aes(x = Station_new, y = Density_100)) + 
  geom_point()
p34 # clearly, some pools have higher density than others - good reason to check with density


## LUNKERS ----

asyoy.lu.summ.den <- asyoy.lu |>
  select(Year, Species, Lunker, Station_new, Density_100) |>
  group_by(Species, Lunker, Station_new) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
  )

bt.lu.summ.den

p35 <- ggplot(asyoy.lu.summ.den, aes(as.factor(Lunker), mean, group = Station_new, label = Station_new)) + 
  geom_point(size=4, position=position_dodge(width = 0.5)) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x  = element_text(vjust=0.4, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = Station_new), width=0.1, position=position_dodge(width = 0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("LUNKER present") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
p35

ggsave("output/ASYOY_LU_density.png", width=10, height=8, units="in")



# Lunkers - all ----
plot_grid(p23, p31, p27, p35, nrow = 2, ncol = 2)

# END ----