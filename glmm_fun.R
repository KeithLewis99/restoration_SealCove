# functions
spatialAutoCorrBase_fun <- function(x, y){
  col = colorRamp(c("red", "white", "blue"))(y$scaledResiduals)
  plot(x$X, x$Y, type = 'n', xlim=c(345448.6, 345970.3))
  points(x = unique(x$X), y = unique(x$Y), col = rgb(col, maxColorValue = 255))
  text(unique(x$X)+8, unique(x$Y), labels = unique(x$Station_new), cex = 0.5)
  text(unique(x$X)-10, unique(x$Y), labels = y$scaledResiduals, cex = 0.5)
}

# temp1 <- btyoy.glmm4_new_simres_recalcSpace$scaledResiduals # get resids
# group1 <- unique(btyoy.glmm4_new_simres_recalcSpace$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
# temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station = V1, scResid = temp1)

spatialData_join <- function(x, y, z){
  #browser()
  #z <- as.data.frame(matrix(NA, nrow(x), 2)) %>% rename(Station = V1, scResid = temp1)
  temp1 <- y$scaledResiduals # get resids
  group1 <- unique(y$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
  temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station_new = V1, scResid = temp1)
  z1 <- left_join(temp2, x, by = "Station_new")
  z2 <- left_join(z1, z, by = c("Station_new" = "Station"))
}

spatialAutoCorrGG_fun <- function(x) {
  ggplot(x, aes(x = X, y = Y, size = mean)) +
    geom_point() + 
    xlim(345498.6, 345920.3) +  
    geom_text(aes(label = Station_new), check_overlap = T, nudge_x = 8, size = 3) +
    geom_text(aes(label = scResid), nudge_x = -10, size = 3)
}


# x = data, y = non-pool, pool, lunker, z = biomass v density
mean_by_site <- function(x, y, z){
  #browser()
  if (y == "no") {
  ggplot(x, aes(as.factor(Station_new), mean)) + 
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~Treatment) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)") 
    } else if (z == "d"){
      ylab("Density Estimate (g/100 sq. m)")
      }
    } +
    xlab("Year") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
} else if (y == "yes") {
  ggplot(x, aes(as.factor(Station_new), mean)) + 
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~Time) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)") 
    } else if (z == "d"){
      ylab("Density Estimate (g/100 sq. m)")
    }
    } +
    xlab("Year") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  } else if (y == "lunker") {
  ggplot(x, aes(as.factor(Station_new), mean)) + 
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~Lunker) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
      {if (z == "b"){
        ylab("Mean Biomass Estimate (g/100 sq. m)") 
      } else if (z == "d"){
        ylab("Density Estimate (# fish/100 sq. m)")
      }
      } +
    xlab("Year") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  }
}


baci.plot <- function(x, z){
  ggplot(x, aes(as.factor(Station_new), mean)) + 
  geom_point(size=4, position=position_dodge(1)) +
  theme_bw() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
  #facet_grid(Treatment ~ Time) +
    facet_grid(forcats::fct_rev(Treatment) ~ forcats::fct_rev(Time)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)") 
    } else if (z == "d"){
      ylab("Density Estimate (g/100 sq. m)")
    }
    } +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
}
