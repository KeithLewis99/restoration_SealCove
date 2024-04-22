# functions
spatialAutoCorrBase_fun <- function(x, y, xtextRes = 8, xtextSite = -10){
  col = colorRamp(c("red", "white", "blue"))(y$scaledResiduals)
  plot(x$X, x$Y, type = 'n', xlim=c(345448.6, 345970.3))
  points(x = unique(x$X), y = unique(x$Y), col = rgb(col, maxColorValue = 255))
  text(unique(x$X)+xtextRes, unique(x$Y), labels = unique(x$Station_new), cex = 0.5)
  text(unique(x$X)+xtextSite, unique(x$Y), labels = y$scaledResiduals, cex = 0.5)
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

spatialAutoCorrGG_fun <- function(x, xtextRes = -8, xtextSite = 10) {
  ggplot(x, aes(x = X, y = Y, size = mean)) +
    geom_point() +
    xlim(345498.6, 345920.3) +
    geom_text(aes(label = Station_new), check_overlap = T, nudge_x = xtextSite, size = 3) +
    geom_text(aes(label = scResid), nudge_x = xtextRes, size = 3)
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
  p1 <- ggplot(x, aes(as.factor(Station_new), mean)) +
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
  return(p1)
}


fig.data <- function(df, pl, sp, var){
  #browser()
  if(pl == "No"){
    tmp <- df |>
      filter(Pool == pl & Species == sp) |>
      group_by(Year, Type) |>
      summarise(N  = length({{var}}),
                mean = mean({{var}}),
                sd   = sd({{var}}),
                se   = sd / sqrt(N))
    tmp$Type <- as.factor(tmp$Type)
    levels(tmp$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
  } else if (pl == "Yes") {
    tmp <- df |>
      filter(Pool == pl & Species == sp) |>
      group_by(Year, Type) |>
      droplevels() |>
      summarise(N  = length({{var}}),
                mean = mean({{var}}),
                sd   = sd({{var}}),
                se   = sd / sqrt(N)
    )
  
  # creates proper levels 
  tmp$Type <- as.factor(tmp$Type)
  tmp <- droplevels(tmp)
  levels(tmp$Type) <- c("Compensation", "Compensation")
  # creates NA's for 1990
  tmp_row <- tmp[1,]
  tmp_row[1,] <- NA
  tmp_row$Year <- 1990
  tmp_row$Type <- "Compensation"
  tmp_row$Type <- as.factor(tmp_row$Type)
  tmp_row$Year <- as.factor(tmp_row$Year)
  tmp <- rbind(tmp, tmp_row)
  }
return(tmp)
}




fig.np <- function(df){
  p1 <- ggplot(df, aes(as.factor(Year), mean)) + 
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
  return(p1)
}

# p2 <- ggplot(tmp1, aes(as.factor(Year), mean)) + 
#   geom_point(size=4, position=position_dodge(1)) +
#   theme_bw() + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
#   facet_grid(~Type) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
#   #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
#   ylab("") +
#   xlab("Year") +
#   theme(panel.grid.minor=element_blank(),
#         panel.grid.major=element_blank()) +
#   geom_vline(xintercept = 3, linetype = 3)
