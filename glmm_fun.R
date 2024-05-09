# functions


#' spatialAutoCorrBase_fun
#'
#' @param x - Seal Cove data set
#' @param y - residuals calculated in DHARMa for a glmmTMB object
#' @param xtextRes - a graphical term to move the residual value away from the point
#' @param xtextSite - a graphical term to move the residual value away from the point
#'
#' @return a base plot of lat v long for Seal Cove sites.  alpha numerics to the right are the Seal Cove station and to the left are the scaled residuals (see below)
#' Scaled residuals  
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# To interpret the residuals, remember that a scaled residual value of 0.5 means that half of the simulated data are higher than the observed value (blue), and half of them lower (red).
#' @export
#'
#' @examples spatialAutoCorrBase_fun(bt.np, bt.glmm1_simres_recalcSpace)
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

#' spatialData_join
#'
#' @param x data set with N, mean, sd, and se for Seal Cove sites
#' @param y - residuals calculated in DHARMa for a glmmTMB object
#' @param z - the coordinates (lat/long) for the Seal Cove sites
#'
#' @return a dataframe with the above dataframes properly bound together
#' @export
#'
#' @examples bt.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], bt.glmm1_simres_recalcSpace, coords.np)
#' 
spatialData_join <- function(x, y, z){
  #browser()
  #z <- as.data.frame(matrix(NA, nrow(x), 2)) %>% rename(Station = V1, scResid = temp1)
  temp1 <- y$scaledResiduals # get resids
  group1 <- unique(y$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
  temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station_new = V1, scResid = temp1)
  z1 <- left_join(temp2, x, by = "Station_new")
  z2 <- left_join(z1, z, by = c("Station_new" = "Station"))
}





#' spatialAutoCorrGG_fun
#'
#' @param x - a dataframe produced by spatialData_join
#' @param xtextRes - a graphical term to move the residual value away from the point
#' @param xtextSite - a graphical term to move the residual value away from the point
#'
#' @return a ggplot of lat v long for Seal Cove sites.  alpha numerics to the right are the Seal Cove station and to the left are the scaled residuals (see spatialAutoCorr_fun above for an explanation).  
#' @export
#'
#' @examples spatialAutoCorrGG_fun(bt.np.biomass.all)
#' 
spatialAutoCorrGG_fun <- function(x, xtextRes = -8, xtextSite = 10) {
  ggplot(x, aes(x = X, y = Y, size = mean)) +
    geom_point() +
    xlim(345498.6, 345920.3) +
    geom_text(aes(label = Station_new), check_overlap = T, nudge_x = xtextSite, size = 3) +
    geom_text(aes(label = scResid), nudge_x = xtextRes, size = 3)
}


# x = data, y = non-pool, pool, lunker, z = biomass v density
#' Title
#'
#' @param x 
#' @param y filter variable: pools or non-pools - pool == "yes", non-pool == "no, LUNKER == "lunker"
#' @param z filter variable: density or biomass, density == "d", biomass == "b"
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations by Control and Impact for non-pools, Before-After for Pools, and Lunker/no Lunker for the pools on Impact sites.
#' @export
#'
#' @examples mean_by_site(bt.np.biomass.station, "no", "b")
mean_by_site <- function(x, y, z){
  #browser()
  if (y == "no") {
  ggplot(x, aes(as.factor(Station_new), mean)) +
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
     facet_grid(~factor(Treatment, levels = c("Control", "Impact"))) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)")
    } else if (z == "d"){
      ylab("Density Estimate (#/100 sq. m)")
      }
    } +
    xlab("Station") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
} else if (y == "yes") {
  ggplot(x, aes(as.factor(Station_new), mean)) +
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    #facet_grid(forcats::fct_rev(Time)) +
    #facet_grid(~Time) +
    facet_grid(~factor(Time, levels = c("Before", "After"))) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)")
    } else if (z == "d"){
      ylab("Density Estimate (#/100 sq. m)")
    }
    } +
    xlab("Station") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  } else if (y == "lunker") {
  ggplot(x, aes(as.factor(Station_new), mean)) +
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~Lunker) +
      #facet_grid(forcats::fct_rev(Treatment) ~ forcats::fct_rev(Time)) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
      {if (z == "b"){
        ylab("Mean Biomass Estimate (g/100 sq. m)")
      } else if (z == "d"){
        ylab("Density Estimate (# fish/100 sq. m)")
      }
      } +
    xlab("Station") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  }
}


#' baci.plot
#' for non-pools only
#' @param x - dataframe with the mean and standard deviations and errors for biomass or density by Treatment (Control:Impact) and Time (Before:After)
#' @param z filter variable: response variable - density or biomass, density == "d", biomass == "b"
#' @return a 
#' @export 
#'
#' @examples baci.plot(bt.np.biomass.baci, "b")
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
      ylab("Density Estimate (#/100 sq. m)")
    }
    } +
  xlab("Station") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())
  return(p1)
}


#' fig.data
#'
#' @param df original dataframe from K. Loughlin with all associated variables for Stations by year, species, treatment, type, and response variables (Density_100, Biomass_100)
#' @param pl filter variable: pools or non-pools - pool == "Yes", non-pool == "No"
#' @param sp filter variable: species BT = brook trout, AS = atlantic salmon, YOY = young of year
#' @param var filter variable: response variable -  density("Biomass_100") or biomass ("Biomass_100")
#'
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations.  Figures are facted by  by Treatment(Control:Impact) and Time(Before:After)
#' @export
#'
#' @examples bt.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BT", Biomass_100)

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




#' fig.np
#'
#' @param df dataframe with mean, sd, and se by year and Type (Control, Compensation, Downstream).  Note that Downstream is another set of Controls added later
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations.  Figures are facted by  by Treatment(Control:Impact) and Time(Before:After)
#' @export
#'
#' @examples p1 <- fig.np(bt.bio.np.summ)
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
