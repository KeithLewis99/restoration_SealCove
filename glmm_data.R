# library ----
#library(plyr)
library(dplyr)
library(readr)
library(sf)


# import data ----

BT_estimates_by_site <- read_csv("CS_Estimates/BT_estimates_by_site.csv", col_types = cols(Density_round = col_double() ))
head(BT_estimates_by_site)
str(BT_estimates_by_site)

edited_cs_estimates_by_site <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/edited_cs_estimates_by_site.csv")
str(edited_cs_estimates_by_site)
unique(edited_cs_estimates_by_site$Station)


# manipulate data ----
## to make the Stations consistent create a new variable "Station_new" and fill to change site names.  The below information is from Keith Clarke
edited_cs_estimates_by_site$Station_new <- rep(NA, length(edited_cs_estimates_by_site$Station))

for (i in 1:length(edited_cs_estimates_by_site$Station)){
  #browser()
  if(edited_cs_estimates_by_site$Station[i] == "C3") {
    edited_cs_estimates_by_site$Station_new[i] <- "C10Upper"
  } else if (edited_cs_estimates_by_site$Station[i] == "C4"){
    edited_cs_estimates_by_site$Station_new[i] <- "C11"
  } else if (edited_cs_estimates_by_site$Station[i] == "C5") {
    edited_cs_estimates_by_site$Station_new[i] <- "C12"
  } else {
    edited_cs_estimates_by_site$Station_new[i] <- edited_cs_estimates_by_site$Station[i]
  }
}


edited_cs_estimates_by_site[, c(1:4, 17)]
unique(edited_cs_estimates_by_site$Station_new)
length(unique(edited_cs_estimates_by_site$Station_new))

edited_cs_estimates_by_site$Time <- as.factor(edited_cs_estimates_by_site$Time)
edited_cs_estimates_by_site$Treatment <- as.factor(edited_cs_estimates_by_site$Treatment)
edited_cs_estimates_by_site$Station_new <- as.factor(edited_cs_estimates_by_site$Station_new)
edited_cs_estimates_by_site$Int <- interaction(edited_cs_estimates_by_site$Treatment, edited_cs_estimates_by_site$Time)
edited_cs_estimates_by_site$numYear <- as.numeric(edited_cs_estimates_by_site$Year)
edited_cs_estimates_by_site$Year <- as.factor(edited_cs_estimates_by_site$Year)
edited_cs_estimates_by_site$numYear_sq <- (edited_cs_estimates_by_site$numYear)^2

# subset data ----
# Cote used the means by year: this is so we can use all the data

# species check
species_check <- function(df, species){
  for(i in 1:length(df$Species)){
    if(df$Species[i] != species)
      print("SPECIES ERROR")
    else if(df$Species[i] == species){
      print("SPECIES OK")    
    }
  }
}


## non-pools ----
# BT data
bt.np <- subset(edited_cs_estimates_by_site, Species == "BT" & Pool == "No" & Station != "45")
species_check(bt.np, "BT") # species check
# bt.np[, c(2, 4, 6, 7, 10:13, 17)]

# BTYOY data
btyoy.np <- subset(edited_cs_estimates_by_site, Species == "BTYOY" & Pool == "No" & Station != "45")
# str(btyoy.np)
species_check(btyoy.np, "BTYOY") # species check

btyoy.np.001 <- btyoy.np
btyoy.np.001$Biomass_100 <- ifelse(btyoy.np$Biomass_100 == 0, 0.01, btyoy.np$Biomass_100) 
btyoy.np.001$Density_100 <- ifelse(btyoy.np$Density_100 == 0, 0.01, btyoy.np$Density_100)


# AS data
as.np <- subset(edited_cs_estimates_by_site, Species == "AS " & Pool == "No" & Station != "45") #
#str(as.np)
#View(as.np)
species_check(as.np, "AS ") # species check

# ASYOY data
asyoy.np <- subset(edited_cs_estimates_by_site, Species == "ASYOY" & Pool == "No" & Station != "45")
#str(asyoy.np)
species_check(asyoy.np, "ASYOY")  # species check
#View(asyoy.np)


## pools ----
### create data sets for all species
bt.pl <- subset(edited_cs_estimates_by_site, Species == "BT" & Pool == "Yes")
str(bt.pl)
bt.pl[, c(2, 4, 6, 7, 10:13, 17)]
btyoy.pl <- subset(edited_cs_estimates_by_site, Species == "BTYOY" & Pool == "Yes")
str(btyoy.pl)
btyoy.pl[, c(2, 4, 6, 7, 10:13, 17)]
as.pl <- subset(edited_cs_estimates_by_site, Species == "AS " & Pool == "Yes")
str(as.pl)
as.pl[, c(1, 2, 4, 6, 7, 10:13, 17)]
asyoy.pl <- subset(edited_cs_estimates_by_site, Species == "ASYOY" & Pool == "Yes")
str(asyoy.pl)
asyoy.pl[, c(2, 4, 6, 7, 10:13, 17)]


# Cote data ----
# the below if largely for David Cote's approach
## density ----
######Creating dataset of mean abundance for each year

# Speciesdensitybyhabitat <- ddply(edited_cs_estimates_by_site, c("Year", "Species", "Pool", "Time", "Treatment"), summarise,
#                                  N  = length(Density_100),
#                                  mean = mean(Density_100),
#                                  sd   = sd(Density_100),
#                                  se   = sd / sqrt(N)
# )
# 
# str(Speciesdensitybyhabitat)

Speciesdensitybyhabitat <- edited_cs_estimates_by_site |>
  select(Year, Species, Pool, Time, Treatment, Density_100) |>
  group_by(Year, Species, Pool, Time, Treatment) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N)
)
str(Speciesdensitybyhabitat)
Speciesdensitybyhabitat$mean


# BTdensitybyhabitat <- ddply(BT_estimates_by_site, c("Year", "Pool", "Time", "Treatment"), summarise,
#                             N  = length(Density_round),
#                             mean = mean(Density_round),
#                             sd   = sd(Density_round),
#                             se   = sd / sqrt(N)
# )
# 

BTdensitybyhabitat <- BT_estimates_by_site |>
  select(Year, Pool, Time, Treatment, Density_round) |>
  group_by(Year, Pool, Time, Treatment) |>
  summarise(N  = length(Density_round),
            mean = mean(Density_round),
            sd   = sd(Density_round),
            se   = sd / sqrt(N)
)

BTdensitybyhabitat

## biomass ----
####Creating dataset of mean biomass for each year (grams only)

# Speciesbio100byhabitat <- ddply(edited_cs_estimates_by_site, c("Year", "Species", "Pool", "Time", "Treatment"), summarise,
#                                 N  = length(Biomass_100),
#                                 mean = mean(Biomass_100),
#                                 sd   = sd(Biomass_100),
#                                 se   = sd / sqrt(N)
# )



Speciesbio100byhabitat <- edited_cs_estimates_by_site |>
  select(Year, Species, Pool, Time, Treatment, Biomass_100) |>
  group_by(Year, Species, Pool, Time, Treatment) |>
  summarise(N  = length(Biomass_100),
             mean = mean(Biomass_100),
             sd   = sd(Biomass_100),
             se   = sd / sqrt(N)
)

Speciesbio100byhabitat

# BTbyhabitat <- ddply(BT_estimates_by_site, c("Year", "Pool", "Time", "Treatment"), summarise,
#                      N  = length(Biomass_round),
#                      mean = mean(Biomass_round),
#                      sd   = sd(Biomass_round),
#                      se   = sd / sqrt(N)
# )


BTbyhabitat <- BT_estimates_by_site |>
  select(Year, Species, Pool, Time, Treatment, Biomass_round) |>
  group_by(Year, Species, Pool, Time, Treatment) |>
  summarise(N  = length(Biomass_round),
             mean = mean(Biomass_round),
             sd   = sd(Biomass_round),
             se   = sd / sqrt(N)
)



BTbyhabitat$YOS<-BTbyhabitat$Year-1990

# pool
## BT Biomass ----
BTbyhabitat.pool <- BTbyhabitat[BTbyhabitat$Pool == "Yes", ]
BTbyhabitat.pool$Time <- as.factor(BTbyhabitat.pool$Time)
BTbyhabitat.pool$Treatment <- as.factor(BTbyhabitat.pool$Treatment)
BTbyhabitat.not.pool <- BTbyhabitat[BTbyhabitat$Pool == "No", ]
BTbyhabitat.not.pool$Time <- as.factor(BTbyhabitat.not.pool$Time)
BTbyhabitat.not.pool$Treatment <-
  as.factor(BTbyhabitat.not.pool$Treatment)
BTbyhabitat.not.pool$Int <- interaction(BTbyhabitat.not.pool$Treatment, BTbyhabitat.not.pool$Time)

## BT Desnity ----
BTdensitybyhabitat.pool<-BTdensitybyhabitat[BTdensitybyhabitat$Pool=="Yes",]
BTdensitybyhabitat.pool$Time<-as.factor(BTdensitybyhabitat.pool$Time)
BTdensitybyhabitat.pool$Treatment<-as.factor(BTdensitybyhabitat.pool$Treatment)
BTdensitybyhabitat.not.pool<-BTdensitybyhabitat[BTdensitybyhabitat$Pool=="No",]
BTdensitybyhabitat.not.pool$Time<-as.factor(BTdensitybyhabitat.not.pool$Time)
BTdensitybyhabitat.not.pool$Treatment<-as.factor(BTdensitybyhabitat.not.pool$Treatment)


## BTYOY BIOMASS ----
BTYOYbio100byhabitat<-subset(Speciesbio100byhabitat, Speciesbio100byhabitat$Species=="BTYOY")


##so this means we might have to break the analysis up into 2 parts - say one for each habitat
BTYOYbio100byhabitat.pool<-BTYOYbio100byhabitat[BTYOYbio100byhabitat$Pool=="Yes",]
BTYOYbio100byhabitat.pool$Time<-as.factor(BTYOYbio100byhabitat.pool$Time)
BTYOYbio100byhabitat.pool$Treatment<-as.factor(BTYOYbio100byhabitat.pool$Treatment)
BTYOYbio100byhabitat.not.pool<-BTYOYbio100byhabitat[BTYOYbio100byhabitat$Pool=="No",]
BTYOYbio100byhabitat.not.pool$Time<-as.factor(BTYOYbio100byhabitat.not.pool$Time)
BTYOYbio100byhabitat.not.pool$Treatment<-as.factor(BTYOYbio100byhabitat.not.pool$Treatment)


BTYOYdensitybyhabitat<-subset(Speciesdensitybyhabitat, Speciesdensitybyhabitat$Species=="BTYOY")


##so this means we might have to break the analysis up into 2 parts - say one for each habitat
BTYOYdensitybyhabitat.pool<-BTYOYdensitybyhabitat[BTYOYdensitybyhabitat$Pool=="Yes",]
BTYOYdensitybyhabitat.pool$Time<-as.factor(BTYOYdensitybyhabitat.pool$Time)
BTYOYdensitybyhabitat.pool$Treatment<-as.factor(BTYOYdensitybyhabitat.pool$Treatment)
BTYOYdensitybyhabitat.not.pool<-BTYOYdensitybyhabitat[BTYOYdensitybyhabitat$Pool=="No",]
BTYOYdensitybyhabitat.not.pool$Time<-as.factor(BTYOYdensitybyhabitat.not.pool$Time)
BTYOYdensitybyhabitat.not.pool$Treatment<-as.factor(BTYOYdensitybyhabitat.not.pool$Treatment)


##AS BIOMASS ----

ASbio100byhabitat<-subset(Speciesbio100byhabitat, Speciesbio100byhabitat$Species=="AS ")


##so this means we might have to break the analysis up into 2 parts - say one for each habitat
ASbio100byhabitat.pool<-ASbio100byhabitat[ASbio100byhabitat$Pool=="Yes",]
ASbio100byhabitat.pool$Time<-as.factor(ASbio100byhabitat.pool$Time)
ASbio100byhabitat.pool$Treatment<-as.factor(ASbio100byhabitat.pool$Treatment)
ASbio100byhabitat.not.pool<-ASbio100byhabitat[ASbio100byhabitat$Pool=="No",]
ASbio100byhabitat.not.pool$Time<-as.factor(ASbio100byhabitat.not.pool$Time)
ASbio100byhabitat.not.pool$Treatment<-as.factor(ASbio100byhabitat.not.pool$Treatment)


############AS Density

ASdensitybyhabitat <- subset(Speciesdensitybyhabitat, Speciesdensitybyhabitat$Species=="AS ")


##so this means we might have to break the analysis up into 2 parts - say one for each habitat
ASdensitybyhabitat.pool<-ASdensitybyhabitat[ASdensitybyhabitat$Pool=="Yes",]
ASdensitybyhabitat.pool$Time<-as.factor(ASdensitybyhabitat.pool$Time)
ASdensitybyhabitat.pool$Treatment<-as.factor(ASdensitybyhabitat.pool$Treatment)
ASdensitybyhabitat.not.pool<-ASdensitybyhabitat[ASdensitybyhabitat$Pool=="No",]
ASdensitybyhabitat.not.pool$Time<-as.factor(ASdensitybyhabitat.not.pool$Time)
ASdensitybyhabitat.not.pool$Treatment<-as.factor(ASdensitybyhabitat.not.pool$Treatment)




## ASYOY Biomass ----
ASYOYbio100byhabitat<-subset(Speciesbio100byhabitat, Speciesbio100byhabitat$Species=="ASYOY")

##so this means we might have to break the analysis up into 2 parts - say one for each habitat
ASYOYbio100byhabitat.pool <- ASYOYbio100byhabitat[ASYOYbio100byhabitat$Pool=="Yes",]
ASYOYbio100byhabitat.pool$Time <- as.factor(ASYOYbio100byhabitat.pool$Time)
ASYOYbio100byhabitat.pool$Treatment <- as.factor(ASYOYbio100byhabitat.pool$Treatment)
ASYOYbio100byhabitat.not.pool <- ASYOYbio100byhabitat[ASYOYbio100byhabitat$Pool=="No",]
ASYOYbio100byhabitat.not.pool$Time <- as.factor(ASYOYbio100byhabitat.not.pool$Time)
ASYOYbio100byhabitat.not.pool$Treatment <- as.factor(ASYOYbio100byhabitat.not.pool$Treatment)

############ASYOY Density

ASYOYdensitybyhabitat<-subset(Speciesdensitybyhabitat, Speciesdensitybyhabitat$Species=="ASYOY")


##so this means we might have to break the analysis up into 2 parts - say one for each habitat
ASYOYdensitybyhabitat.pool<-ASYOYdensitybyhabitat[ASYOYdensitybyhabitat$Pool=="Yes",]
ASYOYdensitybyhabitat.pool$Time<-as.factor(ASYOYdensitybyhabitat.pool$Time)
ASYOYdensitybyhabitat.pool$Treatment<-as.factor(ASYOYdensitybyhabitat.pool$Treatment)
ASYOYdensitybyhabitat.not.pool<-ASYOYdensitybyhabitat[ASYOYdensitybyhabitat$Pool=="No",]
ASYOYdensitybyhabitat.not.pool$Time<-as.factor(ASYOYdensitybyhabitat.not.pool$Time)
ASYOYdensitybyhabitat.not.pool$Treatment<-as.factor(ASYOYdensitybyhabitat.not.pool$Treatment)

# cons v destroyed ----
# constructed to destroyed - weak test because of so few destroyed pools.
bt.np.imp <- subset(bt.np, Treatment == "Impact")
btyoy.np.imp <- subset(btyoy.np, Treatment == "Impact")
as.np.imp <- subset(as.np, Treatment == "Impact")
asyoy.np.imp <- subset(asyoy.np, Treatment == "Impact")



# LUNKERS ----

bt.lu <- bt.pl |>
  filter(Station_new != "D1") |>
  mutate(Lunker = recode(Station_new, 
                         "3" = "No", 
                         "5" = "Yes", 
                         "7" = "Yes", 
                         "9" = "No"))

bt.lu$Lunker <- as.factor(bt.lu$Lunker)
str(bt.lu)


btyoy.lu <- btyoy.pl |>
  filter(Station_new != "D1") |>
  mutate(Lunker = recode(Station_new, 
                         "3" = "No", 
                         "5" = "Yes", 
                         "7" = "Yes", 
                         "9" = "No"))

btyoy.lu$Lunker <- as.factor(btyoy.lu$Lunker)
str(btyoy.lu)


as.lu <- as.pl |>
  filter(Station_new != "D1") |>
  mutate(Lunker = recode(Station_new, 
                         "3" = "No", 
                         "5" = "Yes", 
                         "7" = "Yes", 
                         "9" = "No"))

as.lu$Lunker <- as.factor(as.lu$Lunker)
str(as.lu)

asyoy.lu <- asyoy.pl |>
  filter(Station_new != "D1") |>
  mutate(Lunker = recode(Station_new, 
                         "3" = "No", 
                         "5" = "Yes", 
                         "7" = "Yes", 
                         "9" = "No"))

asyoy.lu$Lunker <- as.factor(asyoy.lu$Lunker)
# str(asyoy.lu)


# UTMs ----

## Need UTMs of the sites.  I have generated lat/longs in decimal degrees for the west side of the Trans Canada Highway in Google Earth and with GPS.  Given the very small spatial extent of the sites and the inherent error in GPS and inaccuracy in estimating sites in Google Earth with the canopy cover, I believe that the relative differences are fine.  
#### This video to get lat long from Google Earth to csv https://www.youtube.com/watch?v=lOG1iJ4C5Ac

# now to transform the decimal degrees into UTM
## note that coordinatesA has station 45 which only exists in 1993 and seems to be a combination of 4 and 5. Up until now, I have been treating it as 4.  I think its best to delete it as we really don't know what it is and it seems to have multiple areas.  
coord_raw <- as.data.frame(read_csv("Raw data/coordinatesB.csv"))
str(coord_raw)

# the below should work with pipes but doesn't seem to. So rather than trying to figure it out, just do this.
testsf = st_as_sf(coord_raw, coords = c("Longitude", "Latitude")) 
# convert oject to sf object
testsf <- st_set_crs(testsf, "+proj=longlat +datum=WGS84") # retrieve coordinate system
coord_utm <- st_transform(testsf,"+proj=utm +zone=22 +ellps=GRS80 +datum=NAD83") # transform coordinates into UTM
coord_utm
str(coord_utm)
# extract coordinates and make dataframe
tmp <- st_coordinates(coord_utm)
tmp1 <- st_drop_geometry(coord_utm)
coords <- as.data.frame(cbind(tmp1, tmp))

# replot the coordinates but with the eastings and northings on a similar scale
plot(coords$X, coords$Y, type = 'n', xlim=c(345348.6, 346070.3))  # , xlim=rev(c(345648.6, 345770.3))
text(coords$X, coords$Y, coords$Station) # X = north-south, Y = west-east

## coords - non-pools ----
# join coordinates with data set - may not be required
bt.np <- left_join(bt.np, coords, by = c("Station_new" = "Station"))
btyoy.np <- left_join(btyoy.np, coords, by = c("Station_new" = "Station"))
as.np <- left_join(as.np, coords, by = c("Station_new" = "Station"))
asyoy.np <- left_join(asyoy.np, coords, by = c("Station_new" = "Station"))

#str(bt.np)
#bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]

# just coords for non-pool
coords.np <- as.data.frame(coords[c(1:4, 7:8, 10, 12:13, 14:15, 17) ,]) 
nrow(coords.np)

## coords pools ----
bt.pl <- left_join(bt.pl, coords, by = c("Station_new" = "Station"))
btyoy.pl <- left_join(btyoy.pl, coords, by = c("Station_new" = "Station"))
as.pl <- left_join(as.pl, coords, by = c("Station_new" = "Station"))
asyoy.pl <- left_join(asyoy.pl, coords, by = c("Station_new" = "Station"))

#bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]
# just coords for pool
coords.pl <- as.data.frame(coords[c(5:6, 9, 11, 16) ,]) 
nrow(coords.pl)

## coords lunkers
bt.lu <- left_join(bt.lu, coords, by = c("Station_new" = "Station"))
btyoy.lu <- left_join(btyoy.lu, coords, by = c("Station_new" = "Station"))
as.lu <- left_join(as.lu, coords, by = c("Station_new" = "Station"))
asyoy.lu <- left_join(asyoy.lu, coords, by = c("Station_new" = "Station"))


# just coords for pool with LUNKERS from After
coords.lu <- as.data.frame(coords[c(5:6, 9, 11) ,]) 
nrow(coords.lu)


# biomass by station ----
## BT
bt.np.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

bt.np.biomass.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

bt.pl.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BT") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

bt.lu.biomass.station <- bt.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))
## BTYOY
btyoy.np.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

btyoy.np.biomass.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

btyoy.pl.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BTYOY") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

btyoy.lu.biomass.station <- btyoy.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

## AS
as.np.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

as.np.biomass.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

as.pl.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "AS ") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

as.lu.biomass.station <- as.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


## ASYOY
asyoy.np.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

asyoy.np.biomass.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

asyoy.pl.biomass.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "ASYOY") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))

asyoy.lu.biomass.station <- asyoy.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Biomass_100),
            mean = mean(Biomass_100),
            sd   = sd(Biomass_100),
            se   = sd / sqrt(N))


# density by station ----
## BT
bt.np.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

bt.np.density.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BT") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

bt.pl.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BT") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

bt.lu.density.station <- bt.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))
## BTYOY
btyoy.np.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

btyoy.np.density.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "BTYOY") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

btyoy.pl.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "BTYOY") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

btyoy.lu.density.station <- btyoy.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

## AS
as.np.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

as.np.density.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "AS ") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

as.pl.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "AS ") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

as.lu.density.station <- as.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


## ASYOY
asyoy.np.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Station_new, Treatment) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))


asyoy.np.density.baci <- edited_cs_estimates_by_site |>
  filter(Pool == "No" & Species == "ASYOY") |>
  group_by(Station_new, Treatment, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

asyoy.pl.density.station <- edited_cs_estimates_by_site |>
  filter(Pool == "Yes" & Species == "ASYOY") |>
  group_by(Station_new, Time) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))

asyoy.lu.density.station <- asyoy.lu |>
  group_by(Station_new, Lunker) |>
  summarise(N  = length(Density_100),
            mean = mean(Density_100),
            sd   = sd(Density_100),
            se   = sd / sqrt(N))



# END ----

