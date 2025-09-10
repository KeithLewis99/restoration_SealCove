# The purpose of this file is simply to produce bootstrap estimates of the Seal Cove data so that it is compatible with the other values from the Synthesis paper.

# Unfortunately, I could not just bring in the data that I used in density_glmm_models.R and biomass_glmm_models.R because these estimates were derived using Cote's code and i've abandoned this.  Therefore, I replicated the code I used in the other synthesis projects before calculating the bootstrap intervals.  

# source("glmm_data.R")
# source("glmm_fun.R")

# libraries ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)


# import ----
# import files in catch and convert to proper format
#create a pattern and bind directory to pattern
temp = list.files(path = "Raw data/data_byYear/", pattern="Sealcove.*bysite.csv$", full.names = T, ignore.case = TRUE)

# import files as a list
ls_sc = (lapply(temp, read.csv))
str(ls_sc)
str(ls_sc,1)
str(ls_sc[1])

# standardize names
names(ls_sc) <- c("1990", "1991", "1992", "1993", "1994", "1999", "2007", "2015", "2016")

# clean data -----
# create either a large dataframe and then do some summaries
# as above, create summaries for FSA
df_all <- bind_rows(ls_sc)
unique(df_all$Year) # all good
unique(df_all$Species) # spaces in Species
unique(df_all$Station) # a huge mess


## remove "space" from df_all$Species
for(i in seq_along(df_all$Species)){
  df_all$Species[i] <- gsub("*\\s", paste0("\\1"), df_all$Species[i])
}
unique(df_all$Species) # all good

## clean Stn ----
## to make the Stations consistent create a new variable "Station_new" and fill to change site names.  The below information is from Keith Clarke
df_all$Station_new <- rep(NA, length(df_all$Station))

tmp <- df_all |> 
  group_by(Year, Station) |> 
  summarise(count = n()) |>
  pivot_wider(names_from = Year, values_from = count) |> 
  print(n = Inf)
# write.csv(tmp, "output/stations.csv")


# for 2 ----
for (i in 1:length(df_all$Station)){
  #browser()
  if (df_all$Station[i] == "C3" |
      df_all$Station[i] == "T10Upper" | 
      df_all$Station[i] == "T10upper" |
      df_all$Station[i] == "T10U" |
      df_all$Station[i] == "10U" |
      df_all$Station[i] == "10U ") {
    df_all$Station_new[i] <- "C10U"
  }  else if (df_all$Station[i] == "T10lower" |
              df_all$Station[i] == "T10L" |
              df_all$Station[i] == "10L") {
    df_all$Station_new[i] <- "C10L"
  } else if(df_all$Station[i] == "C4" | 
            df_all$Station[i] == "T11" | 
            df_all$Station[i] == "11"){
    df_all$Station_new[i] <- "C11"
  } else if (df_all$Station[i] == "C5" | 
             df_all$Station[i] == "T12" |
             df_all$Station[i] == "12") {
    df_all$Station_new[i] <- "C12"
  } else if (df_all$Station[i] == "D15"){
      df_all$Station_new[i] <- "C15"
  } else if (df_all$Station[i] == "D16"){
    df_all$Station_new[i] <- "C16"
  } else if (df_all$Station[i] == "3"){
    df_all$Station_new[i] <- "T3"
  } else if (df_all$Station[i] == "4"){
    df_all$Station_new[i] <- "T4"
  } else if (df_all$Station[i] == "5"){
    df_all$Station_new[i] <- "T5"
  } else if (df_all$Station[i] == "6"){
    df_all$Station_new[i] <- "T6"
  } else if (df_all$Station[i] == "7"){
    df_all$Station_new[i] <- "T7"
  } else if (df_all$Station[i] == "8"){
    df_all$Station_new[i] <- "T8"
  } else if (df_all$Station[i] == "9"){
    df_all$Station_new[i] <- "T9"
  } else if (df_all$Station[i] == "1" & df_all$Year[i] == 1988 |
             df_all$Station[i] == "T1" & df_all$Year[i] == 1989){
    df_all$Station_new[i] <- "D1"
  } else if (df_all$Station[i] == "2" & df_all$Year[i] == 1988 |
             df_all$Station[i] == "T2" & df_all$Year[i] == 1989){
    df_all$Station_new[i] <- "D2"
  } else if (df_all$Station[i] == "1" & df_all$Year[i] >= 2015) {
    df_all$Station_new[i] <- "T1"
  } else if (df_all$Station[i] == "2" & df_all$Year[i] >= 2015){
    df_all$Station_new[i] <- "T2"
  } else if (df_all$Station[i] == "T10 Lower") {  df_all$Station_new[i] <- "C10L"
  } else if (df_all$Station[i] == "T10 Upper") {  df_all$Station_new[i] <- "C10U"
  } else {
    df_all$Station_new[i] <- df_all$Station[i]
  }
}

unique(df_all$Station_new)

tmp1 <- df_all |> 
  group_by(Year, Station, Station_new) |> 
  summarise(count = n()) |>
  pivot_wider(names_from = Year, values_from = count) |> 
  print(n = Inf)
# write.csv(tmp1, "output/stations1.csv")

# sum catch ----
## first, create a table for abun = T (total catch) and biomass
df_sum <- df_all |>
  group_by(Year, Species, Station_new, Sweep) |>
  filter(!(Year == 1993 & (Station_new == "T4" | Station_new == "T5"))) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 
str(df_sum, give.attr = F)
# write.csv(df_sum, "output/df_sum.csv")


# grid ----
# create dataset with all possible combinations of the following variables
year <- as.character(unique(df_sum$Year))
station <- unique(df_sum$Station_new)
species <- c("AS", "ASYOY", "BT", "BTYOY")
sweep <- c(1:max(df_sum$Sweep))

# make grid
df_grid <- expand.grid(Year = year, 
                       Species = species,
                       Station_new = station,
                       Sweep = sweep) |> 
  arrange(Year, Species, Station_new, Sweep)
#str(df_grid)

# write.csv(df_grid, "output/df_grid.csv")

# edit grid ----

# get max by Year and Station
df_sweep <- df_sum |> 
  group_by(Year, Station_new) |>
  summarise(max_sweep = max(Sweep)) |> 
  pivot_wider(id_cols = Station_new,
              names_from = Year,
              values_from = max_sweep)

# edit grid
# remove the structural zeros, i.e., sites that weren't fished
df_grid$Year <- as.integer(as.character(df_grid$Year))
df_grid$Sweep <- as.integer(df_grid$Sweep)

df_grid1 <- df_grid |>
  filter(!((Year > 1989) & 
             (Station_new == "D1" | Station_new == "D2")) &
           !((Year <= 1990) &
           (Station_new == "C15" | Station_new == "C16" |
              Station_new == "T1" | Station_new == "T2" |
              Station_new == "T3" | Station_new == "T4" |
              Station_new == "T5" | Station_new == "T6" |
              Station_new == "T7" | Station_new == "T8" |
              Station_new == "T9")) &
           !(Year <= 1991 & Station_new == "C10L") &
           !((Year == 1993) & (Station_new == "T4" | Station_new == "T5"))
  ) # all of the above weren't sampled with the exception of T4/T5 which were combined in 1993 for reasons unknown

# str(df_grid1, give.attr = F)
# write.csv(df_grid1, "output/df_grid1.csv")


# join grid and summary ----
# now, join the two dataframes - sites with no fish caught are NA
df_all1 <- full_join(df_grid1, df_sum, by = c("Year", "Species", "Station_new", "Sweep")) |>
  arrange(Year, Species, Station_new, Sweep)
#str(df_all1, give.attr = F)
nrow(df_all1 |> filter(Sweep > 3)) # 2080 rows with Sweep > 3 + 540 = 900
nrow(df_all1 |> filter(Sweep <= 3)) # 1560 rows with Sweep < 3
# write.csv(df_all1, "output/df_all1.csv")


# get max Sweep ----
# this is for below where I remove the extra sweeps
df_stn_tag <- df_all1 |>
  group_by(Year) |>
  filter(!is.na(abun)) |>
  summarise(tag = max(Sweep)) |>
  print(n = Inf)

# this may be redundant with above
df_stn_tag_all <- df_all1 |>
  group_by(Year, Station_new) |>
  filter(!is.na(abun)) |>
  summarise(tag = max(Sweep)) |>
  print(n = Inf)
# plot(density(df_stn_tag_all$tag))


# join all and tags
df_all2 <- full_join(df_all1, df_stn_tag, by = c("Year")) |>
  arrange(Year, Species, Station_new, Sweep) |>
  filter( Sweep <= tag) #!is.na(abun) &

# replace NA with zero
df_all2 <- df_all2 |>
  replace_na(list(bio.sum = 0, abun = 0)) 

# write.csv(df_all2, "output/df_all2.csv")

# for analysis ----
df_a <- df_all2 |> 
  filter(Sweep <= 3) |>
  group_by(Year, Station_new, Species) |>
  summarise(abun = sum(abun),
            bio = sum(bio.sum))

## variables ----
df_a$treatment <- NA
df_a$time <- NA
df_a$type <- NA

df_a <- df_a |>
  mutate(treatment = if_else((Station_new == "C10L" |
                                Station_new == "C10U" |
                                Station_new == "C11" |
                                Station_new == "C12" |
                                Station_new == "C15" |
                                Station_new == "C16"),
                             "control", "impact")) |>
  mutate(time = if_else(Year <= 1990, "before", "after")) |>
  mutate(type = if_else((Station_new == "D1" |
                          Station_new == "T3" |
                          Station_new == "T5" |
                          Station_new == "T7" |
                          Station_new == "T9"),
                          "pool", "riffle"))
# write.csv(df_a, "output/df_a.csv")


# area ----
df_area <- read.csv("CS_Estimates/edited_cs_estimates_by_site.csv")
df_area <- df_area |> filter(!(Station == 45))
unique(df_area$Station)
str(df_area)

# remove spaces in species
for(i in seq_along(df_area$Species)){
  df_area$Species[i] <- gsub("*\\s", paste0("\\1"), df_area$Species[i])
}
unique(df_area$Species)

# make Station names the same
df_area$Station_new <- NA
for (i in 1:length(df_area$Station)){
  #browser()
  if (df_area$Station[i] == "C3" |
      df_area$Station[i] == "C10Upper") {
    df_area$Station_new[i] <- "C10U"
  }  else if (df_area$Station[i] == "C10lower") {
    df_area$Station_new[i] <- "C10L"
  } else if(df_area$Station[i] == "C4") {
    df_area$Station_new[i] <- "C11"
  } else if (df_area$Station[i] == "C5") { 
    df_area$Station_new[i] <- "C12"
  } else if (df_area$Station[i] == "D15"){
    df_area$Station_new[i] <- "C15"
  } else if (df_area$Station[i] == "D16"){
    df_area$Station_new[i] <- "C16"
  } else {
    df_area$Station_new[i] <- df_area$Station[i]
  }
}

unique(df_area$Station_new)

# add a C
df_area$Station_new <- ifelse(grepl("^[0-9]+$", df_area$Station_new), paste0("T", df_area$Station_new), df_area$Station_new)
unique(df_area$Station_new)

# add 1993 Stn 8 ASY and 1992 Stn 7 AS
tmp <- df_area[1:4,]
tmp[, 1:17] <- NA
tmp[1, c(1, 2, 16, 17)] <- c("AS", 1992, 58, "T7")
tmp[2, c(1, 2, 16, 17)] <- c("BTYOY", 1992, 58, "T7")
tmp[3, c(1, 2, 16, 17)] <- c("AS", 1993, 72, "T8")
tmp[4, c(1, 2, 16, 17)] <- c("ASYOY", 1993, 72, "T8")
tmp[, c(1, 2, 16, 17)]
tmp$Year <- as.integer(tmp$Year)
tmp$Area <- as.integer(tmp$Area)
str(tmp)

# remove errors in df_area - just 
df_area |>
  filter((Year == 1992 & Station_new == "T7") |
           (Year == 1993 & Station_new == "T8"))

df_area1 <- df_area |>
  filter(!(
    (Species == "BTYOY" & Year == 1992 & Station_new == "T7") |
      (Species == "AS" & Year == 1993 & Station_new == "T8")))

df_area1 |>
  filter((Year == 1992 & Station_new == "T7") |
           (Year == 1993 & Station_new == "T8"))


df_area1 <- rbind(df_area1, tmp)
str(df_area1)
# join ----
# View(df_a |>
#   group_by(Year, Species, Station_new) |>
#   summarise(count = n()) |>
#   pivot_wider(names_from = Year, values_from = count)
# )
unique(df_a$Station_new)

df_a1 <- left_join(df_a, df_area1[, c(1, 2, 16, 17)], 
          by = c("Year", 
                 "Species",
                 "Station_new"
                   ))
unique(df_a1$Station_new)
unique(df_a$Station_new)
unique(df_area$Station_new)
# write.csv(df_a1, "output/df_a1.csv")

# below was to try and figure out why some discrepancies, specifically the 1993 problem with T4 & T5 as well as 1993 with T7 & T8 
# df_a1 |> filter(is.na(Area)) |> select(Year, Station_new, Species, Area)
# df_area |> filter(is.na(Area)) |> select(Year, Station_new, Species, Area)
# df_area |> filter(
#   (Year == 1992 & Station_new == "T7") |
#     (Year == 1993 & Station_new == "T8")) |> select(Year, Station_new, Species, Area, Density_100)
# 
df_a1 |> filter(
  (Year == 1992 & Station_new == "T7") |
    (Year == 1993 & Station_new == "T8")
) |> select(Year, Station_new, Species, abun, Area)
# 
# df_a |> filter(
#   (Year == 1992 & Station_new == "T7") |
#     (Year == 1993 & (Station_new == "T8" | Station_new == "T4" | Station_new == "T5"))
# ) |> select(Year, Station_new, Species, abun)
# 
# df_a1 |> filter(
#   (Year == 1992 & Station_new == "C7") |
#     (Year == 1993 & (Station_new == "C8" | Station_new == "C4" | Station_new == "C5"))
# ) |> select(Year, Station_new, Species, abun)

# calc density ----
df_a2 <- df_a1 |>
  group_by(Year, Species, Station_new, type) |>
  mutate(abun.stand = abun/Area*100, bio.stand = bio/Area*100)
# write.csv(df_a2, "output/df_a2.csv")


# bootstrap ----
# produce confidence intervals
spp_den.ci <- df_a2 |>
  group_by(Species, Year, treatment, time, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_bio.ci <- df_a2 |>
  group_by(Species, Year, treatment, time, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
