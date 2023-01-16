###############################################################
# Description                                                 #
###############################################################

# aggregates the daily noise data to monthly data

###############################################################
# load data                                                   #
###############################################################

# Tegel -------------------------------------------------------------------
# txl19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Tegel/txl_complete_merged_2019.dta"))
# txl20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Tegel/txl_complete_merged.dta"))

# Schoenefeld -------------------------------------------------------------
# sxf19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Schoenefeld/sxf_complete_merged_2019.dta"))
# sxf20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Schoenefeld/sxf_complete_merged.dta"))

# Duesseldorf -------------------------------------------------------------
dus19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged_2019.dta"))
dus20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged.dta"))

# Frankfurt ---------------------------------------------------------------
fra18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2018.dta"))
fra19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2019.dta"))
fra20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged.dta"))
fra21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2021.dta"))

# Hamburg -----------------------------------------------------------------
ham19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged_2019.dta"))
ham20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged.dta"))

# Hannover ----------------------------------------------------------------
haj18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2018.dta"))
haj19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2019.dta"))
haj20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged.dta"))
haj21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2021.dta"))

# Leipzig -----------------------------------------------------------------
lej19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2019.dta"))
lej20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged.dta"))
lej21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2021.dta"))

# Muenchen ----------------------------------------------------------------
muc19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged_2019.dta"))
muc20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged.dta"))


###############################################################
# Preparation                                                 #
###############################################################

# merge both years to one data set
# txl <- rbind(txl19, txl20)
# sxf <- rbind(sxf19, sxf20)
dus <- rbind(dus19, dus20)
fra <- rbind(fra18, fra19, fra20, fra21)
ham <- rbind(ham19, ham20)
haj <- rbind(haj18, haj19, haj20, haj21)
lej <- rbind(lej19, lej20, lej21)
muc <- rbind(muc19, muc20)

# reassign zeros for MUC and DUS to NA
muc$leq_tag_flug[muc$leq_tag_flug == 0] <- NA
muc$leq_nacht_flug[muc$leq_nacht_flug == 0] <- NA
muc$l_den_flug[muc$l_den_flug == 0] <- NA

dus$leq_tag_flug[dus$leq_tag_flug == 0] <- NA
dus$leq_nacht_flug[dus$leq_nacht_flug == 0] <- NA

# create year month variable
# txl$year_mon <- format(as.Date(txl$date, format = "%Y-%m-%d"), "%Y-%m")
# sxf$year_mon <- format(as.Date(sxf$date, format = "%Y-%m-%d"), "%Y-%m")
dus$year_mon <- format(as.Date(dus$date, format = "%Y-%m-%d"), "%Y-%m")
fra$year_mon <- format(as.Date(fra$date, format = "%Y-%m-%d"), "%Y-%m")
ham$year_mon <- format(as.Date(ham$date, format = "%Y-%m-%d"), "%Y-%m")
haj$year_mon <- format(as.Date(haj$date, format = "%Y-%m-%d"), "%Y-%m")
lej$year_mon <- format(as.Date(lej$date, format = "%Y-%m-%d"), "%Y-%m")
muc$year_mon <- format(as.Date(muc$date, format = "%Y-%m-%d"), "%Y-%m")

# drop unneeded variables
# txl <- txl[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
# sxf <- sxf[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
dus <- dus[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
fra <- fra[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
ham <- ham[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
haj <- haj[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
lej <- lej[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
muc <- muc[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]

###############################################################
# Aggregation                                                 #
###############################################################

# merge everything together and aggregate based just on month
all <- rbind(dus, fra, ham, haj, lej, muc)

avg_month <- aggregate(list(avg_leq_tag_flug = all$leq_tag_flug,
                            avg_l_den_flug = all$l_den_flug,
                            avg_leq_nacht_flug = all$leq_nacht_flug,
                            lat_station = all$lat_station,
                            lon_station = all$lon_station),
                       by = list(year_mon = all$year_mon),
                       FUN = mean, na.rm = TRUE)

avg_airport <- aggregate(list(avg_leq_tag_flug = all$leq_tag_flug,
                              avg_l_den_flug = all$l_den_flug,
                              avg_leq_nacht_flug = all$leq_nacht_flug,
                              lat_station = all$lat_station,
                              lon_station = all$lon_station),
                         by = list(year_mon = all$year_mon,
                                   airport = all$airport_name),
                         FUN = mean, na.rm = TRUE)

# Tegel
avg_txl <- aggregate(list(avg_leq_tag_flug = txl$leq_tag_flug,
                            avg_l_den_flug = txl$l_den_flug,
                            avg_leq_nacht_flug = txl$leq_nacht_flug,
                            lat_station = txl$lat_station,
                            lon_station = txl$lon_station),
                       by = list(year_mon = txl$year_mon,
                                 station = txl$station),
                       FUN = mean, na.rm = TRUE)
avg_txl$airport <- "Airport Tegel"


# Schoenefeld
avg_sxf <- aggregate(list(avg_leq_tag_flug = sxf$leq_tag_flug,
                          avg_l_den_flug = sxf$l_den_flug,
                          avg_leq_nacht_flug = sxf$leq_nacht_flug,
                          lat_station = sxf$lat_station,
                          lon_station = sxf$lon_station),
                     by = list(year_mon = sxf$year_mon,
                               station = sxf$station),
                     FUN = mean, na.rm = TRUE)
avg_sxf$airport <- "Airport Schoenefeld"

# Duesseldorf
avg_dus <- aggregate(list(avg_leq_tag_flug = dus$leq_tag_flug,
                          avg_l_den_flug = dus$l_den_flug,
                          avg_leq_nacht_flug = dus$leq_nacht_flug,
                          lat_station = dus$lat_station,
                          lon_station = dus$lon_station),
                     by = list(year_mon = dus$year_mon,
                               station = dus$station),
                     FUN = mean, na.rm = TRUE)
avg_dus$airport <- "Airport Duesseldorf"


# Frankfurt
avg_fra <- aggregate(list(avg_leq_tag_flug = fra$leq_tag_flug,
                          avg_l_den_flug = fra$l_den_flug,
                          avg_leq_nacht_flug = fra$leq_nacht_flug,
                          lat_station = fra$lat_station,
                          lon_station = fra$lon_station),
                     by = list(year_mon = fra$year_mon,
                               station = fra$station),
                     FUN = mean, na.rm = TRUE)
avg_fra$airport <- "Airport Frankfurt"

# Hamburg
avg_ham <- aggregate(list(avg_leq_tag_flug = ham$leq_tag_flug,
                          avg_l_den_flug = ham$l_den_flug,
                          avg_leq_nacht_flug = ham$leq_nacht_flug,
                          lat_station = ham$lat_station,
                          lon_station = ham$lon_station),
                     by = list(year_mon = ham$year_mon,
                               station = ham$station),
                     FUN = mean, na.rm = TRUE)
avg_ham$airport <- "Airport Hamburg"


# Hannover
avg_haj <- aggregate(list(avg_leq_tag_flug = haj$leq_tag_flug,
                          avg_l_den_flug = haj$l_den_flug,
                          avg_leq_nacht_flug = haj$leq_nacht_flug,
                          lat_station = haj$lat_station,
                          lon_station = haj$lon_station),
                     by = list(year_mon = haj$year_mon,
                               station = haj$station),
                     FUN = mean, na.rm = TRUE)
avg_haj$airport <- "Airport Hannover"


# Leipzig
avg_lej <- aggregate(list(avg_leq_tag_flug = lej$leq_tag_flug,
                          avg_l_den_flug = lej$l_den_flug,
                          avg_leq_nacht_flug = lej$leq_nacht_flug,
                          lat_station = lej$lat_station,
                          lon_station = lej$lon_station),
                     by = list(year_mon = lej$year_mon,
                               station = lej$station),
                     FUN = mean, na.rm = TRUE)
avg_lej$airport <- "Airport Leipzig"


# Muenchen
avg_muc <- aggregate(list(avg_leq_tag_flug = muc$leq_tag_flug,
                          avg_l_den_flug = muc$l_den_flug,
                          avg_leq_nacht_flug = muc$leq_nacht_flug,
                          lat_station = muc$lat_station,
                          lon_station = muc$lon_station),
                     by = list(year_mon = muc$year_mon,
                               station = muc$station),
                     FUN = mean, na.rm = TRUE)
avg_muc$airport <- "Airport Muenchen"


###############################################################
# Add UTM coordinates                                         #
###############################################################

# all together
avg_month.sf <- st_as_sf(avg_month, coords = c("lon_station", "lat_station"), crs = 4326)
avg_month.sf <- st_transform(avg_month.sf, crs = 32632)
coords <- data.frame(st_coordinates(avg_month.sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_month <- cbind(avg_month, coords)

# Tegel
txl_sf <- st_as_sf(avg_txl, coords = c("lon_station", "lat_station"), crs = 4326)
txl_sf <- st_transform(txl_sf, crs = 32632)
coords <- data.frame(st_coordinates(txl_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_txl <- cbind(avg_txl, coords)

# Schoenefeld
sxf_sf <- st_as_sf(avg_sxf, coords = c("lon_station", "lat_station"), crs = 4326)
sxf_sf <- st_transform(sxf_sf, crs = 32632)
coords <- data.frame(st_coordinates(sxf_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_sxf <- cbind(avg_sxf, coords)

# Duesseldorf
dus_sf <- st_as_sf(avg_dus, coords = c("lon_station", "lat_station"), crs = 4326)
dus_sf <- st_transform(dus_sf, crs = 32632)
coords <- data.frame(st_coordinates(dus_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_dus <- cbind(avg_dus, coords)

# Frankfurt
fra_sf <- st_as_sf(avg_fra, coords = c("lon_station", "lat_station"), crs = 4326)
fra_sf <- st_transform(fra_sf, crs = 32632)
coords <- data.frame(st_coordinates(fra_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_fra <- cbind(avg_fra, coords)

# Hamburg
ham_sf <- st_as_sf(avg_ham, coords = c("lon_station", "lat_station"), crs = 4326)
ham_sf <- st_transform(ham_sf, crs = 32632)
coords <- data.frame(st_coordinates(ham_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_ham <- cbind(avg_ham, coords)

# Hannover
haj_sf <- st_as_sf(avg_haj, coords = c("lon_station", "lat_station"), crs = 4326)
haj_sf <- st_transform(haj_sf, crs = 32632)
coords <- data.frame(st_coordinates(haj_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_haj <- cbind(avg_haj, coords)

# Leipzig
lej_sf <- st_as_sf(avg_lej, coords = c("lon_station", "lat_station"), crs = 4326)
lej_sf <- st_transform(lej_sf, crs = 32632)
coords <- data.frame(st_coordinates(lej_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_lej <- cbind(avg_lej, coords)

# Muenchen
muc_sf <- st_as_sf(avg_muc, coords = c("lon_station", "lat_station"), crs = 4326)
muc_sf <- st_transform(muc_sf, crs = 32632)
coords <- data.frame(st_coordinates(muc_sf))
names(coords) <- c("lon_utm", "lat_utm")
avg_muc <- cbind(avg_muc, coords)

###############################################################
# Combine everything                                          #
###############################################################

avg_all <- rbind(avg_txl, avg_sxf, avg_dus, avg_fra, avg_ham, avg_haj, avg_lej, avg_muc)

###############################################################
# Export                                                      #
###############################################################

write.fst(avg_txl, path = file.path(dataFlug, "Hauptflughaefen_Laerm/txl_1920.fst"))
write.fst(avg_sxf, path = file.path(dataFlug, "Hauptflughaefen_Laerm/sxf_1920.fst"))
write.fst(avg_dus, path = file.path(dataFlug, "Hauptflughaefen_Laerm/dus_1920.fst"))
write.fst(avg_fra, path = file.path(dataFlug, "Hauptflughaefen_Laerm/fra_1920.fst"))
write.fst(avg_ham, path = file.path(dataFlug, "Hauptflughaefen_Laerm/ham_1920.fst"))
write.fst(avg_haj, path = file.path(dataFlug, "Hauptflughaefen_Laerm/haj_1920.fst"))
write.fst(avg_lej, path = file.path(dataFlug, "Hauptflughaefen_Laerm/lej_1920.fst"))
write.fst(avg_muc, path = file.path(dataFlug, "Hauptflughaefen_Laerm/muc_1920.fst"))
write.fst(avg_all, path = file.path(dataFlug, "Hauptflughaefen_Laerm/all_1920.fst"))
write.fst(avg_month, path = file.path(dataFlug, "Hauptflughaefen_Laerm/avg_month.fst"))
write.fst(avg_airport, path = file.path(dataFlug, "Hauptflughaefen_Laerm/avg_airport.fst"))

################################################################
# Clear everything                                             #
################################################################

rm(list=ls())

################################################################
# Restore original set up                                      #
################################################################

# directory
directoryPath <- "N:/Corona_FDZ/Fluglaerm_PT/Auswertung"
# airport related data and general data storage
dataFlug <- "N:/Corona_FDZ/Fluglaerm_PT/Daten/"
# source of the RED data
dataImmo <- "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/SUF/v4/"
# source of data on boundaries and regions
dataGebiete <- "M:/_FDZ/interne Daten/Gebietseinheit/"
# source of the coding files
codePath <- "N:/Corona_FDZ/Fluglaerm_PT/Codes/"
# output path
outputPath <- "N:/Corona_FDZ/Fluglaerm_PT/Auswertung/output/"


setwd(directoryPath)


library(haven)
library(dplyr)
library(data.table)
library(sf)
library(zoo)
library(ggplot2)
library(readxl)
library(fixest)
library(sp)
library(gstat)
library(tmap)
library(psych)
library(xlsx)
library(fst)

