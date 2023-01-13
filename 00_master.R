################################################################
# Path Specification                                           #
################################################################

# directory
main_path <- "N:/Corona_FDZ/Fluglaerm_PT/"
# airport related data and general data storage
dataFlug <- "N:/Corona_FDZ/Fluglaerm_PT/Daten/"
# source of the RED data
dataImmo <- "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/v6.1/"
# source of data on boundaries and regions
dataGebiete <- "M:/_FDZ/interne Daten/Gebietseinheit/"
# source of the coding files
codePath <- "N:/Corona_FDZ/Fluglaerm_PT/Codes/"
# auxiliary code
codeAux <- "N:/Corona_FDZ/Fluglaerm_PT/Codes/aux_code/"
# output path
outputPath <- "N:/Corona_FDZ/Fluglaerm_PT/Auswertung/output/"

################################################################
# Set Directory                                                #
################################################################

setwd(directoryPath)

###############################################################
# load libraries                                              #
###############################################################

pacman::p_load(dplyr)
pacman::p_load(data.table)
pacman::p_load(sf)
pacman::p_load(zoo)
pacman::p_load(ggplot2)
pacman::p_load(readxl)
pacman::p_load(fixest)
pacman::p_load(sp)
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(psych)
pacman::p_load(openxlsx)
pacman::p_load(fst)
pacman::p_load(lemon)
pacman::p_load(lubridate)
pacman::p_load(scales)
pacman::p_load(MetBrewer)
pacman::p_load(tidyr)

################################################################
# Preparation Files                                            #
################################################################


# basic preparation of the RED housing data -------------------------------

source(file.path(codePath, "01_02_preparation_basic_wm.R"), encoding = "UTF-8")
source(file.path(codePath, "01_03_preparation_basic_wk.R"), encoding = "UTF-8")

# Preparation Contour and housing data ------------------------------------

source(file.path(codePath, "01_06_preparation_contour_wm.R"), encoding = "UTF-8")
source(file.path(codePath, "01_07_preparation_contour_wk.R"), encoding = "UTF-8")

# Preparation flight activity data ----------------------------------------
# adds the number of flights to the housing data

# source(file.path(codePath, "01_08_preparation_flight_activity.R"), encoding = "UTF-8")


# Traveling time ----------------------------------------------------------
# defines the control group using the driving time to the respective airport

# source(file.path(codePath, "01_09_traveling_time.R"), encoding = "UTF-8")

# exports the files:
# - hk_time.fst
# - wm_time.fst
# - wk_time.fst
# which are the final data sets. It defines the control group to 25 minutes from the respective airport and covers a period of
# Jan 2019 to Dec 2020

################################################################
# Noise data                                                   #
################################################################

# aggregate the noise data to monthly values
source(file.path(codePath, "01_10_aggregation_noise.R"), encoding = "UTF-8")

################################################################
# Mapping and Descriptives                                     #
################################################################


# general descriptives and mapping ----------------------------------------
# plots the different airports on a Germany map
# plots noise levels over time
# creates descriptive tables
# creates number of observations per ring

source(file.path(codePath, "02_01_mapping_descriptives.R"), encoding = "UTF-8")


# maps of treated and control ---------------------------------------------
# maps for each airport the treated and the non-treated

source(file.path(codePath, "02_02_mapping_groups.R"), encoding = "UTF-8")


################################################################
# Estimation Files                                             #
################################################################

# estimation with contour rings -------------------------------------------
# estimation for housing data (both types WM and HK) under different specifications (see the file itself)

source(file.path(codePath, "03_01_estimation_hk.R"), encoding = "UTF-8")
source(file.path(codePath, "03_02_estimation_wm.R"), encoding = "UTF-8")
source(file.path(codePath, "03_03_estimation_wk.R"), encoding = "UTF-8")





# needed? -----------------------------------------------------------------






################################################################
# Common Trend                                                 #
################################################################

# plots the common trend for the four selected airports (TXL, DUS, FRA, HAM)
# and for four contour rings (6 (i.e. 1+2), 3, 4, 5)

source(file.path(codePath, "02_common_trend.R"), encoding = "UTF-8")

################################################################
# Interpolation                                                #
################################################################

source(file.path(codePath, "02_interpolation.R"), encoding = "UTF-8")
