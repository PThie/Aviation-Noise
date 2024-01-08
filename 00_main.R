###############################################################
# load libraries                                              #
###############################################################

# install.packages(
#     c(
#         "dplyr", "data.table", "sf", "zoo", "ggplot2",
#         "readxl", "fixest", "sp", "gstat", "tmap",
#         "psych", "openxlsx", "fst", "lemon", "lubridate",
#         "scales", "MetBrewer", "tidyr", "qs", "docstring",
#         "jsonlite", "haven", "stringr", "ggmap", "cartography"
#     )
# )

suppressPackageStartupMessages(
    {
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
        library(openxlsx)
        library(fst)
        library(lemon)
        library(lubridate)
        library(scales)
        library(MetBrewer)
        library(tidyr)
        library(qs)
        library(docstring)
        library(stringr)
        library(ggmap)
        library(cartography)
    }
)

################################################################
# Globals                                                      #
################################################################

immo_version <- "v7.1"
utmcrs <- 32632
owndpi <- 800

# ggplot theme (line graphs)
owntheme <- theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17, vjust = 2),
    axis.text.y = element_text(size = 15),
    panel.background = element_rect(colour = "white", fill = "white"),
    axis.line = element_line(linewidth = 0.5, linetype = "solid", color = "black"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 15),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank()
)

################################################################
# Path Specification                                           #
################################################################

# directory
main_path <- "N:/Corona_FDZ/Fluglaerm_PT"
# airport related data and general data storage
data_path <- file.path(main_path, "data")
# source of the RED data
data_immo <- file.path(
    "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/",
    immo_version
)
# source of data on boundaries and regions
data_gebiete <- "M:/_FDZ/interne Daten/Gebietseinheit/"
# source of the coding files
code_path <- file.path(main_path, "code")
# auxiliary code
code_aux <- file.path(main_path, "code/aux_code")
# output path
output_path <- file.path(main_path, "output")

################################################################
# Set Directory                                                #
################################################################

setwd(main_path)

################################################################
# Global functions                                             #
################################################################

# intersection function
source(
    file.path(
        code_path,
        "functions/intersection_function.R"
    )
)

# subsetting and intersecting housing data with noise contour (using
# intersection function)
source(
    file.path(
        code_path,
        "functions/housing_cont.R"
    )
)

# preparation data for estimation
source(
    file.path(
        code_path,
        "functions/prep_est.R"
    )
)


# TODO: Needs rework (was never complete)
################################################################
# Preparation Files                                            #
################################################################

#----------------------------------------------
# preparation of airport contours

source(
    file.path(
        code_path,
        "01_00_preparation_airport_data.R"
    )
)

# basic preparation of the RED housing data -------------------------------

# source(file.path(codePath, "01_02_preparation_basic_wm.R"), encoding = "UTF-8")
# source(file.path(codePath, "01_03_preparation_basic_wk.R"), encoding = "UTF-8")

# Preparation Contour and housing data ------------------------------------

# source(file.path(codePath, "01_06_preparation_contour_wm.R"), encoding = "UTF-8")
# source(file.path(codePath, "01_07_preparation_contour_wk.R"), encoding = "UTF-8")

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
# source(file.path(codePath, "01_10_aggregation_noise.R"), encoding = "UTF-8")

################################################################
# Mapping and Descriptives                                     #
################################################################

# general descriptives and mapping ----------------------------------------
# plots the different airports on a Germany map
# plots noise levels over time
# creates descriptive tables
# creates number of observations per ring

# source(file.path(codePath, "02_01_mapping_descriptives.R"), encoding = "UTF-8")

# maps of treated and control ---------------------------------------------
# maps for each airport the treated and the non-treated

# source(file.path(codePath, "02_02_mapping_groups.R"), encoding = "UTF-8")

################################################################
# Estimation Files                                             #
################################################################

# estimation with contour rings -------------------------------------------
# estimation for housing data (both types WM and HK) under different specifications (see the file itself)

# source(file.path(codePath, "03_01_estimation_hk.R"), encoding = "UTF-8")
# source(file.path(codePath, "03_02_estimation_wm.R"), encoding = "UTF-8")
# source(file.path(codePath, "03_03_estimation_wk.R"), encoding = "UTF-8")





# needed? -----------------------------------------------------------------






################################################################
# Common Trend                                                 #
################################################################

# plots the common trend for the four selected airports (TXL, DUS, FRA, HAM)
# and for four contour rings (6 (i.e. 1+2), 3, 4, 5)

# source(file.path(codePath, "02_common_trend.R"), encoding = "UTF-8")

################################################################
# Interpolation                                                #
################################################################

# source(file.path(codePath, "02_interpolation.R"), encoding = "UTF-8")
