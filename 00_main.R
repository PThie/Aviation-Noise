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
        library(broom)
    }
)

################################################################
# Globals                                                      #
################################################################

immo_version <- "v9"
utmcrs <- 32632
owndpi <- 800

# ggplot theme (line graphs)
owntheme <- ggplot2::theme(
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

# reading housing data
source(
    file.path(
        code_path,
        "functions/read_housing.R"
    )
)

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

#----------------------------------------------
# Preparation of raw housing data
# Creates the files: TYPES_prepared.qs (with TYPES equals WK, HK, or WM)

# Preparation of the apartment sales data
source(
    file.path(
        code_path,
        "01_01_preparation_basic_wk.R"
    )
)

# Preparation of the houses sales data
source(
    file.path(
        code_path,
        "01_02_preparation_basic_hk.R"
    )
)

# Preparation of the apartment rents data
source(
    file.path(
        code_path,
        "01_03_preparation_basic_wm.R"
    )
)

#----------------------------------------------
# Combine housing data and contour data for each housing type
# Creates the files: TYPES_contour.qs (with TYPES equals WK, HK, or WM)

source(
    file.path(
        code_path,
        "01_04_preparation_contour_wk.R"
    )
)

source(
    file.path(
        code_path,
        "01_05_preparation_contour_hk.R"
    )
)

source(
    file.path(
        code_path,
        "01_06_preparation_contour_wm.R"
    )
)

#----------------------------------------------
# Add additional control variables to the final housing data sets for each
# housing type (WK, HK, WM)
# Additional variables include: Distance to regional centers and other noise
# sources
# Creates the files: TYPES_complete.qs (with TYPES equals WK, HK, or WM)

source(
    file.path(
        code_path,
        "01_07_preparation_add_variables_wk.R"
    )
)

source(
    file.path(
        code_path,
        "01_08_preparation_add_variables_hk.R"
    )
)

source(
    file.path(
        code_path,
        "01_09_preparation_add_variables_wm.R"
    )
)

#----------------------------------------------
# Preparation of noise data (based on measuring stations)

source(
    file.path(
        code_path,
        "01_10_preparation_aggregation_noise.R"
    )
)

################################################################
# Mapping and Descriptives                                     #
################################################################

#----------------------------------------------
# Descriptives for comparing treatment and control region
# Using grid-level data

source(
    file.path(
        code_path,
        "02_01_descriptives_griddata.R"
    )
)

#----------------------------------------------
# Descriptives for the housing data

source(
    file.path(
        code_path,
        "02_02_descriptives_housing_data.R"
    )
)

#----------------------------------------------
# Descriptives for plotting the airport locations and the contour

source(
    file.path(
        code_path,
        "02_03_descriptives_contour_airport_locations.R"
    )
)

#----------------------------------------------
# Plotting the noise data

source(
    file.path(
        code_path,
        "02_04_descriptives_noise_data.R"
    )
)

#----------------------------------------------
# Plotting the flight activity data

source(
    file.path(
        code_path,
        "02_05_descriptives_flight_activity.R"
    )
)

#----------------------------------------------
# Plotting the stock development

source(
    file.path(
        code_path,
        "02_06_descriptives_stocks_expectations.R"
    )
)

################################################################
# Estimation Files                                             #
################################################################

#----------------------------------------------
# Baseline estimation

source(
    file.path(
        code_path,
        "03_01_estimation_baseline.R"
    )
)

#----------------------------------------------
# Heterogeneity Analysis

source(
    file.path(
        code_path,
        "03_02_estimation_heterogeneity.R"
    )
)

#----------------------------------------------
# Robustness Analysis

source(
    file.path(
        code_path,
        "03_03_estimation_robustness.R"
    )
)