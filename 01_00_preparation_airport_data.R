###############################################################
# Description                                                 #
###############################################################

# This file prepares the airport data (locations and noise shapes) for later
# usage.

# author: Patrick Thiel

###############################################################
# load data                                                   #
###############################################################

#----------------------------------------------
# airport locations
airports_loc <- read.xlsx(
    file.path(
        data_path, "Flughaefen/flughaefen_loc/flughaefen_germany_prepared.xlsx"
    ),
    sheet = 1
)

#----------------------------------------------
# noise contour

# main airports
haupt_contour <- st_read(
    file.path(
        data_path,
        "Contour_Maps/Hauptflughaefen/Mair_Lden_17.shp"
    ),
    quiet = TRUE
)

# airports in agglomeration areas
ball_contour <- st_read(
    file.path(
        data_path,
        "Contour_Maps/Ballungsraeume/Lden/Aggair_Lden_17.shp"
    ),
    quiet = TRUE
)

###############################################################
# preparation airport locations                               #
###############################################################

# keep only variables of interest
airports_loc <- airports_loc |>
    select("name", "city", "ICAO_code", "longitude", "latitude", "mainair") |>
    rename(
        icao = ICAO_code
    )

# select main airports
main_airports <- airports_loc |>
    filter(mainair == 1)

# drop those that are not main airports in the analysis
main_airports <- main_airports |>
    filter(
        icao == "EDDS" |
        icao == "EDDN" |
        icao == "EDDL" |
        icao == "EDDT" |
        icao == "EDDV" |
        icao == "EDDB" |
        icao == "EDDP" |
        icao == "EDDF" |
        icao == "EDDK" |
        icao == "EDDH" |
        icao == "EDDM"
    )

# select those that are airports in agglomeration areas
agg_airports <- airports_loc |>
    filter(
        icao == "EDLE" |
        icao == "EDFM" |
        icao == "EDLW" |
        icao == "EDDW" |
        icao == "EDFZ" |
        icao == "EDDC"
    )

# make sf and transform crs
main_airports <- st_as_sf(
    main_airports,
    coords = c("longitude", "latitude"),
    crs = 4326
)
main_airports <- st_transform(
    main_airports,
    crs = utmcrs
)

agg_airports <- st_as_sf(
    agg_airports,
    coords = c("longitude", "latitude"),
    crs = 4326
)
agg_airports <- st_transform(
    agg_airports,
    crs = utmcrs
)

# define all airports
airports <- rbind(main_airports, agg_airports)

#----------------------------------------------
# export

qs::qsave(
    airports,
    file.path(
        data_path,
        "Flughaefen/airport_locations_prep.qs"
    )
)

###############################################################
# preparation airport shapes                                  #
###############################################################

# transform
haupt_contour <- st_transform(
    haupt_contour,
    crs = utmcrs
)

ball_contour <- st_transform(
    ball_contour,
    crs = utmcrs
)

# keep relevant columns
haupt_contour <- haupt_contour |>
    select(ICAO, DB_Low, DB_High, geometry) |>
    rename(
        icao = ICAO
    )

# keep relevant columns
# add icao to agglomeration airports
ball_contour <- ball_contour |>
    select(Agglomerat, DB_Low, DB_High, geometry) |>
    mutate(
        icao = case_when(
            Agglomerat == "Essen" ~ "EDLE",
            Agglomerat == "Mülheim an der Ruhr" ~ "EDLE",
            Agglomerat == "Mannheim" ~ "EDFM",
            Agglomerat == "Dortmund" ~ "EDLW",
            Agglomerat == "Bremen" ~ "EDDW",
            Agglomerat == "Mainz" ~ "EDFZ",
            Agglomerat == "Dresden" ~ "EDDC"
        ),
        Agglomerat = NULL
    )

# add identifier for main airports
haupt_contour <- haupt_contour |>
    mutate(
        main_airport = 1
    )

ball_contour <- ball_contour |>
    mutate(
        main_airport = 0
    )

# combine both airport "types"
airport_contour <- rbind(
    haupt_contour, ball_contour
)

#----------------------------------------------
# export

qs::qsave(
    haupt_contour,
    file.path(
        data_path, "Contour_Maps/main_airports_contour.qs"
    )
)

qs::qsave(
    airport_contour,
    file.path(
        data_path, "Contour_Maps/all_airports_contour.qs"
    )
)
