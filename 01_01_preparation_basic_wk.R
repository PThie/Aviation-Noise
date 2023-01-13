###############################################################
# load data                                                   #
###############################################################

# housing data
red_org <- haven::read_dta(
    file.path(
        data_immo, "WK_allVersions_ohneText.dta"
    )
)


# # regional info (regional centers settlement density, and  regional types)
# regional_types <- read.fst(file.path(dataFlug, "raumtyp/raumtyp_siedlungsdicht_nach_gemeinde_prep.fst"))
# regional_center <- readRDS(file.path(dataFlug, "raumzentren/raumzentren_nach_gemeinde_prep.rds"))

# # railroad noise
# rail_noise <- st_read(file.path(dataFlug, "umgebungslaerm/schiene/Basisdaten/Mrail_Source_17.shp"))

# # industrial noise
# industry_noise <- st_read(file.path(dataFlug, "umgebungslaerm/industrie/Ballungsraeume/Lden/Aggind_Lden_17.shp"))

# # street noise
# streets <- st_read(file.path(dataFlug, "umgebungslaerm/strasse/Hauptverkehrsstrassen/Basisdaten/Mroad_Source_17.shp"))


#----------------------------------------------
# airport locations
airport_locations <- qs::qread(
    file.path(
        data_path,
        "Flughaefen/airport_locations_prep.qs"
    )
)

#----------------------------------------------
# noise contour

# main airports
haupt_contour <- qs::qread(
    file.path(
        data_path,
        "Contour_Maps/main_airports_contour.qs"
    )
)

# all airports
air_contour <- qs::qread(
    file.path(
        data_path,
        "Contour_Maps/all_airports_contour.qs"
    )
)

###############################################################
# preparation                                                 #
###############################################################

# copy (to save reload time)
red <- as.data.frame(red_org)

#----------------------------------------------
# main variables
# drop irrelevant variables
# adjust the format of the remaining variables

red <- red |>
    mutate(
        # object characteristics
        obid = unclass(obid),
        kaufpreis = unclass(kaufpreis),
        ajahr = unclass(ajahr),
        amonat = unclass(amonat),
        ejahr = unclass(ejahr),
        emonat = unclass(emonat),
        spell = unclass(spell),

        # housing characteristics
        baujahr = unclass(baujahr),
        wohnflaeche = unclass(wohnflaeche),
        nutzflaeche = unclass(nutzflaeche),
        anzahletagen = unclass(anzahletagen),
        zimmeranzahl = unclass(zimmeranzahl),
        badezimmer = unclass(badezimmer),
        ausstattung = unclass(ausstattung),
        heizungsart = unclass(heizungsart),
        objektzustand = unclass(objektzustand),
        balkon = unclass(balkon),
        einbaukueche = unclass(einbaukueche),
        etage = unclass(etage),
        garten = unclass(garten),
        hits_gen = unclass(hits_gen),
        liste_show_gen = unclass(liste_show_gen),
        liste_match_gen = unclass(liste_match_gen),

        # geographic characteristics
        amr = unclass(erg_amd),
        kid2019 = unclass(kid2019),
        r1_id = unclass(ergg_1km),
        blid = unclass(blid),
        gid2019 = unclass(gid2019),
        plz = unclass(plz),
        lat_gps = unclass(lat_gps),
        lon_gps = unclass(lon_gps),
        .keep = "none"
    )

#----------------------------------------------
# remove duplicates and use the last spell

red$n <- ave(
    1:length(red$obid),
    red$obid,
    FUN = length
)

red <- red[red$n == red$spell, ]

# check if it worked (should be zero)
sum(duplicated(red$obid))

# remove aux variable
red$n <- NULL
red$spell <- NULL

###############################################################
# restrict years                                              #
###############################################################

red <- red[red$ajahr >= 2018, ]

###############################################################
# prepare independent variables                               #
###############################################################

#----------------------------------------------
# sales price
# restrict kaufpreis to reasonable range
red$kaufpreis[red$kaufpreis < 0] <- NA

breaks_quantile <- as.numeric(
    quantile(red$kaufpreis, c(0.01, 0.99), na.rm = TRUE)
)
breaks_quantile

red <- red[
    red$kaufpreis >= breaks_quantile[1] & red$kaufpreis <= breaks_quantile[2],
]

#----------------------------------------------
# living space
# restrict to reasonable range
red$wohnflaeche[red$wohnflaeche < 0] <- NA

breaks_quantile <- as.numeric(
    quantile(red$wohnflaeche, c(0.01, 0.99), na.rm = TRUE)
)
breaks_quantile

red <- red[
    red$wohnflaeche >= breaks_quantile[1] & red$wohnflaeche <= breaks_quantile[2], 
]

# creating a squared version
red$wohnflaeche_squ <- red$wohnflaeche^2

#----------------------------------------------
# dummy for first occupancy
red$first_occupancy <- 0
red$first_occupancy[red$objektzustand == 1] <- 1

#----------------------------------------------
# construction year

# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr <= 0] <- NA

# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr < 1500] <- NA

#----------------------------------------------
# district ID
# reassign the missings in kid2019
red$kid2019[is.na(red$kid2019)] <- 0

# dropping those variables where you dont have a Kreis ID
red <- red[red$kid2019 > 0, ]

#----------------------------------------------
# balcony
red$balkon[red$balkon < 0] <- 0

#----------------------------------------------
# garden
red$garten[red$garten < 0] <- 0

#----------------------------------------------
# built-in kitchen
red$einbaukueche[red$einbaukueche < 0] <- 0

#----------------------------------------------
# re-assign the remaining missing values
red[red < 0] <- NA

#----------------------------------------------
# constructing date variables (year and month)

# starting date
red$start_date <- ymd(
    paste(red$ajahr, red$amonat, "01", sep = "-")
)
red$year_mon_start <- format(
    as.Date(red$start_date), "%Y-%m"
)

# ending date
red$end_date <- ymd(
    paste(red$ejahr, red$emonat, "01", sep = "-")
)
red$year_mon_end <- format(
    as.Date(red$end_date), "%Y-%m"
)

#----------------------------------------------
# construct dummies for unknown values
red$etageUNBEKANNT <- 0
red$etageUNBEKANNT[is.na(red$etage)] <- 1

red$einbaukuecheUNBEKANNT <- 0
red$einbaukuecheUNBEKANNT[is.na(red$einbaukueche)] <- 1

red$balkonUNBEKANNT <- 0
red$balkonUNBEKANNT[is.na(red$balkon)] <- 1

red$gartenUNBEKANNT <- 0
red$gartenUNBEKANNT[is.na(red$garten)] <- 1

red$nutzflaecheUNBEKANNT <- 0
red$nutzflaecheUNBEKANNT[is.na(red$nutzflaeche)] <- 1

red$anzahletagenUNBEKANNT <- 0
red$anzahletagenUNBEKANNT[is.na(red$anzahletagen)] <- 1

red$badezimmerUNBEKANNT <- 0
red$badezimmerUNBEKANNT[is.na(red$badezimmer)] <- 1

red$ausstattungUNBEKANNT <- 0
red$ausstattungUNBEKANNT[is.na(red$ausstattung)] <- 1

red$heizungsartUNBEKANNT <- 0
red$heizungsartUNBEKANNT[is.na(red$heizungsart)] <- 1

red$objektzustandUNBEKANNT <- 0
red$objektzustandUNBEKANNT[is.na(red$objektzustand)] <- 1

red$baujahr_catUNBEKANNT <- 0
red$baujahr_catUNBEKANNT[is.na(red$baujahr)] <- 1

#----------------------------------------------
# reassigning missing values
# main idea: when there is no value then it simply was not specified because
#  there is no such feature or just one (e.g. number of bathrooms)

red$nutzflaeche[is.na(red$nutzflaeche)] <- 0
# implausible to have no floors (there must be at least one floor)
red$anzahletagen[is.na(red$anzahletagen)] <- 1
# implausible that a house does not have a bathroom
red$badezimmer[is.na(red$badezimmer)] <- 1
# assume "normal" ausstattung if not further specified
red$ausstattung[is.na(red$ausstattung)] <- 2
# assume central heating (Zentralheizung) if not further specified
# (most comman type of heating)
red$heizungsart[is.na(red$heizungsart)] <- 13
# assume "gepflegt"
red$objektzustand[is.na(red$objektzustand)] <- 7
# set floor to median value
red$etage[is.na(red$etage)] <- median(red$etage, na.rm = TRUE)

#----------------------------------------------
# age
# construct a variables which specifies the age of the building
red$alter <- NA
red$alter <- red$ejahr - red$baujahr
red$alter[red$alter <= 0] <- NA

# if age us unknown specfied as median age
red$alter[is.na(red$alter)] <- median(red$alter, na.rm = TRUE)

# construct squared age (to capture non-linear trend)
red$alter_squ <- NA
red$alter_squ <- red$alter^2

# construct UNBEKANNT variable
red$alterUNBEKANNT <- 0
red$alterUNBEKANNT[is.na(red$alter)] <- 1

###############################################################
# prepare dependent variables                                 #
###############################################################

#----------------------------------------------
# price per square meters
red$price_sqmeter <- red$kaufpreis / red$wohnflaeche

# generate log of price per square meters
red$ln_price_sqmeter <- log(red$price_sqmeter)

#----------------------------------------------
# price log
red$ln_flatprice <- log(red$kaufpreis)

###############################################################
# prepare coordinates                                         #
###############################################################

# drop objects which do not have a geographic information
red_geo <- red
red_geo <- red_geo[!is.na(red_geo$lat_gps), ]
red_geo <- red_geo[!is.na(red_geo$lon_gps), ]

# define as sf
red_geo <- st_as_sf(
    red_geo,
    coords = c("lon_gps", "lat_gps"),
    crs = 4326,
    remove = FALSE
)
red_geo <- st_transform(
    red_geo,
    crs = utmcrs
)

# CONTINUE:
# Put in different file

###############################################################
# additional controls                                         #
###############################################################

# # create AGS with zeros
# red_geo$AGS_gem <- as.character(red_geo$gid2019)
# red_geo$AGS_gem <- ifelse(nchar(red_geo$AGS_gem) == 7,
#                           yes = paste0(0, red_geo$AGS_gem),
#                           no = paste0(red_geo$AGS_gem))

# red_geo$AGS_gem[red_geo$AGS_gem == "2e+06"] <- "02000000"
# red_geo$AGS_gem[red_geo$AGS_gem == "01.1e+07"] <- "11000000"

# # -------------------------------------------------------------------------
# # regional types
# regional_types$municipality <- NULL
# regional_types$settlement_density <- NULL
# red_geo <- merge(red_geo, regional_types, by.x = "AGS_gem", by.y = "ags", all.x = TRUE)


# # -------------------------------------------------------------------------
# # regional center
# regional_center <- st_set_geometry(regional_center, regional_center$geometry)
# regional_center <- st_transform(regional_center, crs = 32632)
# regional_center <- regional_center[!st_is_empty(regional_center$geometry), ]

# # subset for large center (Oberzentrum == 1)
# largcenter <- regional_center[regional_center$center_identifier == 1, ]

# # subset for medium center (Mittelzentrum == 2)
# medcenter <- regional_center[regional_center$center_identifier == 2, ]

# # subset for small center (Grundzentrum == 3)
# smalcenter <- regional_center[regional_center$center_identifier == 3, ]


# -------------------------------------------------------------------------
# distances regional centers

# add distance to closest center to housing
# red_geo$distance_largcenter <- as.numeric(apply(st_distance(red_geo, largcenter), 1, min)) / 1000 # to get kilometers
# red_geo$distance_medcenter <- as.numeric(apply(st_distance(red_geo, medcenter), 1, min)) / 1000 # to get kilometers
# red_geo$distance_smalcenter <- as.numeric(apply(st_distance(red_geo, smalcenter), 1, min)) / 1000 # to get kilometers

# NOTE: I continued here

###############################################################
# distance to airport building                                #
###############################################################

main_airports <- airport_locations |>
    filter(mainair == 1)

# add distance to closest main airport (in km)
red_geo$distance_main_airports_building <- as.numeric(
    apply(st_distance(red_geo, main_airports), 1, min)
) / 1000

# add distance to the closest airport in general
red_geo$distance_all_airports_building <- as.numeric(
    apply(st_distance(red_geo, airport_locations), 1, min)
) / 1000

###############################################################
# add closest airport based on shape                          #
###############################################################

#----------------------------------------------
# make union

# for main airports
main_airports_union <- haupt_contour |>
    group_by(icao) |>
    summarise(geometry = st_union(geometry))

# for all airports
airports_union <- air_contour |>
    group_by(icao) |>
    summarise(geometry = st_union(geometry))

#----------------------------------------------
# distance to nearest shape

# function to calculate distance
air_noise_distance <- function(data_sf) {
    # identify nearest airport
    ordering_main_airports <- apply(
        st_distance(data_sf, main_airports_union), 1, which.min
    )
    ordering_all_airports <- apply(
        st_distance(data_sf, airports_union), 1, which.min
    )

    # add closest airport
    data_sf$closest_main_airports <- main_airports_union$icao[
        ordering_main_airports
    ]
    data_sf$closest_all_airports <- airports_union$icao[
        ordering_all_airports
    ]

    # add distance to closest airport shape (in km)
    data_sf$distance_main_airports <- as.numeric(
        apply(st_distance(data_sf, main_airports_union), 1, min)
    ) / 1000
    data_sf$distance_all_airports <- as.numeric(
        apply(st_distance(data_sf, airports_union), 1, min)
    ) / 1000

    # return
    return(data_sf)
}

# split data set to make distance calculation faster
red_geo1 <- red_geo |> filter(ajahr == 2018)
red_geo2 <- red_geo |> filter(ajahr == 2019)
red_geo3 <- red_geo |> filter(ajahr == 2020)
red_geo4 <- red_geo |> filter(ajahr == 2021)
red_geo5 <- red_geo |> filter(ajahr == 2022)

# apply function to subsets
red_geo1 <- air_noise_distance(red_geo1)
red_geo2 <- air_noise_distance(red_geo2)
red_geo3 <- air_noise_distance(red_geo3)
red_geo4 <- air_noise_distance(red_geo4)
red_geo5 <- air_noise_distance(red_geo5)

# merge back together
red_geo <- rbind(
    red_geo1, red_geo2, red_geo3, red_geo4, red_geo5
)

# # -------------------------------------------------------------------------
# # distance to industry

# industry_noise <- st_transform(industry_noise, crs = 32632)

# nearest_ind <- st_nearest_feature(red_geo, industry_noise)
# red_geo$distance_industry <- as.numeric(st_distance(red_geo, industry_noise[nearest_ind, ], by_element = TRUE) / 1000)


# # distance to rail --------------------------------------------------------

# # transform crs
# rail_noise <- st_transform(rail_noise, crs = 32632)

# nearest_rail <- st_nearest_feature(red_geo, rail_noise)
# red_geo$distance_railroads <- as.numeric(st_distance(red_geo, rail_noise[nearest_rail, ], by_element = TRUE) / 1000)


# # distance to streets -----------------------------------------------------

# # transform crs
# streets <- st_transform(streets, crs = 32632)

# nearest <- st_nearest_feature(red_geo, streets)
# saveRDS(nearest, file.path(dataFlug, "housing/Temp/nearest_streets_wk.rds"))
# red_geo$distance_streets <- as.numeric(st_distance(red_geo, streets[nearest, ], by_element = TRUE) / 1000)

###############################################################
# save data                                                   #
###############################################################

qs::qsave(
    red_geo,
    file.path(
        dataFlug,
        "housing/WK_prepared.qs"
    )
)