###############################################################
# load data                                                   #
###############################################################

# housing data
red_org <- haven::read_dta(
    file.path(
        data_immo, "WM_allVersions_ohneText.dta"
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
        mietekalt = unclass(mietekalt),
        nebenkosten = unclass(nebenkosten),
        heizkosten = unclass(heizkosten),
        ajahr = unclass(ajahr),
        amonat = unclass(amonat),
        ejahr = unclass(ejahr),
        emonat = unclass(emonat),
        spell = unclass(spell),

        # housing characteristics
        baujahr = unclass(baujahr),
        wohnflaeche = unclass(wohnflaeche),
        nutzflaeche = unclass(nutzflaeche),
        zimmeranzahl = unclass(zimmeranzahl),
        badezimmer = unclass(badezimmer),
        ausstattung = unclass(ausstattung),
        heizungsart = unclass(heizungsart),
        objektzustand = unclass(objektzustand),
        etage = unclass(etage),
        balkon = unclass(balkon),
        einbaukueche = unclass(einbaukueche),
        garten = unclass(garten),

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
red$mietekalt[red$mietekalt < 0] <- NA

breaks_quantile <- as.numeric(
    quantile(red$mietekalt, c(0.01, 0.99), na.rm = TRUE)
)
breaks_quantile

# excluding values lower the 1% percentile and larger than the 99% percentile
red <- red[
    (red$mietekalt >= breaks_quantile[1] & red$mietekalt <= breaks_quantile[2]),
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


# baujahr -----------------------------------------------------------------
# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr <= 0] <- NA

# redefine baujahr if < 1500 (because unrealistic value)
red$baujahr[red$baujahr < 1500] <- 0

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

red$baujahr_catUNBEKANNT <- 0
red$baujahr_catUNBEKANNT[is.na(red$baujahr)] <- 1

red$nutzflaecheUNBEKANNT <- 0
red$nutzflaecheUNBEKANNT[is.na(red$nutzflaeche)] <- 1

red$badezimmerUNBEKANNT <- 0
red$badezimmerUNBEKANNT[is.na(red$badezimmer)] <- 1

red$ausstattungUNBEKANNT <- 0
red$ausstattungUNBEKANNT[is.na(red$ausstattung)] <- 1

red$heizungsartUNBEKANNT <- 0
red$heizungsartUNBEKANNT[is.na(red$heizungsart)] <- 1

red$objektzustandUNBEKANNT <- 0
red$objektzustandUNBEKANNT[is.na(red$objektzustand)] <- 1

#----------------------------------------------
# reassigning missing values
# main idea: when there is no value then it simply was not specified because
#  there is no such feature or just one (e.g. number of bathrooms)

red$nutzflaeche[is.na(red$nutzflaeche)] <- 0
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
# rent log
red$ln_mietekalt <- log(red$mietekalt)

# rent per square meter ---------------------------------------------------
red$rent_sqmeter <- red$mietekalt / red$wohnflaeche

# log rent per square meter -----------------------------------------------
red$ln_rent_sqmeter <- log(red$rent_sqmeter)

# CONTINUE here

###############################################################
# prepare coordinates                                         #
###############################################################

# drop objects which do not have a geographic information
red_geo <- red
red_geo <- red_geo[!is.na(red_geo$lat_gps), ]
red_geo <- red_geo[!is.na(red_geo$lon_gps), ]

# define as sf
red_geo <- st_as_sf(red_geo, coords = c("lon_gps", "lat_gps"), crs = 4326, remove = FALSE)
red_geo <- st_transform(red_geo, crs = 32632)

###############################################################
# additional controls                                         #
###############################################################

# create AGS with zeros
red_geo$AGS_gem <- as.character(red_geo$gid2019_gen)
red_geo$AGS_gem <- ifelse(nchar(red_geo$AGS_gem) == 7,
                          yes = paste0(0, red_geo$AGS_gem),
                          no = paste0(red_geo$AGS_gem))

red_geo$AGS_gem[red_geo$AGS_gem == "2e+06"] <- "02000000"
red_geo$AGS_gem[red_geo$AGS_gem == "01.1e+07"] <- "11000000"

# -------------------------------------------------------------------------
# regional types
regional_types$municipality <- NULL
regional_types$settlement_density <- NULL
red_geo <- merge(red_geo, regional_types, by.x = "AGS_gem", by.y = "ags", all.x = TRUE)


# -------------------------------------------------------------------------
# regional center
regional_center <- st_set_geometry(regional_center, regional_center$geometry)
regional_center <- st_transform(regional_center, crs = 32632)
regional_center <- regional_center[!st_is_empty(regional_center$geometry), ]

# subset for large center (Oberzentrum == 1)
largcenter <- regional_center[regional_center$center_identifier == 1, ]

# subset for medium center (Mittelzentrum == 2)
medcenter <- regional_center[regional_center$center_identifier == 2, ]

# subset for small center (Grundzentrum == 3)
smalcenter <- regional_center[regional_center$center_identifier == 3, ]


# -------------------------------------------------------------------------
# distances regional centers

# add distance to closest center to housing
red_geo$distance_largcenter <- as.numeric(apply(st_distance(red_geo, largcenter), 1, min)) / 1000 # to get kilometers
red_geo$distance_medcenter <- as.numeric(apply(st_distance(red_geo, medcenter), 1, min)) / 1000 # to get kilometers
red_geo$distance_smalcenter <- as.numeric(apply(st_distance(red_geo, smalcenter), 1, min)) / 1000 # to get kilometers

# -------------------------------------------------------------------------
# distance to airport building

##### prepare

# keep only variables of interest
airports_loc <- airports_loc %>% select("name", "city", "ICAO_code", "longitude", "latitude", "mainair")

# select main airports
main_airports <- airports_loc %>% filter(mainair == 1)

# drop those that are not main airports in the analysis
main_airports <- main_airports %>% filter(ICAO_code == "EDDS" |
                                            ICAO_code == "EDDN" |
                                            ICAO_code == "EDDL" |
                                            ICAO_code == "EDDT" |
                                            ICAO_code == "EDDV" |
                                            ICAO_code == "EDDB" |
                                            ICAO_code == "EDDP" |
                                            ICAO_code == "EDDF" |
                                            ICAO_code == "EDDK" |
                                            ICAO_code == "EDDH" |
                                            ICAO_code == "EDDM")


# select those that are airports in agglomeration areas
agg_airports <- airports_loc %>% filter(ICAO_code == "EDLE" |
                                          ICAO_code == "EDFM" |
                                          ICAO_code == "EDLW" |
                                          ICAO_code == "EDDW" |
                                          ICAO_code == "EDFZ" |
                                          ICAO_code == "EDDC")

# make sf and transform crs
main_airports <- st_as_sf(main_airports, coords = c("longitude", "latitude"), crs = 4326)
main_airports <- st_transform(main_airports, crs = 32632)

agg_airports <- st_as_sf(agg_airports, coords = c("longitude", "latitude"), crs = 4326)
agg_airports <- st_transform(agg_airports, crs = 32632)

# define all airports
airports <- rbind(main_airports, agg_airports)

##### distance

# add distance to closest center to housing
red_geo$distance_main_airports_building <- as.numeric(apply(st_distance(red_geo, main_airports), 1, min)) / 1000 # to get kilometers
red_geo$distance_all_airports_building <- as.numeric(apply(st_distance(red_geo, airports), 1, min)) / 1000 # to get kilometers

# -------------------------------------------------------------------------
# split up data to make distance calculates faster (for industry and railroad noise)

red_geo1 <- red_geo %>% filter(ajahr == 2018)
red_geo2 <- red_geo %>% filter(ajahr == 2019)
red_geo3 <- red_geo %>% filter(ajahr == 2020)
red_geo4 <- red_geo %>% filter(ajahr == 2021)


# -------------------------------------------------------------------------
# distance to airports

##### prepare

# add ICAO to additional airports
ball_contour$ICAO <- NA
ball_contour$ICAO[ball_contour$Agglomerat == "Essen"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mülheim an der Ruhr"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mannheim"] <- "EDFM"
ball_contour$ICAO[ball_contour$Agglomerat == "Dortmund"] <- "EDLW"
ball_contour$ICAO[ball_contour$Agglomerat == "Bremen"] <- "EDDW"
ball_contour$ICAO[ball_contour$Agglomerat == "Mainz"] <- "EDFZ"
ball_contour$ICAO[ball_contour$Agglomerat == "Dresden"] <- "EDDC"

ball_contour <- ball_contour[, c("ICAO", "DB_Low", "DB_High", "geometry")]

# identifier for main airports
haupt_contour$main_airport <- 1
ball_contour$main_airport <- 0

# combine both airport "types"
airport_contour <- rbind(haupt_contour, ball_contour)

# make union
main_airports_union <- haupt_contour %>% group_by(ICAO) %>% summarise(geometry = st_union(geometry))
airports_union <- airport_contour %>% group_by(ICAO) %>% summarise(geometry = st_union(geometry))


##### function to calculate distance
air_noise_distance <- function(data.sf){
  
  # identify nearest airport
  ordering_main_airports <- apply(st_distance(data.sf, main_airports_union), 1, which.min)
  ordering_all_airports <- apply(st_distance(data.sf, airports_union), 1, which.min)
  
  data.sf$closest_main_airports <- main_airports_union$ICAO[ordering_main_airports]
  data.sf$closest_all_airports <- airports_union$ICAO[ordering_all_airports]
  
  # add distance to closest center to housing
  data.sf$distance_main_airports <- as.numeric(apply(st_distance(data.sf, main_airports_union), 1, min)) / 1000 # to get kilometers
  data.sf$distance_all_airports <- as.numeric(apply(st_distance(data.sf, airports_union), 1, min)) / 1000 # to get kilometers
  
  # return
  return(data.sf)
}

##### apply function to subsets
red_geo1 <- air_noise_distance(red_geo1)
red_geo2 <- air_noise_distance(red_geo2)
red_geo3 <- air_noise_distance(red_geo3)
red_geo4 <- air_noise_distance(red_geo4)

# merge back together
red_geo <- rbind(red_geo1, red_geo2, red_geo3, red_geo4)

# -------------------------------------------------------------------------
# distance to industry

industry_noise <- st_transform(industry_noise, crs = 32632)

nearest_ind <- st_nearest_feature(red_geo, industry_noise)
red_geo$distance_industry <- as.numeric(st_distance(red_geo, industry_noise[nearest_ind, ], by_element = TRUE) / 1000)


# distance to rail --------------------------------------------------------

# transform crs
rail_noise <- st_transform(rail_noise, crs = 32632)

nearest_rail <- st_nearest_feature(red_geo, rail_noise)
red_geo$distance_railroads <- as.numeric(st_distance(red_geo, rail_noise[nearest_rail, ], by_element = TRUE) / 1000)


# distance to streets -----------------------------------------------------

# transform crs
streets <- st_transform(streets, crs = 32632)

nearest <- st_nearest_feature(red_geo, streets)
saveRDS(nearest, file.path(dataFlug, "housing/Temp/nearest_streets_wm.rds"))
red_geo$distance_streets <- as.numeric(st_distance(red_geo, streets[nearest, ], by_element = TRUE) / 1000)

###############################################################
# save data                                                   #
###############################################################

saveRDS(red_geo, file.path(dataFlug, "housing/Temp/red_WM_prepared.rds"))

###############################################################
# clear                                                       #
###############################################################

rm(list=ls())
