################################################################
# Description                                                  #
################################################################

# This file generates all the robustness checks presented in the paper.

################################################################
# Load Data                                                    #
################################################################

wk_housing <- read_housing(filename = "WK_complete")
hk_housing <- read_housing(filename = "HK_complete")
wm_housing <- read_housing(filename = "WM_complete")

#----------------------------------------------
# read grid data

griddata <- read.fst(
    file.path(
        data_path,
        "griddata/griddata_prep.fst"
    )
)

#----------------------------------------------
# different grid specification

grid500 <- st_read(
    file.path(
        data_path,
        "raster/DE_Grid_ETRS89-UTM32_500m.gpkg/geogitter/DE_Grid_ETRS89-UTM32_500m.gpkg"
    ),
    quiet = TRUE
)
grid250 <- st_read(
    file.path(
        data_path,
        "raster/DE_Grid_ETRS89-UTM32_250m.gpkg/DE_Grid_ETRS89-UTM32_250m/geogitter/DE_Grid_ETRS89-UTM32_250m.gpkg"
    ),
    quiet = TRUE
)
grid5000 <- st_read(
    file.path(
        data_path,
        "raster/DE_Grid_ETRS89-UTM32_5km.gpkg/geogitter/DE_Grid_ETRS89-UTM32_5km.gpkg"
    ),
    quiet = TRUE
)

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

################################################################
# Preparation                                                  #
################################################################

# preparation function
# slightly different from general one because now with option to include
# lockdown
# also add periods definition (needed for pre-trend testing)

prep_est_march <- function(housing_data, drop_march = FALSE){
    #' @title Preparation for estimation
    #' 
    #' @description This function prepares the data for estimation.
    #' 
    #' @param housing_data Housing data after contour information and additonal
    #' variables have been added
    #' @param drop_march Option to exclude March (= TRUE)
    #'   
    #' @return Returns housing data ready for estimation
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # drop geometry
    housing_data <- st_drop_geometry(housing_data)

    # make "months" factor variable
    housing_data$months <- as.factor(housing_data$year_mon_end)
    
    # restrict sample
    housing_data <- housing_data |>
        # restrict to 5km to contour ring
        filter(distance_main_airports <= 5) |>
        # exlcude 1km buffer
        filter(distance_main_airports >= 1 | distance_main_airports == 0) |>
        # drop Airports Tegel and Schoenefeld
        filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
    
    # add ring for 60dB and above
    housing_data <- housing_data |>
        mutate(
            con_ring8 = case_when(
                con_ring1 == 1 |
                con_ring2 == 1 |
                con_ring3 == 1 |
                con_ring4 == 1 ~ 1,
                con_ring5 == 1 ~ 0
            )
        )

    housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0

    # option to exclude March 2020
    # periods definition in else statement because otherwise you cannot define
    # March 2020 as t
    if (drop_march == TRUE) {
        housing_data <- housing_data |>
            filter(year_mon_end != "2020-03")
    } else {
        # define periods (for temporal dynamics)
        housing_data <- housing_data |>
            mutate(
                periods = case_when(
                    year_mon_end >= "2018-01" & year_mon_end <= "2018-08" ~ "t-4",
                    year_mon_end >= "2018-09" & year_mon_end <= "2019-02" ~ "t-3",
                    year_mon_end >= "2019-03" & year_mon_end <= "2019-08" ~ "t-2",
                    year_mon_end >= "2019-09" & year_mon_end <= "2020-02" ~ "t-1",
                    year_mon_end == "2020-03" ~ "t",
                    year_mon_end >= "2020-04" & year_mon_end <= "2020-06" ~ "t+1",
                    year_mon_end >= "2020-07" & year_mon_end <= "2020-09" ~ "t+2",
                    year_mon_end >= "2020-10" & year_mon_end <= "2020-12" ~ "t+3",
                    year_mon_end >= "2021-01" & year_mon_end <= "2021-03" ~ "t+4",
                    year_mon_end >= "2021-04" & year_mon_end <= "2021-06" ~ "t+5",
                    year_mon_end >= "2021-07" & year_mon_end <= "2021-09" ~ "t+6",
                    year_mon_end >= "2021-10" & year_mon_end <= "2021-12" ~ "t+7",
                    year_mon_end >= "2022-01" & year_mon_end <= "2022-03" ~ "t+8",
                    year_mon_end >= "2022-04" & year_mon_end <= "2022-06" ~ "t+9"
                ),
                periods = as.factor(periods)
            )
    }

    # return
    return(housing_data)
}

wk_prep <- prep_est(wk_housing)
hk_prep <- prep_est_march(hk_housing)
wm_prep <- prep_est_march(wm_housing)

#----------------------------------------------
# table labels (for TEX output)
tablabel_char <- c(
    "alter" = "Age", "alter_squ" = "Age$^2$", "wohnflaeche" = "Living space", "wohnflaeche_squ" = "Living space$^2$", 
    "as.factor(objektzustand)2" = "Condition: First occupancy after reconstruction", "as.factor(objektzustand)3" = "Condition: Like new", "as.factor(objektzustand)4" = "Condition: Reconstructed",
    "as.factor(objektzustand)5" = "Condition: Modernised", "as.factor(objektzustand)6" = "Condition: Completely renovated", "as.factor(objektzustand)7" = "Condition: Well kempt",
    "as.factor(objektzustand)8" = "Condition: Needs renovation", "as.factor(objektzustand)9" = "Condition: By arrangement", "as.factor(objektzustand)10" = "Condition: Dilapidated", "objektzustandUNBEKANNT" = "Condition (unknown)",
    "as.factor(heizungsart)2" = "Heating: Electric heating", "as.factor(heizungsart)3" = "Heating: Self-contained central heating", "as.factor(heizungsart)4" = "Heating: District heating", 
    "as.factor(heizungsart)5" = "Heating: Floor heating", "as.factor(heizungsart)6" = "Heating: Gas heating", "as.factor(heizungsart)7" = "Heating: Wood pellet heating", 
    "as.factor(heizungsart)8" = "Heating: Night storage heating", "as.factor(heizungsart)9" = "Heating: Heating by stove", "as.factor(heizungsart)10" = "Heating: Oil heating",
    "as.factor(heizungsart)11" = "Heating: Solar heating", "as.factor(heizungsart)12" = "Heating: Thermal heat pump", "as.factor(heizungsart)13" = "Heating: Central heating",
    "heizungsartUNBEKANNT" = "Heating (unknown)", "as.factor(ausstattung)2" = "Endowment: Normal", "as.factor(ausstattung)3" = "Endowment: Sophisticated", "as.factor(ausstattung)4" = "Endowment: Deluxe",
    "ausstattungUNBEKANNT" = "Endowment (unknown)", "badezimmer" = "Number bathrooms", "badezimmerUNBEKANNT" = "Number bathrooms (unknown)",
    "etage" = "Floor", "balkon" = "Balcony", "einbaukueche" = "Built-in kitchen", "garten" = "Garten"
)

#----------------------------------------------
# baseline formula function
reg_base_allring_lockdown <- function(dep_var, cnt) {
    #' @param dep_var Dependent variable (LHS)
    #' @param cnt Controls
    fm <- formula(
        paste(dep_var," ~",
        paste(cnt, collapse = " + "),
        # coefficient of interest (interaction term)
        paste("+ fir_lockdown * con_ring0"),
        "| months + r1_id")
    )
    return(fm)
}

#----------------------------------------------
# estimation function
est_fm <- function(df, fm){
    feols(fml = fm, data = df)
}

#----------------------------------------------
# controls for apartment purchases (WK)

controls <- c(
    "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
    "etage", "etageUNBEKANNT", "balkon", "balkonUNBEKANNT",
    "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
    "einbaukueche", "einbaukuecheUNBEKANNT", "garten", "gartenUNBEKANNT",
    "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
    "badezimmer", "badezimmerUNBEKANNT",
    "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
    "distance_industry", "distance_railroads", "distance_streets",
    "distance_all_airports_building"
)

################################################################
# Pretrends other housing types                                #
################################################################

# house sales
hk_trend_est <- feols(
    ln_houseprice ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
        anzahletagen + anzahletagenUNBEKANNT + zimmeranzahl + grundstuecksflaeche +
        grundstuecksflaeche_squ + as.factor(objektzustand) + objektzustandUNBEKANNT +
        as.factor(heizungsart) + heizungsartUNBEKANNT + as.factor(ausstattung) + 
        ausstattungUNBEKANNT + badezimmer + badezimmerUNBEKANNT + distance_largcenter + 
        distance_medcenter + distance_smalcenter + distance_all_airports_building + 
        distance_industry + distance_railroads + distance_streets + con_ring0 +
        i(con_ring0, factor_var = periods, ref = "t"),
        data = hk_prep, fixef = c("months", "r1_id"), se = "hetero"
)

# apartment rents
wm_trend_est <- feols(
    ln_rent_sqmeter ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
        etage + etageUNBEKANNT + balkon + balkonUNBEKANNT + zimmeranzahl +
        as.factor(objektzustand) + objektzustandUNBEKANNT + einbaukueche +
        einbaukuecheUNBEKANNT + garten + gartenUNBEKANNT + as.factor(heizungsart) + 
        heizungsartUNBEKANNT + as.factor(ausstattung) + ausstattungUNBEKANNT + 
        badezimmer + badezimmerUNBEKANNT + distance_largcenter + distance_medcenter + 
        distance_smalcenter + distance_all_airports_building + distance_industry + 
        distance_railroads + distance_streets + con_ring0 +
        i(con_ring0, factor_var = periods, ref = "t"),
        data = wm_prep, fixef = c("months", "r1_id"), se = "hetero"
)

# export
esttex(
    hk_trend_est, wm_trend_est,
    file = file.path(output_path, "regressions/pretrends_hk_wm.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("house sales", "apart rent")
)

################################################################
# Include neutral zone                                         #
################################################################
# include 1 km buffer around contour

#----------------------------------------------
# preparation

prep_buffer <- function(housing_data){
    #' @title Preparation for estimation
    #' 
    #' @description This function prepares the data for estimation. Simiarl to
    #' prep_est function but some modification (like including neutral zone)
    #' 
    #' @param housing_data Housing data after contour information and additonal
    #' variables have been added
    #'   
    #' @return Returns housing data ready for estimation
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # drop geometry
    housing_data <- st_drop_geometry(housing_data)

    # make "months" factor variable
    housing_data$months <- as.factor(housing_data$year_mon_end)
    
    # restrict sample
    housing_data <- housing_data |>
        # restrict to 5km to contour ring
        filter(distance_main_airports <= 5) |>
        # drop March 2020
        filter(year_mon_end != "2020-03") |>
        # drop Airports Tegel and Schoenefeld
        filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
    
    # add ring for 60dB and above
    housing_data <- housing_data |>
        mutate(
            con_ring8 = case_when(
                con_ring1 == 1 |
                con_ring2 == 1 |
                con_ring3 == 1 |
                con_ring4 == 1 ~ 1,
                con_ring5 == 1 ~ 0
            )
        )

    housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0

    # return
    return(housing_data)
}

# apply function
wk_prep_withbuffer <- prep_buffer(wk_housing)

# specify formula
wk_base <- reg_base_allring_lockdown(dep_var = "ln_flatprice", cnt = controls)

#----------------------------------------------
# estimation
wk_base_nz <- est_fm(df = wk_prep_withbuffer, fm = wk_base)

# display results
etable(
    wk_base_nz,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

################################################################
# Change of  FE                                                #
################################################################

#----------------------------------------------
# prep grid function
prep_grid <- function(grid_sf, gridIDname){
    # keep only relevant columns
    grid_sf <- grid_sf |>
        select(id, geom)
    
    # rename
    names(grid_sf)[names(grid_sf) == "id"] <- paste0("id_", gridIDname)
    
    # set geometry
    grid_sf <- st_set_geometry(grid_sf, grid_sf$geom)
    
    # transform
    grid_sf <- st_transform(grid_sf, crs = 32632)
    
    # return
    return(grid_sf)
}

# apply prep function
grid250_prep <- prep_grid(grid250, gridIDname = "250m")
grid500_prep <- prep_grid(grid500, gridIDname = "500m")
grid5000_prep <- prep_grid(grid5000, gridIDname = "5km")

#----------------------------------------------
# join to housing data

# make spatial data
wk_prep_sf <- st_as_sf(
    wk_prep, coords = c("lon_gps", "lat_gps"), crs = 4326
)

# transform
wk_prep_sf <- st_transform(wk_prep_sf, crs = st_crs(grid5000_prep))

# join with grids
wk_prep_sf <- st_join(wk_prep_sf, grid250_prep, left = TRUE, largest = TRUE)
wk_prep_sf <- st_join(wk_prep_sf, grid500_prep, left = TRUE, largest = TRUE)
wk_prep_sf <- st_join(wk_prep_sf, grid5000_prep, left = TRUE, largest = TRUE)

# drop geometry
wk_prep_grid <- st_drop_geometry(wk_prep_sf)

# intermediate save
write_fst(
    wk_prep_grid,
    file.path(
        data_path,
        "housing/wk_contour_new_griddef.fst"
    )
)

#----------------------------------------------
# estimation

# estimation function
reg_base_allring_lockdown_grids <- function(dep_var, gridID){
    fm <- formula(
        paste(dep_var," ~", 
        paste(controls, collapse = " + "), 
        paste("+ fir_lockdown * con_ring0"), 
        "| months + ", gridID)
        )
    return(fm)
}

# specify formula
wk_base_250mgrid <- reg_base_allring_lockdown_grids(dep_var = "ln_flatprice", gridID = "id_250m")
wk_base_500mgrid <- reg_base_allring_lockdown_grids(dep_var = "ln_flatprice", gridID = "id_500m")
wk_base_5kmgrid <- reg_base_allring_lockdown_grids(dep_var = "ln_flatprice", gridID = "id_5km")

# run regression
wk_base_est_250mgrid <- est_fm(df = wk_prep_grid, fm = wk_base_250mgrid)
wk_base_est_500mgrid <- est_fm(df = wk_prep_grid, fm = wk_base_500mgrid)
wk_base_est_5kmgrid <- est_fm(df = wk_prep_grid, fm = wk_base_5kmgrid)

# display results
etable(
    wk_base_est_250mgrid, wk_base_est_500mgrid, wk_base_est_5kmgrid,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

################################################################
# Time trend                                                   #
################################################################

#----------------------------------------------
# preparation

# set municipalities as factor
wk_prep$gid2019_fac <- as.factor(wk_prep$gid2019)

# define formula function
reg_base_allring_lockdown_trend <- function(dep_var){
    fm <- formula(
        paste(dep_var," ~", 
        paste(controls, collapse = " + "), 
        paste("+ fir_lockdown * con_ring0"), 
        paste("+ i(gid2019_fac, months)"),
        "| months + r1_id")
    )
    return(fm)
}

# specify formula
wk_base_trend <- reg_base_allring_lockdown_trend(dep_var = "ln_flatprice")

#----------------------------------------------
# estimation
wk_base_trend <- est_fm(df = wk_prep, fm = wk_base_trend)

# display results
etable(
    wk_base_trend,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero",
    drop = "months"
)

################################################################
# Different control distances                                  #
################################################################

#----------------------------------------------
# preparation

# preparation function
# similar to before but with option to define maximum distance of the control
# region
prep_est_distances <- function(housing_data, control_distance){
    #' @title Preparation for estimation
    #' 
    #' @description This function prepares the data for estimation.
    #' 
    #' @param housing_data Housing data after contour information and additonal
    #' variables have been added
    #' @param control_distance Maximum distance for control contour
    #'   
    #' @return Returns housing data ready for estimation
    #' 
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # drop geometry
    housing_data <- st_drop_geometry(housing_data)

    # make "months" factor variable
    housing_data$months <- as.factor(housing_data$year_mon_end)
    
    # restrict sample
    housing_data <- housing_data |>
        # restrict to 5km to contour ring
        filter(distance_main_airports <= control_distance) |>
        # exlcude 1km buffer
        filter(distance_main_airports >= 1 | distance_main_airports == 0) |>
        # drop March 2020
        filter(year_mon_end != "2020-03") |>
        # drop Airports Tegel and Schoenefeld
        filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
    
    # add ring for 60dB and above
    housing_data <- housing_data |>
        mutate(
            con_ring8 = case_when(
                con_ring1 == 1 |
                con_ring2 == 1 |
                con_ring3 == 1 |
                con_ring4 == 1 ~ 1,
                con_ring5 == 1 ~ 0
            )
        )

    housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0

    # return
    return(housing_data)
}

wk_prep_dist_3km <- prep_est_distances(wk_housing, control_distance = 3)
wk_prep_dist_75km <- prep_est_distances(wk_housing, control_distance = 7.5)
wk_prep_dist_10km <- prep_est_distances(wk_housing, control_distance = 10)

################################################################
# Restriction POP and HH                                       #
################################################################

#----------------------------------------------
# preparation

# merge to housing data
wk_grid <- merge(
    wk_prep,
    griddata,
    by = "r1_id"
)

#----------------------------------------------
# subsamples for different grid populations

# based on population
fir_qr_pop <- as.numeric(
    quantile(wk_grid$r1_ewa_a_gesamt, probs = 0.1, na.rm = TRUE)
)
fir_qr_pop

wk_grid_pop <- wk_grid |>
    filter(r1_ewa_a_gesamt >= fir_qr_pop)

# based on households
fir_qr_hh <- as.numeric(
    quantile(wk_grid$r1_mba_a_haushalt, probs = 0.1, na.rm = TRUE)
)
fir_qr_hh

wk_grid_hh <- wk_grid |>
    filter(r1_mba_a_haushalt >= fir_qr_hh)

#----------------------------------------------
# estimation

wk_base_est_pop <- est_fm(df = wk_grid_pop, fm = wk_base)
wk_base_est_hh <- est_fm(df = wk_grid_hh, fm = wk_base)

# display results
etable(
    wk_base_est_pop, wk_base_est_hh,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero",
    headers = c("pop restr", "hh restrc")
)

################################################################
# Placebo treatment                                            #
################################################################

#----------------------------------------------
# preparation

wk_placebo <- wk_prep |>
    # restrict data to before March 2020
    filter(
        year_mon_end < "2020-03"
    ) |>
    # define placebo treatment
    mutate(
        placebo_lockdown = case_when(
            year_mon_end >= "2019-03" ~ 1,
            TRUE ~ 0
        )
    ) |>
    # drop March 2019 (as in the "real" setting)
    filter(
        year_mon_end != "2019-03"
    )

#----------------------------------------------
# rerun baseline regression

# function
reg_base_allring_lockdown <- function(dep_var){
    fm <- formula(
        paste(dep_var," ~", 
        paste(controls, collapse = " + "), 
        paste("+ placebo_lockdown * con_ring0"), 
        "| months + r1_id"))
    return(fm)
}

# apply function and get formulas
wk_placebo_est <- reg_base_allring_lockdown(dep_var = "ln_flatprice")

# run regression
mod_wk_placebo <- est_fm(wk_placebo, wk_placebo_est)

# display results
etable(
    mod_wk_placebo,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

################################################################
# Combined export                                              #
################################################################

# export table I
esttex(
    mod_wk_placebo, wk_base_trend, wk_base_est_pop, wk_base_est_hh,
    file = file.path(output_path, "regressions/robust_set_1.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("placebo", "trend", "pop_restr", "hh_restr"),
    drop = "months"
)

# export table II
esttex(
    wk_base_nz, wk_base_est_250mgrid, wk_base_est_500mgrid, wk_base_est_5kmgrid,
    file = file.path(output_path, "regressions/robust_set_2.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("without NZ", "grid FE 250m", "grid FE 500m", "grid FE 5km")
)

################################################################
# airports locations as control definition                     #
################################################################

#----------------------------------------------
# preparation

# drop those that are not main airports in the analysis
main_airports <- airport_locations |>
    filter(
        icao == "EDDS" |
        icao == "EDDN" |
        icao == "EDDL" |
        icao == "EDDV" |
        icao == "EDDP" |
        icao == "EDDF" |
        icao == "EDDK" |
        icao == "EDDH" |
        icao == "EDDM"
    )

# drop Berlin from contour data
haupt_contour <- haupt_contour |>
    filter(
        icao != "EDDT" & icao != "EDDB"
    )

# form union of contour
airport_unions <- haupt_contour |>
    group_by(icao) |>
    summarise(geometry = st_union(geometry))

# function that calculates the max, mean, or median distance between 
# airport building and the contour border
# and then buffers based on this distance around airport building
dist_func <- function(airport_icao, dist_measure){
    # turn border of the contour into points
    bord <- st_cast(
        airport_unions[airport_unions$icao == airport_icao, ],
        to = "POINT"
    )

    # calculate max, mean, or median distance between the airport building
    # and the contour border
    if(dist_measure == "MAX"){
        max_dist <- as.numeric(
            max(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ])
            )
        )
        main_airports_dist <- st_buffer(
            main_airports[main_airports$icao == airport_icao, ],
            dist = max_dist
        )
    } else if(dist_measure == "MEAN"){
        mean_dist <- mean(
            as.numeric(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ]
                )
            )
        )
        main_airports_dist <- st_buffer(
            main_airports[main_airports$icao == airport_icao, ],
            dist = mean_dist
        )
    } else {
        median_dist <- median(
            as.numeric(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ]
                )
            )
        )
        main_airports_dist <- st_buffer(
            main_airports[main_airports$icao == airport_icao, ],
            dist = median_dist
        )
    }

    # return
    return(main_airports_dist)
}

# function that returns the values of calculated distances
dist_func_values <- function(airport_icao, dist_measure){
    # turn border of the contour into points
    bord <- st_cast(
        airport_unions[airport_unions$icao == airport_icao, ],
        to = "POINT"
    )

    # calculate max, mean, or median distance between the airport building and the contour border
    if(dist_measure == "MAX"){
        dist <- as.numeric(
            max(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ]
                )
            )
        )
    } else if(dist_measure == "MEAN"){
        dist <- mean(
            as.numeric(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ]
                )
            )
        )
    } else {
        dist <- median(
            as.numeric(
                st_distance(
                    bord,
                    main_airports[main_airports$icao == airport_icao, ]
                )
            )
        )
    }
    
    # return
    return(dist)
}

# define vector with all airport ICAOs
airport_list <- unique(airport_unions$icao)

# get distances
dist_mean <- data.frame()
dist_max <- data.frame()
dist_median <- data.frame()

for(airport in airport_list){
    # mean
    output_mean <- dist_func_values(
        airport_icao = airport, dist_measure = "MEAN"
    )
    dist_mean <- rbind(dist_mean, output_mean)
    
    # max
    output_max <- dist_func_values(
        airport_icao = airport, dist_measure = "MAX"
    )
    dist_max <- rbind(dist_max, output_max)

    # median
    output_median <- dist_func_values(
        airport_icao = airport, dist_measure = "MEDIAN"
    )
    dist_median <- rbind(dist_median, output_median)
}

dist <- cbind(airport_list, dist_mean, dist_median, dist_max)
names(dist) <- c("airports", "mean", "median", "max")

openxlsx::write.xlsx(
    dist,
    file.path(
        output_path,
        "descriptives/robust_dynamic_distances_airports.xlsx"
    ),
    rowNames = FALSE
)

#----------------------------------------------
# define buffer

# define buffer based on simple distance
main_airports_5km <- st_buffer(main_airports, dist = 5000)
main_airports_10km <- st_buffer(main_airports, dist = 10000)
main_airports_15km <- st_buffer(main_airports, dist = 15000)
main_airports_20km <- st_buffer(main_airports, dist = 20000)

# define buffer based on distance given by contour shape
# create empty data frames
main_airports_max <- data.frame()
main_airports_mean <- data.frame()
main_airports_median <- data.frame()

# loop over all airports for all distances
for(airport in airport_list){
    # mean
    output_mean <- dist_func(airport_icao = airport, dist_measure = "MEAN")
    main_airports_mean <- rbind(main_airports_mean, output_mean)
    
    # max
    output_max <- dist_func(airport_icao = airport, dist_measure = "MAX")
    main_airports_max <- rbind(main_airports_max, output_max)
    
    # median
    output_median <- dist_func(airport_icao = airport, dist_measure = "MEDIAN")
    main_airports_median <- rbind(main_airports_median, output_median)
}

#----------------------------------------------
# plot based on static distances

# to define plot box merge largest polygons
# plot it invisible
plot_union_static <- st_union(
    main_airports_20km[main_airports_20km$icao == "EDDF", ], 
    haupt_contour[haupt_contour$icao == "EDDF", ]
)

# plot
static_distance_plot <- tm_shape(plot_union_static)+
    tm_polygons(alpha = 0)+
    tm_shape(main_airports_20km[main_airports_20km$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.1)+
    tm_shape(main_airports_15km[main_airports_15km$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.2)+
    tm_shape(main_airports_10km[main_airports_10km$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.3)+
    tm_shape(main_airports_5km[main_airports_5km$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.5)+
    tm_shape(haupt_contour[haupt_contour$icao == "EDDF", ])+
    tm_polygons(col = "darkorange2")+
    tm_shape(main_airports[main_airports$icao == "EDDF", ])+
    tm_dots(col = "black", size = 1)+
    tm_layout(frame = FALSE)

tmap_save(
    static_distance_plot,
    file.path(
        output_path,
        "graphs/robust_static_dist_plot.png"
    )
)

#----------------------------------------------
# plot based on dynamic distances
# to define plot box merge largest polygons
# plot it invisible
plot_union_dynamic <- st_union(
    main_airports_max[main_airports_max$icao == "EDDF", ],
    haupt_contour[haupt_contour$icao == "EDDF", ]
)

dynamic_distance_plot <- tm_shape(plot_union_dynamic)+
    tm_polygons(alpha = 0)+
    tm_shape(main_airports_max[main_airports_max$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.1)+
    tm_shape(main_airports_mean[main_airports_mean$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.2)+
    tm_shape(main_airports_median[main_airports_median$icao == "EDDF", ])+
    tm_polygons(col = "royalblue2", alpha = 0.4)+
    tm_shape(haupt_contour[haupt_contour$icao == "EDDF", ])+
    tm_polygons(col = "darkorange2")+
    tm_shape(main_airports[main_airports$icao == "EDDF", ])+
    tm_dots(col = "black", size = 1)+
    tm_layout(frame = FALSE)

tmap_save(
    dynamic_distance_plot,
    file.path(
        output_path,
        "graphs/robust_dynamic_dist_plot.png"
    )
)

#----------------------------------------------
# define treated and control

# make housing data sf
wk_housing_sf <- st_as_sf(wk_housing, coords = c("lon_gps", "lat_gps"), crs = 4326)
wk_housing_sf <- st_transform(wk_housing_sf, crs = 32632)

# intersections
int_dist <- function(buffer_df, namebuffer){
    # intersection between housing data and buffer
    inter <- st_intersects(wk_housing_sf, buffer_df)
    
    # add to housing data
    wk_housing_sf$inter_aux <- lengths(inter)
    
    # rename
    names(wk_housing_sf)[names(wk_housing_sf) == "inter_aux"] <- paste0("inter_", namebuffer)
    
    # return
    return(wk_housing_sf)
}

# apply intersection: static distances
wk_distances_5km <- int_dist(buffer_df = main_airports_5km, namebuffer = "5km")
wk_distances_10km <- int_dist(buffer_df = main_airports_10km, namebuffer = "10km")
wk_distances_15km <- int_dist(buffer_df = main_airports_15km, namebuffer = "15km")
wk_distances_20km <- int_dist(buffer_df = main_airports_20km, namebuffer = "20km")

# apply intersection: dynamic distances
wk_distances_mean <- int_dist(buffer_df = main_airports_mean, namebuffer = "mean")
wk_distances_max <- int_dist(buffer_df = main_airports_max, namebuffer = "max")
wk_distances_median <- int_dist(buffer_df = main_airports_median, namebuffer = "median")

#----------------------------------------------
# prep estimation

prep_est_dist <- function(housing_data, dist_buffer){
    # drop geometry
    housing_data <- st_drop_geometry(housing_data)

    # make "months" factor variable
    housing_data$months <- as.factor(housing_data$year_mon_end)

    # restrict sample
    housing_data <- housing_data |>
        # exlcude 1km buffer
        filter(distance_main_airports >= 1 | distance_main_airports == 0) |>
        # drop March 2020
        filter(year_mon_end != "2020-03") |>
        # drop Airports Tegel and Schoenefeld
        filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB") |>
        # select control range
        filter({{dist_buffer}} == 1 | con_ring0 == 1)
    
    # add ring for 60dB and above
    housing_data <- housing_data |>
        mutate(
            con_ring8 = case_when(
                con_ring1 == 1 |
                con_ring2 == 1 |
                con_ring3 == 1 |
                con_ring4 == 1 ~ 1,
                con_ring5 == 1 ~ 0
            )
        )
    
    housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0
    
    # return
    return(housing_data)
}

# prep static distances
wk_distances_5km <- prep_est_dist(wk_distances_5km, dist_buffer = inter_5km)
wk_distances_10km <- prep_est_dist(wk_distances_10km, dist_buffer = inter_10km)
wk_distances_15km <- prep_est_dist(wk_distances_15km, dist_buffer = inter_15km)
wk_distances_20km <- prep_est_dist(wk_distances_20km, dist_buffer = inter_20km)

# prep dynamic distances
wk_distances_mean <- prep_est_dist(wk_distances_mean, dist_buffer = inter_mean)
wk_distances_max <- prep_est_dist(wk_distances_max, dist_buffer = inter_max)
wk_distances_median <- prep_est_dist(wk_distances_median, dist_buffer = inter_median)

#----------------------------------------------
# estimation static distances

wk_base_est_5km <- est_fm(df = wk_distances_5km, fm = wk_base)
wk_base_est_10km <- est_fm(df = wk_distances_10km, fm = wk_base)
wk_base_est_15km <- est_fm(df = wk_distances_15km, fm = wk_base)
wk_base_est_20km <- est_fm(df = wk_distances_20km, fm = wk_base)

# display results
etable(
    wk_base_est_5km, wk_base_est_10km, wk_base_est_15km, wk_base_est_20km,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero",
    headers = c("5km circle", "10k circle", "15km circle", "20km circle")
)

# export
esttex(
    wk_base_est_5km, wk_base_est_10km, wk_base_est_15km, wk_base_est_20km, 
    file = file.path(
        output_path,
        "regressions/robust_static_distances_control.tex"
    ),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("5km circle", "10k circle", "15km circle", "20km circle")
)

#----------------------------------------------
# estimation dynamic distances

wk_base_est_meandist <- est_fm(df = wk_distances_mean, fm = wk_base)
wk_base_est_maxdist <- est_fm(df = wk_distances_max, fm = wk_base)
wk_base_est_mediandist <- est_fm(df = wk_distances_median, fm = wk_base)

# display results
etable(
    wk_base_est_meandist, wk_base_est_mediandist, wk_base_est_maxdist,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero",
    headers = c("mean circle", "median circle", "max circle")
)

# export
esttex(
    wk_base_est_meandist, wk_base_est_mediandist, wk_base_est_maxdist,
    file = file.path(
        output_path,
        "regressions/robust_dynamic_distances_control.tex"
    ),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("mean circle", "median circle", "max circle")
)
