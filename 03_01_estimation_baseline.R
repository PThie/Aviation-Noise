################################################################
# Description                                                  #
################################################################

# This function performes the baseline estimation for all housing types.

################################################################
# Load Data                                                    #
################################################################

wk_housing <- read_housing(filename = "WK_complete")
hk_housing <- read_housing(filename = "HK_complete")
wm_housing <- read_housing(filename = "WM_complete")

################################################################
# preparation                                                  #
################################################################

#----------------------------------------------
# apply prep function

wk_housing <- prep_est(wk_housing)
hk_housing <- prep_est(hk_housing)
wm_housing <- prep_est(wm_housing)

# without restriction
wk_housing_complete <- wk_housing

# restrict data to the maximum time frame
wk_housing <- wk_housing |>
    filter(year_mon_end <= time_horizon)

hk_housing <- hk_housing |>
    filter(year_mon_end <= time_horizon)

wm_housing <- wm_housing |>
    filter(year_mon_end <= time_horizon)

#----------------------------------------------
# define housing characteristics (controls)

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

controls_with_time <- c(
    "alter * months", "alter_squ * months", "wohnflaeche * months", "wohnflaeche_squ * months", 
    "etage * months", "etageUNBEKANNT * months", "balkon * months", "balkonUNBEKANNT * months",
    "as.factor(objektzustand) * months", "objektzustandUNBEKANNT * months", "zimmeranzahl * months",
    "einbaukueche * months", "einbaukuecheUNBEKANNT * months", "garten * months", "gartenUNBEKANNT * months",
    "as.factor(heizungsart) * months", "heizungsartUNBEKANNT * months", "as.factor(ausstattung) * months", "ausstattungUNBEKANNT * months",
    "badezimmer * months", "badezimmerUNBEKANNT * months",
    "distance_largcenter * months", "distance_medcenter * months", "distance_smalcenter * months", 
    "distance_industry * months", "distance_railroads * months", "distance_streets * months",
    "distance_all_airports_building * months"
)

#----------------------------------------------
# for apartments add factor variable of airports

wk_housing <- wk_housing |>
    mutate(
        airportFE = as.factor(closest_main_airports),
        plzFE = as.factor(plz)
    )

wk_housing_complete <- wk_housing_complete |>
    mutate(
        airportFE = as.factor(closest_main_airports),
        plzFE = as.factor(plz)
    )

################################################################
# estimation functions                                         #
################################################################
# baseline setting
# key variable: interaction lockdown x all rings together (larger equal 55dB)

# define formula
reg_base_allring_lockdown <- function(dep_var, cnt, fixef = c("none", "time", "region", "both")) {
    #' @param dep_var Dependent variable (LHS)
    #' @param cnt Controls
    #' @param fixef Indicator for fixed effects
    
    if(fixef == "none") {
        fm <- formula(
            paste(dep_var," ~",
                paste(cnt, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0")
            )
        )
    } else if (fixef == "time") {
        fm <- formula(
            paste(dep_var," ~",
                paste(cnt, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months"
            )
        )
    } else if (fixef == "region") {
        fm <- formula(
            paste(dep_var," ~",
                paste(cnt, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| r1_id"
        )
    )
    } else {
        fm <- formula(
            paste(dep_var," ~",
                paste(cnt, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months + r1_id"
            )
        )
    }

    # return results
    return(fm)
}

# define different formula including an unconditional estimation
reg_base_unconditional <- function(dep_var, incl_controls = c("yes", "no"), other_fe = c("airport", "zipcode")) {
    # leave out controls (unconditional regression) or not
    if(incl_controls == "yes") {
        fm <- formula(
            paste(dep_var, " ~",
                paste(controls, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months + r1_id"
            )
        )
    } else {
        fm <- formula(
            paste(dep_var, " ~",
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months + r1_id"
            )
        )
    }

    # using airport FE instead of grid FE
    if(other_fe == "airport"){
        fm <- formula(
            paste(dep_var, " ~",
                paste(controls, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months + airportFE"
            )
        )
    } else if(other_fe == "zipcode"){
        fm <- formula(
            paste(dep_var, " ~",
                paste(controls, collapse = " + "),
                # coefficient of interest (interaction term)
                paste("+ fir_lockdown * con_ring0"),
                "| months + plzFE"
            )
        )
    }

    # return results
    return(fm)
}

# define estimation function
est_fm <- function(df, dependent, contr, FE){
    feols(fml = reg_base_allring_lockdown(dep_var = dependent, cnt = contr, fixef = FE), data = df)
}

est_fm_uncond <- function(df, dependent, contr, FE) {
    feols(fml = reg_base_unconditional(dep_var = dependent, incl_controls = contr, other_fe = FE), data = df)
}

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

################################################################
# baseline apartment prices                                    #
################################################################

# run regression
wk_base_est_ols <- est_fm(df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "none")
wk_base_est_timeFE <- est_fm(df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "time")
wk_base_est_regionFE <- est_fm(df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "region")
wk_base_est_bothFE <- est_fm(df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "both")

wk_base_est_unconditional <- est_fm_uncond(df = wk_housing, dependent = "ln_flatprice", contr = "no", FE = "no")
wk_base_est_airportFE <- est_fm_uncond(df = wk_housing, dependent = "ln_flatprice", contr = "yes", FE = "airport")
wk_base_est_zipcodeFE <- est_fm_uncond(df = wk_housing, dependent = "ln_flatprice", contr = "yes", FE = "zipcode")
wk_base_est_standard <- est_fm_uncond(df = wk_housing, dependent = "ln_flatprice", contr = "yes", FE = "no")

# FE with complete time horizon
wk_base_est_bothFE_complete <- est_fm(df = wk_housing_complete, dependent = "ln_flatprice", contr = controls, FE = "both")

# display results
etable(
    wk_base_est_ols, wk_base_est_timeFE, wk_base_est_regionFE, wk_base_est_bothFE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

etable(
    wk_base_est_bothFE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero", drop = "months"
)

# export
esttex(
    wk_base_est_ols, wk_base_est_timeFE, wk_base_est_regionFE, wk_base_est_bothFE,
    file = file.path(output_path, "regressions/base_wk.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

esttex(
    wk_base_est_unconditional, wk_base_est_airportFE, wk_base_est_standard,
    file = file.path(output_path, "regressions/base_wk_unconditional.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

esttex(
    wk_base_est_zipcodeFE,
    file = file.path(output_path, "regressions/base_wk_zipcodeFE.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

esttex(
    wk_base_est_bothFE_complete,
    file = file.path(output_path, "regressions/base_wk_complete_horizon.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

################################################################
# baseline apartment prices with time interaction              #
################################################################

# run regression
wk_base_est_time_interaction <- est_fm(
    df = wk_housing,
    dependent = "ln_flatprice",
    contr = controls_with_time,
    FE = "both"
)

# print results
etable(
    wk_base_est_time_interaction,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero",
    drop = "months"
)

# export once complete and once without the month interactions
esttex(
    wk_base_est_time_interaction,
    file = file.path(output_path, "regressions/base_wk_char_interaction_complete.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

esttex(
    wk_base_est_time_interaction,
    file = file.path(output_path, "regressions/base_wk_char_interaction_short.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    drop = "months",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

################################################################
# clustering                                                   #
################################################################

# cluster on grids
est_fm_gridcluster <- function(df, dependent, contr, FE){
    feols(fml = reg_base_allring_lockdown(
            dep_var = dependent,
            cnt = contr,
            fixef = FE
        ),
        data =
        df,
        cluster = "r1_id"
    )
}

wk_base_est_gridcl <- est_fm_gridcluster(
    df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "both"
)

# cluster on municipality
est_fm_municcluster <- function(df, dependent, contr, FE){
    feols(fml = reg_base_allring_lockdown(dep_var = dependent, cnt = contr, fixef = FE), data = df, cluster = "gid2019")
}

wk_base_est_municcl <- est_fm_municcluster(
    df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "both"
)

# cluster district
est_fm_distcluster <- function(df, dependent, contr, FE){
    feols(fml = reg_base_allring_lockdown(dep_var = dependent, cnt = contr, fixef = FE), data = df, cluster = "kid2019")
}

wk_base_est_discl <- est_fm_distcluster(
    df = wk_housing, dependent = "ln_flatprice", contr = controls, FE = "both"
)

etable(
    wk_base_est_gridcl, wk_base_est_municcl, wk_base_est_discl,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    headers = c("gridclust", "municclust", "distrclust")
)

# export
esttex(
    wk_base_est_gridcl, wk_base_est_municcl, wk_base_est_discl,
    file = file.path(output_path, "regressions/base_wk_cluster.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("gridclust", "municclust", "distrclust")
)

################################################################
# baseline house prices                                        #
################################################################

# define controls
controls_hk <- c(
    "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
    "anzahletagen", "anzahletagenUNBEKANNT", "grundstuecksflaeche", "grundstuecksflaeche_squ",
    "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
    "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
    "badezimmer", "badezimmerUNBEKANNT",
    "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
    "distance_industry", "distance_railroads", "distance_streets",
    "distance_all_airports_building"
)

# run regression
hk_base_est <- est_fm(
    df = hk_housing,
    dependent = "ln_houseprice",
    contr = controls_hk,
    FE = "both"
)

# display results
etable(
    hk_base_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

################################################################
# baseline: apartment rents                                    #
################################################################

# run regression
wm_base_est <-  est_fm(
    df = wm_housing,
    dependent = "ln_rent_sqmeter",
    contr = controls,
    FE = "both"
)

# display results
etable(
    wm_base_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

################################################################
# baseline: house price & apartment prices                     #
################################################################
# combination of sales data

# column names that are in both data sets
columnnames <- intersect(names(wk_housing), names(hk_housing))

# prep function
combine_prep <- function(dataframe, type){
    # keep only the columns that are in both data sets
    dataframe_comb <- dataframe |>
    select(all_of(columnnames))

    # add type
    if(type == "HK"){
        dataframe_comb$type <- "houses"
    } else {
        dataframe_comb$type <- "apartments"
    }
    
    # calculate log price
    dataframe_comb$ln_price <- log(dataframe_comb$kaufpreis)
    
    # return
    return(dataframe_comb)
}

# apply prep function
wk_housing_comb <- combine_prep(wk_housing, type = "WK")
hk_housing_comb <- combine_prep(hk_housing, type = "HK")

# combine both data sets in one
housing_comb <- rbind(wk_housing_comb, hk_housing_comb)

# define controls new
controls_sales <- c(
    "alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
    "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
    "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
    "badezimmer", "badezimmerUNBEKANNT", "distance_railroads", "distance_industry",
    "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
    "distance_industry", "distance_railroads", "distance_streets",
    "distance_all_airports_building", "type"
)

# run regression
comb_base_est <- est_fm(
    df = housing_comb,
    dependent = "ln_price",
    contr = controls_sales,
    FE = "both"
)

# display results
etable(
    comb_base_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

# display combine
etable(
    hk_base_est,
    comb_base_est,
    wm_base_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

# export
esttex(
    hk_base_est, comb_base_est, wm_base_est,
    file = file.path(output_path, "regressions/base_hk_wk_wm_combined.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("houses", "hous_apart_sales", "apart_rent")
)
