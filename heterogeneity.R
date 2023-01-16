################################################################
# Load Data                                                    #
################################################################

# read function
read_housing <- function(filename) {
    fln <- paste0(filename, ".qs")
    dta <- qs::qread(
        file.path(
            data_path, "housing", fln
        )
    )
    return(dta)
}

wk_housing <- read_housing(filename = "WK_complete")
hk_housing <- read_housing(filename = "HK_complete")
wm_housing <- read_housing(filename = "WM_complete")

################################################################
# Preparation                                                  #
################################################################

# preparation function
# slightly different from general one because now with option to include
# lockdown
# also add periods definition

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

wk_prep <- prep_est_march(wk_housing)
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

################################################################
# Pretrends and temporal dynamics                              #
################################################################

wk_trend_temporal_est <- feols(
    ln_flatprice ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
        etage + etageUNBEKANNT + balkon + balkonUNBEKANNT + zimmeranzahl +
        as.factor(objektzustand) + objektzustandUNBEKANNT + einbaukueche +
        einbaukuecheUNBEKANNT + garten + gartenUNBEKANNT + as.factor(heizungsart) + 
        heizungsartUNBEKANNT + as.factor(ausstattung) + ausstattungUNBEKANNT + 
        badezimmer + badezimmerUNBEKANNT + distance_largcenter + distance_medcenter + 
        distance_smalcenter + distance_all_airports_building + distance_industry + 
        distance_railroads + distance_streets + con_ring0 +
        i(con_ring0, factor_var = periods, ref = "t"),
        data = wk_prep, fixef = c("months", "r1_id"), se = "hetero"
)

etable(
    wk_trend_temporal_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

# export
esttex(
    wk_trend_temporal_est,
    file = file.path(output_path, "regressions/hetero_temporal_dynamics.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

#----------------------------------------------
# make interval graph

# extract coefficients and standard errors
coef <- as.data.frame(
    cbind(
        summary(wk_trend_temporal_est)$coefficients,
        summary(wk_trend_temporal_est)$se
    )
)

# get variable names from rows
coef$vars <- rownames(coef)

# set names for data frame
names(coef) <- c("coef", "se", "vars")
rownames(coef) <- seq(1, nrow(coef), 1)

# keep only period estimates
coef_interest <- coef |>
    filter(stringr::str_detect(vars, "period") == TRUE)

# add reference group (t)
coef_interest <- rbind(
    coef_interest,
    as.data.frame(cbind(coef = 0, se = 0, vars = "periods::t:con_ring0")))

# confidence interval threshold
conf <- 1.64

# define period names
coef_interest <- coef_interest |>
    mutate(
        period = stringr::str_replace(
            vars,
            pattern = "periods::",
            replacement = ""
        ),
        period = stringr::str_replace(
            period,
            pattern = ":con_ring0",
            replacement = ""
        ),
        # set period as factor
        period = factor(
            period,
            levels = c(
                "t-4", "t-3", "t-2", "t-1", "t", "t+1", "t+2", "t+3",
                "t+4", "t+5", "t+6", "t+7", "t+8", "t+9"
            )
        ),
        # define types
        coef = as.numeric(coef),
        se = as.numeric(se),
        # define confidence interval
        conf_min = coef - (conf * se),
        conf_max = coef + (conf * se)
    )

# plot
temporal_dyn <- ggplot(
    data = coef_interest,
    mapping = aes(x = period, y = coef)
)+
geom_point(size = 2.4)+
geom_pointrange(
    mapping = aes(x = period, ymin = conf_min, ymax = conf_max),
    position = position_dodge(width = 0.4),
    size = 0.6
)+
geom_hline(yintercept = 0)+
geom_vline(xintercept = "t")+
scale_y_continuous(
    breaks = seq(-0.04, 0.08, 0.02)
)+
labs(
    x = "",
    y = "Coefficients and 90% CI"
)+
theme(
    panel.background = element_blank(),
    panel.border = element_rect(size = 1, fill = NA),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 17),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.length = unit(units = "cm", 0.2)
)

# export
ggsave(
    plot = temporal_dyn,
    file.path(
        output_path, "graphs/timesplit_plot.png"
    )
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
    digits = "r3", replace = TRUE, dict = tablabel_char,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("house sales", "apart rent")
)

################################################################
# Noise intensities                                            #
################################################################
# i.e. differnt noise rings

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

# define formula function
reg_spacesplit <- function(dep_var){
    fm_rings_complete <- formula(
        paste(dep_var, " ~", 
        paste(controls, collapse = " + "), 
        paste(
            "+ fir_lockdown * con_ring8 + fir_lockdown * con_ring5"
        ),
        "| months + r1_id"))
    return(fm_rings_complete)
}

# specify formulas
wk_spacesplit <- reg_spacesplit(dep_var = "ln_flatprice")

# run regression
wk_spacesplit_est <- est_fm(df = wk_prep, fm = wk_spacesplit)

# display results
etable(
    wk_spacesplit_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3",
    se = "hetero"
)

# export
esttex(
    wk_spacesplit_est,
    file = file.path(output_path, "regressions/hetero_noise_intensities_wk.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

################################################################
# pricey vs. non-pricey neighborhoods                          #
################################################################
# define pricey regions by grid average

#----------------------------------------------
# preparation

# consider only pre-Covid periods
grid_prices <- wk_prep |>
    select(r1_id, price_sqmeter, year_mon_end, closest_main_airports, con_ring0) |>
    filter(year_mon_end >= "2018-01" & year_mon_end <= "2019-12")

# calculate average price on grid level
price_summary <- grid_prices |>
    group_by(r1_id) |>
    summarise(
        mean_price_sqmeter = mean(price_sqmeter, na.rm = TRUE)
    ) |>
    as.data.frame()

# get airports for grids
grid_airport <- grid_prices |>
    select(r1_id, con_ring0, closest_main_airports)
grid_airport <- grid_airport |>
    group_by(r1_id) |>
    summarise(
        mean_group = mean(con_ring0, na.rm = TRUE),
        airport = first(closest_main_airports)
    ) |>
    as.data.frame()

# merge grid prices and grid airports
price_summary <- merge(
    price_summary,
    grid_airport,
    by = "r1_id"
)

# adjust treated group
price_summary <- price_summary |>
    mutate(
        mean_group = case_when(
            mean_group != 1 & mean_group != 0 ~ 0,
            TRUE ~ mean_group
        )
    )

# calculate terciles for each airport separately and treated group
price_terciles <- price_summary |>
    group_by(airport, mean_group) |>
    summarise(
        quant = sort(
            as.numeric(
                quantile(mean_price_sqmeter, probs = seq(0, 1, 1/3), na.rm = TRUE)
            )
        )
    ) |>
    as.data.frame()

# assign categories
price_terciles$cat <- rep(c("min", "onethird", "twothird", "max"), 18)

# export 
openxlsx::write.xlsx(
    price_terciles,
    file.path(
        output_path,
        "descriptives/price_terciles_airports.xlsx"
    ),
    rowName = FALSE
)

# get list of airports
list_airports <- unique(wk_prep$closest_main_airports)

# loop over group, areas, and airports for grid data
for(group in c(0, 1)){
    for(areas in c("low", "medium", "high")){
        for(airport in list_airports){
        if(areas == "low"){
            price_summary$cat[
                price_summary$mean_price_sqmeter <= price_terciles[
                    price_terciles$mean_group == group & price_terciles$airport == airport & price_terciles$cat == "onethird",
                    ]$quant & price_summary$airport == airport] <- "low"
        } else if(areas == "medium"){
            price_summary$cat[
                price_summary$mean_price_sqmeter > price_terciles[
                    price_terciles$mean_group == group & price_terciles$airport == airport & price_terciles$cat == "onethird",
                    ]$quant & price_summary$mean_price_sqmeter <= price_terciles[
                        price_terciles$mean_group == group & price_terciles$airport == airport & price_terciles$cat == "twothird",
                        ]$quant & price_summary$airport == airport] <- "medium"
        } else {
            price_summary$cat[
                price_summary$mean_price_sqmeter > price_terciles[
                    price_terciles$mean_group == group & price_terciles$airport == airport & price_terciles$cat == "twothird",
                    ]$quant & price_summary$airport == airport] <- "high"
        }
        }
    }
}

# keep variables of interest
price_summary_prep <- price_summary |>
    select(r1_id, cat)

# merge back to original data set
wk_prep_grid <- merge(wk_prep, price_summary_prep, by = "r1_id")

#----------------------------------------------
# subset

wk_high_price <- wk_prep_grid |> filter(cat == "high")
wk_medium_price <- wk_prep_grid |> filter(cat == "medium")
wk_low_price <- wk_prep_grid |> filter(cat == "low")

#----------------------------------------------
# estimation

# specify formula
wk_base <- reg_base_allring_lockdown(dep_var = "ln_flatprice", cnt = controls)

# run regression
wk_high_price_est <- est_fm(df = wk_high_price, fm = wk_base)
wk_medium_price_est <- est_fm(df = wk_medium_price, fm = wk_base)
wk_low_price_est <- est_fm(df = wk_low_price, fm = wk_base)

# display results
etable(
    wk_high_price_est, wk_medium_price_est, wk_low_price_est, 
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero",
    headers = c("high price", "medium price", "low price")
)

esttex(
    wk_high_price_est, wk_medium_price_est, wk_low_price_est,
    file = file.path(output_path, "regressions/hetero_pricey_neighborhoods_wk.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    headers = c("high price", "medium price", "low price")
)
