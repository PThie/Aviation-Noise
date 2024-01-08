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
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
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
geom_vline(xintercept = "t", linetype = "twodash")+
scale_y_continuous(
    breaks = seq(-0.04, 0.08, 0.02)
)+
scale_x_discrete(
    labels = c(
        "t-4" = "Jan '18 - Aug '18",
        "t-3" = "Sep '18 - Feb '19",
        "t-2" = "Mar '19 - Aug '19",
        "t-1" = "Sep '19 - Feb '20",
        "t" = "Mar '20",
        "t+1" = "Apr '20 - Jun '20",
        "t+2" = "Jul '20 - Sep '20",
        "t+3" = "Oct '20 - Dec '20",
        "t+4" = "Jan '21 - Mar '21",
        "t+5" = "Apr '21 - Jun '21",
        "t+6" = "Jul '21 - Sep '21",
        "t+7" = "Oct '21 - Dec '21",
        "t+8" = "Jan '22 - Mar '22",
        "t+9" = "Apr '22 - Jun '22"
    )
)+
labs(
    x = "",
    y = "Point estimates and 90% CI"
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
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

################################################################
# Noise intensities and temporal dynamics                      #
################################################################

# high noise
wk_trend_temporal_noise_est <- feols(
    ln_flatprice ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
        etage + etageUNBEKANNT + balkon + balkonUNBEKANNT + zimmeranzahl +
        as.factor(objektzustand) + objektzustandUNBEKANNT + einbaukueche +
        einbaukuecheUNBEKANNT + garten + gartenUNBEKANNT + as.factor(heizungsart) + 
        heizungsartUNBEKANNT + as.factor(ausstattung) + ausstattungUNBEKANNT + 
        badezimmer + badezimmerUNBEKANNT + distance_largcenter + distance_medcenter + 
        distance_smalcenter + distance_all_airports_building + distance_industry + 
        distance_railroads + distance_streets +
        i(con_ring8, factor_var = periods, ref = "t") + 
        i(con_ring5, factor_var = periods, ref = "t"),
        data = wk_prep, fixef = c("months", "r1_id"), se = "hetero"
)

etable(
    wk_trend_temporal_noise_est,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

# export
esttex(
    wk_trend_temporal_noise_est,
    file = file.path(output_path, "regressions/hetero_temporal_dynamics_intensities.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

#----------------------------------------------
# make interval graph

# extract coefficients and standard errors
coef <- as.data.frame(
    cbind(
        summary(wk_trend_temporal_noise_est)$coefficients,
        summary(wk_trend_temporal_noise_est)$se
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
    as.data.frame(cbind(coef = 0, se = 0, vars = "periods::t:con_ring8")),
    as.data.frame(cbind(coef = 0, se = 0, vars = "periods::t:con_ring5"))
)

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
            pattern = ":con_ring8",
            replacement = ""
        ),
        period = stringr::str_replace(
            period,
            pattern = ":con_ring5",
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
        # define group
        group = case_when(
            stringr::str_detect(vars, "con_ring8") == TRUE ~ "high",
            TRUE ~ "low"
        ),
        # define types
        coef = as.numeric(coef),
        se = as.numeric(se),
        # define confidence interval
        conf_min = coef - (conf * se),
        conf_max = coef + (conf * se)
    )

#----------------------------------------------
# plot high noise
int_temporal_dyn_high <- ggplot()+
    geom_pointrange(
        data = coef_interest |> filter(group == "high"),
        mapping = aes(x = period, y = coef, ymin = conf_min, ymax = conf_max, group = factor(group)),
        position = position_dodge(width = 0.7),
        size = 0.6,
        shape = 15
    )+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = "t", linetype = "twodash")+
    scale_y_continuous(
    breaks = seq(-0.10, 0.10, 0.02)
    )+
    scale_x_discrete(
        labels = c(
            "t-4" = "Jan '18 - Aug '18",
            "t-3" = "Sep '18 - Feb '19",
            "t-2" = "Mar '19 - Aug '19",
            "t-1" = "Sep '19 - Feb '20",
            "t" = "Mar '20",
            "t+1" = "Apr '20 - Jun '20",
            "t+2" = "Jul '20 - Sep '20",
            "t+3" = "Oct '20 - Dec '20",
            "t+4" = "Jan '21 - Mar '21",
            "t+5" = "Apr '21 - Jun '21",
            "t+6" = "Jul '21 - Sep '21",
            "t+7" = "Oct '21 - Dec '21",
            "t+8" = "Jan '22 - Mar '22",
            "t+9" = "Apr '22 - Jun '22"
        )
    )+
    labs(
        x = "",
        y = "Coefficients and 90% CI"
    )+
    theme(
        panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.length = unit(units = "cm", 0.2)
    )

# export
ggsave(
    plot = int_temporal_dyn_high,
    file.path(
        output_path, "graphs/time_spaceplit_highnoise_plot.png"
    ),
    height = 10,
    width = 13
)

#----------------------------------------------
# plot low noise
int_temporal_dyn_low <- ggplot()+
    geom_pointrange(
        data = coef_interest |> filter(group == "low"),
        mapping = aes(x = period, y = coef, ymin = conf_min, ymax = conf_max, group = factor(group)),
        position = position_dodge(width = 0.7),
        size = 0.6,
        shape = 17
    )+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = "t", linetype = "twodash")+
    scale_y_continuous(
    breaks = seq(-0.10, 0.10, 0.02)
    )+
    scale_x_discrete(
        labels = c(
            "t-4" = "Jan '18 - Aug '18",
            "t-3" = "Sep '18 - Feb '19",
            "t-2" = "Mar '19 - Aug '19",
            "t-1" = "Sep '19 - Feb '20",
            "t" = "Mar '20",
            "t+1" = "Apr '20 - Jun '20",
            "t+2" = "Jul '20 - Sep '20",
            "t+3" = "Oct '20 - Dec '20",
            "t+4" = "Jan '21 - Mar '21",
            "t+5" = "Apr '21 - Jun '21",
            "t+6" = "Jul '21 - Sep '21",
            "t+7" = "Oct '21 - Dec '21",
            "t+8" = "Jan '22 - Mar '22",
            "t+9" = "Apr '22 - Jun '22"
        )
    )+
    labs(
        x = "",
        y = "Coefficients and 90% CI"
    )+
    theme(
        panel.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.length = unit(units = "cm", 0.2)
    )

# export
ggsave(
    plot = int_temporal_dyn_low,
    file.path(
        output_path, "graphs/time_spaceplit_lownoise_plot.png"
    ),
    height = 10,
    width = 13
)
