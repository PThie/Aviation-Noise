################################################################
# Description                                                  #
################################################################

# This function performes the baseline estimation for the observations in 2023
# and testing their impact on the baseline results. Immo registered in 2023
# more observations than in previous years.

################################################################
# Load Data                                                    #
################################################################

wk_housing <- read_housing(filename = "WK_complete")

################################################################
# preparation                                                  #
################################################################

#----------------------------------------------
# apply prep function

wk_housing <- prep_est(wk_housing)

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

################################################################
# estimation functions                                         #
################################################################
# baseline setting
# key variable: interaction lockdown x all rings together (larger equal 55dB)

# define formula
reg_base_allring_lockdown <- function(dep_var, cnt) {
    #' @param dep_var Dependent variable (LHS)
    #' @param cnt Controls

    fm <- formula(
        paste(dep_var," ~",
            paste(cnt, collapse = " + "),
            # coefficient of interest (interaction term)
            paste("+ fir_lockdown * con_ring0"),
            "| months + r1_id"
        )
    )

    # return results
    return(fm)
}

# define estimation function
est_fm <- function(df, dependent, contr){
    feols(fml = reg_base_allring_lockdown(dep_var = dependent, cnt = contr), data = df)
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
# estimation                                                   #
################################################################

#----------------------------------------------
# Leave one out estimation for months in 2023

# define months to leave out
leave_out_months <- c(paste0("2023-0", seq(1, 6)))

# storage for results
results_storage <- list()

# perform estimation
for (month in leave_out_months) {
    # drop respective month
    housing_data <- wk_housing %>%
        filter(year_mon_end != month)

    # perform baseline estimation
    wk_base_est <- est_fm(df = housing_data, dependent = "ln_flatprice", contr = controls)

    # store output
    results_storage[[month]] <- wk_base_est
}

#----------------------------------------------
# leave out entire 2023

housing_data_wo_2023 <- wk_housing |>
    filter(ejahr <= 2022)

# perform baseline estimation
wk_base_est <- est_fm(df = housing_data_wo_2023, dependent = "ln_flatprice", contr = controls)

# add to results storage
results_storage[["wo_2023"]] <- wk_base_est

#----------------------------------------------
# extract coefficients

get_coefficients <- function(list_name) {
    # get estimates
    est <- results_storage[[list_name]]

    # extract coefficients
    coeffs <- as.data.frame(cbind(coefficient = est$coefficient))

    # redefine row names
    coeffs$variable <- row.names(coeffs)
    row.names(coeffs) <- seq(1, nrow(coeffs))

    # make sure that only interaction is left
    coeffs <- coeffs |>
        filter(str_detect(variable, "fir_lockdown:con_ring0") == TRUE)

    # calculate confidence interval
    con <- confint(est, level = 0.90) |>
        as.data.frame()
    
    # redefine row names
    con$variable <- row.names(con)
    row.names(con) <- seq(1, nrow(con))
    
    con <- con |>
        filter(str_detect(variable, "fir_lockdown:con_ring0") == TRUE)

    # add CI to data
    coeffs <- coeffs |>
        mutate(
            lower_ci = con[, 1],
            upper_ci = con[, 2],
            # add fuel type
            mod = list_name
        ) |>
        # remove variable
        select(-variable)

    return(coeffs)
}

# list for storage
coeff_list <- list()

# extract all coefficients of interest
for (name in names(results_storage)) {
    coeffs <- get_coefficients(name)
    coeff_list[[name]] <- coeffs
}

# bind all information together
coefficients <- do.call(rbind, coeff_list)

#----------------------------------------------
# generate plot

est_plot_2023 <- ggplot(
    data = coefficients,
    mapping = aes(x = factor(mod), y = coefficient)
)+
    geom_pointrange(
        mapping = aes(ymin = lower_ci, ymax = upper_ci),
        linewidth = 1, size = 0.5
    )+
    scale_x_discrete(
        labels = c(
            "wo_2023" = "w/o 2023",
            "2023-01" = "w/o 2023-Jan",
            "2023-02" = "w/o 2023-Feb",
            "2023-03" = "w/o 2023-Mar",
            "2023-04" = "w/o 2023-Apr",
            "2023-05" = "w/o 2023-May",
            "2023-06" = "w/o 2023-Jun"
        )
    )+
    geom_hline(yintercept = 0)+
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
    plot = est_plot_2023,
    file.path(
        output_path,
        "graphs/estimations_looe_2023.png"
    )
)
