################################################################
# Load Data                                                    #
################################################################

wk_housing <- read_housing(filename = "WK_complete")

################################################################
# Preparation                                                  #
################################################################

# restrict to time horizon
wk_housing <- wk_housing |>
    filter(year_mon_end <= time_horizon)

# prepare for estimation
wk_housing <- prep_est(wk_housing)

# fix that there are missings
wk_housing <- wk_housing |>
    group_by(con_ring0) |>
    mutate(
        zimmeranzahl = case_when(
            is.na(zimmeranzahl) ~ median(zimmeranzahl, na.rm = TRUE),
            TRUE ~ zimmeranzahl
        )
    ) |>
    ungroup()

#----------------------------------------------
# specific controls

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

#----------------------------------------------
# function to define estimation formula

reg_base_allring_lockdown <- function(dep_var, cnt, basemod = FALSE) {
    if (basemod == FALSE) {
        fm <- formula(
            paste(dep_var," ~",
                paste(cnt, collapse = " + ")
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

#----------------------------------------------
# generate propensity scores

model_treated <- glm(
    formula = reg_base_allring_lockdown(dep_var = "con_ring0", cnt = controls, basemod = FALSE),
    data = wk_housing,
    family = binomial(link = "logit")
)

# store findings
model_treated_out <- broom::tidy(model_treated, exponentiate = TRUE) |>
    as.data.frame()

write.xlsx(
    model_treated_out,
    file.path(
        output_path,
        "regressions",
        "propensity_scores_treatment.xlsx"
    ),
    rowNames = FALSE
)

# take model and plug in the original data (while keeping all columns even
# though not in the model)
con_ring0_ipw <- wk_housing |>
    mutate(
        propensity_nonstabilized = case_when(
            con_ring0 == 0 ~ 1 - predict(model_treated, type = "response"),
            TRUE ~ predict(model_treated, type = "response")
        ),
        ipw_weights_nonstabilized = 1 / propensity_nonstabilized
    )


#----------------------------------------------
# rerun baseline regression but now with IPW weights

est_mod_weighted <- feols(
    fml = reg_base_allring_lockdown(dep_var = "ln_flatprice", cnt = controls, basemod = TRUE),
    weights = ~ ipw_weights_nonstabilized,
    data = con_ring0_ipw
)

etable(
    est_mod_weighted,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero"
)

# export
esttex(
    est_mod_weighted,
    file = file.path(output_path, "regressions/base_wk_weighted_IPW.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)
