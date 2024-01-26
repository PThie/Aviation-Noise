################################################################
# Description                                                  #
################################################################

# This file generates the robustness where the control group is restricted
# based on the characteristics in the treatment group.

################################################################
# Load Data                                                    #
################################################################

wk_housing <- read_housing(filename = "WK_complete")

################################################################
# Preparation                                                  #
################################################################

wk_prep <- prep_est(wk_housing)

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
# Restrict control units to treatment characteristics          #
################################################################

#----------------------------------------------
# baseline formula function for descriptive table

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
# split data into treatment and control group
wk_prep_control <- wk_prep |>
    filter(con_ring0 == 0)

wk_prep_treated <- wk_prep |>
    filter(con_ring0 == 1)

# function for calculating the quantile for a given variable
quant_calc <- function(var, quant) {
    as.numeric(
        quantile(
            wk_prep_treated[[var]],
            probs = quant,
            na.rm = TRUE
        )
    )
}

# define quantile thresholds for consideration
lower_quantile <- 0.1
upper_quantile <- 0.9

# restrict control units to the characteristics in the treatment group
wk_prep_control_restricted <- wk_prep_control |>
    filter(kaufpreis >= quant_calc(var = "kaufpreis", lower_quantile) & kaufpreis <= quant_calc(var = "kaufpreis", upper_quantile)) |>
    filter(alter >= quant_calc(var = "alter", lower_quantile) & alter <= quant_calc(var = "alter", upper_quantile)) |>
    filter(wohnflaeche >= quant_calc(var = "wohnflaeche", lower_quantile) & wohnflaeche <= quant_calc(var = "wohnflaeche", upper_quantile)) |>
    filter(zimmeranzahl >= quant_calc(var = "zimmeranzahl", lower_quantile) & zimmeranzahl <= quant_calc(var = "zimmeranzahl", upper_quantile)) |>
    filter(heizungsart >= quant_calc(var = "heizungsart", lower_quantile) & heizungsart <= quant_calc(var = "heizungsart", upper_quantile)) |>
    filter(objektzustand >= quant_calc(var = "objektzustand", lower_quantile) & objektzustand <= quant_calc(var = "objektzustand", upper_quantile)) |>
    filter(ausstattung >= quant_calc(var = "ausstattung", lower_quantile) & ausstattung <= quant_calc(var = "ausstattung", upper_quantile)) |>
    filter(distance_largcenter >= quant_calc(var = "distance_largcenter", lower_quantile) & distance_largcenter <= quant_calc(var = "distance_largcenter", upper_quantile)) |>
    filter(distance_medcenter >= quant_calc(var = "distance_medcenter", lower_quantile) & distance_medcenter <= quant_calc(var = "distance_medcenter", upper_quantile)) |>
    filter(distance_smalcenter >= quant_calc(var = "distance_smalcenter", lower_quantile) & distance_smalcenter <= quant_calc(var = "distance_smalcenter", upper_quantile)) |>
    filter(distance_all_airports_building >= quant_calc(var = "distance_all_airports_building", lower_quantile) & distance_all_airports_building <= quant_calc(var = "distance_all_airports_building", upper_quantile)) |>
    filter(distance_industry >= quant_calc(var = "distance_industry", lower_quantile) & distance_industry <= quant_calc(var = "distance_industry", upper_quantile)) |>
    filter(distance_railroads >= quant_calc(var = "distance_railroads", lower_quantile) & distance_railroads <= quant_calc(var = "distance_railroads", upper_quantile)) |>
    filter(distance_streets >= quant_calc(var = "distance_streets", lower_quantile) & distance_streets <= quant_calc(var = "distance_streets", upper_quantile)) |>
    filter(etage >= quant_calc(var = "etage", lower_quantile) & etage <= quant_calc(var = "etage", upper_quantile)) |>
    filter(badezimmer >= quant_calc(var = "badezimmer", lower_quantile) & badezimmer <= quant_calc(var = "badezimmer", upper_quantile))

# combine unchanged treatment group and new control group data
wk_prep_new_control <- rbind(
    wk_prep_control_restricted,
    wk_prep_treated
)

#----------------------------------------------
# generate descriptive table as before

# function for subsetting data
prep_descriptives <- function(housing, price_variable){
    # select the main variables (part of regression)
    housing <- housing |>
        select(
            "ln_flatprice", "alter", "wohnflaeche", "etage", "balkon", "objektzustand",
            "einbaukueche", "garten", "heizungsart", "ausstattung", "zimmeranzahl",
            "badezimmer", "distance_largcenter", "distance_medcenter", "distance_smalcenter",
            "distance_industry", "distance_railroads", "distance_streets", "distance_main_airports_building",
            "con_ring0", "fir_lockdown",
            price_variable
        )
    # return
    return(housing)
}

# apply function
wk_des_data <- prep_descriptives(wk_prep_new_control, price_variable = "kaufpreis")

#----------------------------------------------
# descriptives by lockdown timing (i.e. before and after the lockdown)

group_descriptives <- function(housing, name){
    # calculate descriptives for before and after lockdown
    des <- describeBy(
        housing,
        group = housing$fir_lockdown,
        mat = TRUE,
        digits = 3,
        fast = TRUE,
        na.rm = TRUE
    )
    
    # drop unneeded descriptive statistics
    des$trimmed <- NULL
    des$range <- NULL
    des$max <- NULL
    des$min <- NULL
    des$se <- NULL
    des$item <- NULL
    des$n <- NULL
    des$vars <- NULL
    
    # add variable names
    des$variables <- row.names(des)

    # adjust row names
    row.names(des) <- seq(1, nrow(des), 1)

    # redefine group
    # adjust variable names
    des <- des |>
        rename(
            group = group1
        ) |>
        mutate(
            group = case_when(
                group == 0 ~ "before_lock",
                TRUE ~ "after_lock"
            ),
            variables = str_replace(
                variables,
                pattern = "[0-9]+",
                replacement = ""
            ),
            group_label = paste(variables, group, sep = "_")
        )
    
    # drop unneeded rows
    des <- des |>
        filter(str_detect(variables, "con_ring|lockdown") == FALSE)
    
    # rename mean and sd for merge
    names(des)[names(des) == "mean"] <- paste0("mean_", name)
    names(des)[names(des) == "sd"] <- paste0("sd_", name)
    
    # return
    return(des)
}

# subset for treated and control group
wk_des_treat <- wk_des_data |>
    filter(con_ring0 == 1)

wk_des_contr <- wk_des_data |>
    filter(con_ring0 == 0)

# apply descriptive function
des_treat_wk <- group_descriptives(wk_des_treat, name = "wk_treat")
des_contr_wk <- group_descriptives(wk_des_contr, name = "wk_contr")

# -------------------------------------------------------------------------
# combine

# bring both together
des_table <-  merge(des_treat_wk, des_contr_wk, by = "group_label") |>
    select(variables.x, group.x, mean_wk_treat, sd_wk_treat, mean_wk_contr, sd_wk_contr) |>
    rename(
        variables = variables.x,
        group = group.x
    )

# make wide table
des_table_wide <- des_table |> 
    select(!c(sd_wk_treat, sd_wk_contr)) |> 
    pivot_wider(
        names_from = "group",
        values_from = c("mean_wk_treat", "mean_wk_contr")
    ) |>
    as.data.frame()

#----------------------------------------------
# unconditional difference in difference

reg_uncond_did <- function(depvar){
    # make timing variable factor
    regdata <- wk_des_data
    regdata$fir_lockdown <- factor(regdata$fir_lockdown)

    # define function
    fm <- formula(
        paste(
            depvar, "~",
            paste("con_ring0 * fir_lockdown")
        )
    )

    # run regression
    est_mod <- feols(
            fml = fm,
            data = regdata,
            se = "hetero"
    )

    print(etable(
        est_mod,
        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
        se = "hetero"
    ))

    # extract interaction terms (i.e. unconditional DiD)
    est_mod_df <- est_mod$coeftable[4, c("Estimate", "Std. Error")] |>
        t() |>
        as.data.frame()

    # rename rows and columns
    colnames(est_mod_df) <- c("estimate", "std_error")
    
    # add variable name
    # round other variables
    est_mod_df <- est_mod_df |> 
        mutate(
            variables = depvar,
            estimate = round(estimate, digits = 3),
            std_error = round(std_error, digits = 3)
        ) |>
        relocate(
            variables, .before = estimate
        )
    
    # return output
    return(est_mod_df)
}

# get variable for regression
variable_names <- as.character(unique(des_table_wide$variables))

# define list for storage
reg_output_list <- list()

# run regressions for each variable
for(variable_name in variable_names){
    reg_out <- reg_uncond_did(depvar = variable_name)
    reg_output_list[[variable_name]] <- reg_out
}

# make data frame
uncond_did <- do.call(rbind.data.frame, reg_output_list)

# adjust rows
rownames(uncond_did) <- seq(1, nrow(uncond_did), 1)

# merge to other descriptives
des_table_wide <- merge(
    des_table_wide,
    uncond_did,
    by = "variables"
)

# reorder rows and colums
des_table_wide <- des_table_wide |>
    dplyr::arrange(
        match(
            variables,
            c(
                "ln_flatprice", "kaufpreis", "wohnflaeche", "zimmeranzahl", "alter", "ausstattung",
                "badezimmer", "etage", "heizungsart", "objektzustand", "balkon",
                "garten", "einbaukueche", "distance_smalcenter", "distance_medcenter",
                "distance_largcenter", "distance_main_airports_building",
                "distance_railroads", "distance_industry", "distance_streets"
            )
        )
    ) |>
    select(
        variables, mean_wk_treat_before_lock, mean_wk_treat_after_lock,
        mean_wk_contr_before_lock, mean_wk_contr_after_lock, estimate, std_error
    )

# rename
names(des_table_wide) <- c(
    "variables", "treated_before", "treated_after", "control_before",
    "control_after", "estimate", "std_error"
)

#----------------------------------------------
# number of observations

obs <- wk_des_data |>
    group_by(con_ring0, fir_lockdown) |>
    summarise(
        n = n()
    ) |>
    # change indicator labelling
    # add variables to match other descriptives
    mutate(
        con_ring0 = case_when(
            con_ring0 == 0 ~ "control",
            TRUE ~ "treated"
        ),
        fir_lockdown = case_when(
            fir_lockdown == 0 ~ "before",
            TRUE ~ "after"
        ),
        variables = "observations",
        estimate = NA,
        std_error = NA
    ) |>
        pivot_wider(
        names_from = c("con_ring0", "fir_lockdown"),
        values_from = n
    ) |>
    # rearrange columns to match other descriptives
    select(
        variables, treated_before, treated_after, control_before, control_after,
        estimate, std_error
    ) |>
    as.data.frame()

# bring together with other
des <- rbind(
    des_table_wide, obs
)

# export
openxlsx::write.xlsx(
    des,
    file.path(
        output_path,
        "descriptives/summary_statistics_restricted_control.xlsx"
    ),
    rowNames = FALSE
)

#----------------------------------------------
# baseline estimation

reg_base_allring_lockdown <- function(dep_var, cnt) {
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

# run regression
est_mod_restricted_control <- feols(
    fml = reg_base_allring_lockdown(dep_var = "ln_flatprice", cnt = controls),
    data = wk_prep_new_control
)

etable(
    est_mod_restricted_control,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits = "r3", se = "hetero", drop = "months"
)

# export
esttex(
    est_mod_restricted_control,
    file = file.path(output_path, "regressions/base_wk_restricted_control.tex"),
    digits = "r3", replace = TRUE, dict = tablabel_char, se = "hetero",
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)
