###############################################################
# Description                                                 #
###############################################################

# This function generates descriptive statistics for the housing data.

###############################################################
# load data                                                   #
###############################################################

#----------------------------------------------
# housing data
housing_wk <- qs::qread(
    file.path(
        data_path,
        "housing/WK_complete.qs"
    )
)

###############################################################
# Plotting price development                                  #
###############################################################

prep_est_mod <- function(housing_data){
    #' @title Preparation for estimation
    #' 
    #' @description This function prepares the data for estimation.
    #' 
    #' @param housing_data Housing data after contour information and additonal
    #' variables have been added
    #'   
    #' @return Returns housing data ready for estimation
    #' @note Difference to overall prep_est function: March 2020 stays included
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

    # return
    return(housing_data)
}

#----------------------------------------------
# prepare

# apply preparation for estimation
housing_wk_prep <- prep_est_mod(housing_wk)

# add plot date and quarters
housing_wk_prep <- housing_wk_prep |>
    mutate(
        plot_date = as.yearmon(year_mon_end),
        quarter = as.yearqtr(plot_date),
        plot_date = NULL
    )

#----------------------------------------------
# calculate average prices by quarter

avg_prices <- housing_wk_prep |>
    group_by(quarter, con_ring0) |>
    summarise(
        mean_price = mean(price_sqmeter, na.rm = TRUE)
    ) |>
    as.data.frame()

# add lockdown indicator
avg_prices <- avg_prices |>
    mutate(
        lockdown = case_when(
            quarter <= 2020.00 ~ "before_lock",
            TRUE ~ "after_lock"
        )
    )

#----------------------------------------------
# plot average prices

plot_wk <- ggplot(
    data = avg_prices,
    mapping = aes(x = quarter, group = factor(con_ring0))
    )+
    geom_line(
        mapping = aes(y = mean_price,
        linetype = factor(con_ring0)),
        linewidth = 1
    )+
    scale_linetype_manual(
        values = c(
            "0" = "solid",
            "1" = "twodash"
        ),
        labels = c(
            "0" = "< 55dB (control)",
            "1" = "\u2265 55dB (treated)"
        ),
        name = ""
    )+
    scale_y_continuous(
        breaks = seq(3000, 6000, 500),
        labels = scales::comma
    )+
    scale_x_yearqtr(
        format = "%Y Q%q", 
        limits = c(min(avg_prices$quarter), max(avg_prices$quarter)),
        breaks = seq(min(avg_prices$quarter), max(avg_prices$quarter), 0.25)
    )+
    labs(
        x = "",
        y = expression(paste("Price per sq. meter [€/", m^{2}, "]"))
    )+
    geom_segment(
        aes(x = 2020.00, xend = 2020.00, y = 3000, yend = 6000),
        linetype = 3,
        linewidth = 0.9
    )+
    # add trends
    geom_smooth(
        data = avg_prices |> filter(con_ring0 == 0),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        aes(group = lockdown, x = quarter, y = mean_price), col = "grey60"
    )+
    geom_smooth(
        data = avg_prices |> filter(con_ring0 == 1),
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        aes(group = lockdown, x = quarter, y = mean_price), col = "grey60"
    )+
    owntheme+
    theme(
        legend.position = "bottom"
    )

# export
ggsave(
    plot = plot_wk,
    file.path(
        output_path, "graphs/alternative_style_diss/wk_price_development.png"
    ), 
    width = 8,
    height = 6
)

###############################################################
# Plotting number of observations by quarter                  #
###############################################################

#----------------------------------------------
# overall count by quarter

overall_count_n <- housing_wk_prep |>
    # drop last month of the data
    # because by definition of end date all adds that are active will ahve the
    # end date of the last month
    filter(year_mon_end < "2022-06") |>
    group_by(quarter, con_ring0) |>
    summarise(
        n = n()
    ) |>
    as.data.frame()

# plot
plot_count_overall <- ggplot(
    data = overall_count_n,
    mapping = aes(x = quarter, group = factor(con_ring0))
    )+
    geom_line(
        aes(y = n, linetype = factor(con_ring0)),
        linewidth = 1
    )+
    scale_linetype_manual(
        values = c(
            "0" = "solid",
            "1" = "twodash"
        ),
        labels = c(
            "0" = "< 55dB (control)",
            "1" = "\u2265 55dB (treated)"
        ),
        name = "Groups"
    )+
    scale_y_continuous(
        breaks = seq(0, 10000, 1000),
        labels = scales::comma
    )+
    scale_x_yearqtr(
        format = "%Y Q%q", 
        limits = c(min(overall_count_n$quarter), max(overall_count_n$quarter)),
        breaks = seq(min(overall_count_n$quarter), max(overall_count_n$quarter), 0.25)
    )+
    geom_segment(
        aes(x = 2020.00, xend = 2020.00, y = 0, yend = 6000),
        linetype = "dotted",
        linewidth = 0.9
    )+
    labs(
        x = "",
        y = "Observations"
    )+
    owntheme+
    theme(
        legend.position = "bottom",
        legend.title = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22)
    )

ggsave(
    plot = plot_count_overall,
    file.path(
        output_path,
        "graphs/observations_by_quarter.png"
    ),
    dpi = owndpi,
    width = 10,
    height = 8
)

#----------------------------------------------
# count by quarter and airport

count_airport_n <- housing_wk_prep |>
    # drop last month of the data
    # because by definition of end date all adds that are active will ahve the
    # end date of the last month
    filter(year_mon_end < "2022-06") |>
    group_by(closest_main_airports, quarter, con_ring0) |>
    summarise(
        n = n()
    ) |>
    as.data.frame()

# define colors
col <- met.brewer(
    name = "Redon",
    n = 9
)

# NOTE: treat Frankfurt differently (make it bold)
plot_count_airports <- ggplot()+
    geom_line(
        data = count_airport_n |> filter(con_ring0 == 0 & closest_main_airports != "EDDF"),
        mapping = aes(
            x = quarter,
            y = n,
            group = factor(closest_main_airports),
            col = factor(closest_main_airports),
            linetype = "control"
        ),
        linewidth = 1
    )+
    geom_line(
        data = count_airport_n |> filter(con_ring0 == 0 & closest_main_airports == "EDDF"),
        mapping = aes(
            x = quarter,
            y = n,
            group = 1,
            col = factor(closest_main_airports),
            linetype = "control"
        ),
        linewidth = 2
    )+
    geom_line(
        data = count_airport_n |> filter(con_ring0 == 1 & closest_main_airports != "EDDF"),
        mapping = aes(
            x = quarter,
            y = n,
            group = factor(closest_main_airports),
            col = factor(closest_main_airports),
            linetype = "treated"
        ),
        linewidth = 1
    )+
    geom_line(
        data = count_airport_n |> filter(con_ring0 == 1 & closest_main_airports == "EDDF"),
        mapping = aes(
            x = quarter,
            y = n,
            group = 1,
            col = factor(closest_main_airports),
            linetype = "treated"
        ),
        linewidth = 2
    )+
    scale_linetype_manual(
        values = c(
            "control" = "solid",
            "treated" = "twodash"
        ),
        labels = c(
            "control" = "< 55dB (control)",
            "treated" = "\u2265 55dB (treated)"
        ),
        name = "Groups"
    )+
    scale_color_manual(
        values = col,
        labels = c(
            "EDDF" = "Frankfurt",
            "EDDH" = "Hannover",
            "EDDK" = "Cologne",
            "EDDL" = "Dusseldorf",
            "EDDM" = "Munich",
            "EDDN" = "Nuremberg",
            "EDDP" = "Leipzig",
            "EDDS" = "Stuttgart",
            "EDDV" = "Hannover"
        ),
        name = "Airports"
    )+
    scale_y_continuous(
        breaks = seq(0, 3000, 500),
        labels = scales::comma
    )+
    scale_x_yearqtr(
        format = "%Y Q%q", 
        limits = c(min(count_airport_n$quarter), max(count_airport_n$quarter)),
        breaks = seq(min(count_airport_n$quarter), max(count_airport_n$quarter), 0.25)
    )+
    geom_segment(
        aes(x = 2020.00, xend = 2020.00, y = 0, yend = 2000),
        linetype = "dotted",
        linewidth = 0.9
    )+
    labs(
        x = "",
        y = "Observations"
    )+
    owntheme+
    theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(),
        legend.title = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22)
    )

ggsave(
    plot = plot_count_airports,
    file.path(
        output_path,
        "graphs/observations_by_quarter_airports.png"
    ),
    dpi = owndpi,
    width = 10,
    height = 8
)

###############################################################
# Descriptives Housing Data                                   #
###############################################################

#----------------------------------------------
# subsetting
# keep only the relevant variables

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
wk_des_data <- prep_descriptives(housing_wk_prep, price_variable = "kaufpreis")

#----------------------------------------------
# descriptives by lockdown timing (i.e. before and after the lockdown)

group_descriptives <- function(housing, name){
    # calculate descriptives for before and after lockdown
    des <- describeBy(
        housing,
        group = housing$fir_lockdown, mat = TRUE, digits = 3, fast = TRUE, na.rm = TRUE
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

#----------------------------------------------
# export

openxlsx::write.xlsx(
    des,
    file.path(
        output_path, "descriptives/summary_statistics.xlsx"
    ),
    rowNames = FALSE
)