################################################################
# Load Data                                                    #
################################################################

wk_housing <- readRDS(file.path(dataFlug, "housing/wk_contour.rds"))

# load preparation function
# source(file.path(codePath, "functions/prep_est.Rd"))

################################################################
# Preparation                                                  #
################################################################

##### function
prep_est <- function(housing_data, dropMar = TRUE){
  # make "months" factor variable
  housing_data$months <- as.factor(housing_data$year_mon)
  
  # drop geometry
  housing_data <- st_drop_geometry(housing_data)
  
  # restrict to 5km to contour ring
  housing_data <- housing_data %>% filter(distance_main_airports <= 5)
  
  housing_data <- housing_data %>% filter(distance_main_airports >= 1 | distance_main_airports == 0)
  
  if(dropMar == TRUE){
    # drop March 2020
    housing_data <- housing_data %>% filter(year_mon != "2020-03")
  }
  
  # drop Airports Tegel and Schoenefeld
  housing_data <- housing_data %>% filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
  
  # add ring for 60dB and above
  housing_data <- housing_data %>% mutate(con_ring8 = case_when(con_ring1 == 1 | con_ring2 == 1 | con_ring3 == 1 | con_ring4 == 1 ~ 1,
                                                                con_ring5 == 1 ~ 0))
  
  housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0
  
  # return
  return(housing_data)
}

# define object characteristics -------------------------------------------
ObjChar_variables <- c("alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
                       "etage", "etageUNBEKANNT", "balkon", "balkonUNBEKANNT",
                       "as.factor(objektzustand)", "objektzustandUNBEKANNT",
                       "einbaukueche", "einbaukuecheUNBEKANNT", "garten", "gartenUNBEKANNT",
                       "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
                       "badezimmer", "badezimmerUNBEKANNT", "zimmeranzahl", "distance_industry", "distance_railroads", "distance_streets",
                       "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                       "distance_all_airports_building")

# function for estimation -------------------------------------------------

est_fm <- function(df, fm){
  feols(fml = fm, data = df, se = "hetero")
}

# table labels ------------------------------------------------------------
tablabel_objchar <- c("alter" = "Age", "alter_squ" = "Age$^2$", "wohnflaeche" = "Living space", "wohnflaeche_squ" = "Living space$^2$", 
                      "as.factor(objektzustand)2" = "Condition: First occupancy after reconstruction", "as.factor(objektzustand)3" = "Condition: Like new", "as.factor(objektzustand)4" = "Condition: Reconstructed",
                      "as.factor(objektzustand)5" = "Condition: Modernised", "as.factor(objektzustand)6" = "Condition: Completely renovated", "as.factor(objektzustand)7" = "Condition: Well kempt",
                      "as.factor(objektzustand)8" = "Condition: Needs renovation", "as.factor(objektzustand)9" = "Condition: By arrangement", "as.factor(objektzustand)10" = "Condition: Dilapidated", "objektzustandUNBEKANNT" = "Condition (unknown)",
                      "as.factor(heizungsart)2" = "Heating: Electric heating", "as.factor(heizungsart)3" = "Heating: Self-contained central heating", "as.factor(heizungsart)4" = "Heating: District heating", 
                      "as.factor(heizungsart)5" = "Heating: Floor heating", "as.factor(heizungsart)6" = "Heating: Gas heating", "as.factor(heizungsart)7" = "Heating: Wood pellet heating", 
                      "as.factor(heizungsart)8" = "Heating: Night storage heating", "as.factor(heizungsart)9" = "Heating: Heating by stove", "as.factor(heizungsart)10" = "Heating: Oil heating",
                      "as.factor(heizungsart)11" = "Heating: Solar heating", "as.factor(heizungsart)12" = "Heating: Thermal heat pump", "as.factor(heizungsart)13" = "Heating: Central heating",
                      "heizungsartUNBEKANNT" = "Heating (unknown)", "as.factor(ausstattung)2" = "Endowment: Normal", "as.factor(ausstattung)3" = "Endowment: Sophisticated", "as.factor(ausstattung)4" = "Endowment: Deluxe",
                      "ausstattungUNBEKANNT" = "Endowment (unknown)", "badezimmer" = "Number bathrooms", "badezimmerUNBEKANNT" = "Number bathrooms (unknown)",
                      "etage" = "Floor", "balkon" = "Balcony", "einbaukueche" = "Built-in kitchen", "garten" = "Garten")


################################################################
# Placebo treatment                                            #
################################################################


# general preparation -----------------------------------------------------

# apply prep function
wk_prep_placebotreat <- prep_est(wk_housing, dropMar = TRUE)

# specific preparation ----------------------------------------------------

prep_placebo <- function(housing_data){
  # restrict data to before March 2020
  housing_data <- housing_data %>% filter(year_mon < "2020-03")
  
  # define placebo treatment
  housing_data <- housing_data %>% mutate(placebo_lockdown = case_when(year_mon >= "2019-03" ~ 1,
                                                                       year_mon < "2019-03" ~ 0))
  
  # drop March 2019 (as in the "real" setting)
  housing_data <- housing_data %>% filter(year_mon != "2019-03")
}

wk_placebo <- prep_placebo(wk_prep_placebotreat)

# rerun baseline regression -----------------------------------------------

# function
reg_base_allring_lockdown <- function(depVar){
  fm <- formula(paste(depVar," ~", 
                      paste(ObjChar_variables, collapse = " + "), 
                      paste("+ placebo_lockdown * con_ring0"), 
                      "| months + r1_id"))
  
  return(fm)
}

# apply function and get formulas
wk_placebo_est <- reg_base_allring_lockdown(depVar = "ln_flatprice")

# run regression
mod_wk_placebo <- est_fm(wk_placebo, wk_placebo_est)

etable(mod_wk_placebo, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

# export
esttex(mod_wk_placebo, file = file.path(outputPath, "regression/pretrend_placebo_lockdown.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))


################################################################
# Effect development                                           #
################################################################

# general preparation -----------------------------------------------------
# keeping March 2020

wk_prep_periods <- prep_est(wk_housing, dropMar = FALSE)


# specific preparation ----------------------------------------------------

prep_periods <- function(housing_data){
  # define periods 
  housing_data$periods <- NA
  housing_data$periods[housing_data$year_mon >= "2018-01" & housing_data$year_mon <= "2019-01"] <- "t-2"
  housing_data$periods[housing_data$year_mon >= "2019-02" & housing_data$year_mon <= "2020-02"] <- "t-1"
  housing_data$periods[housing_data$year_mon >= "2020-04" & housing_data$year_mon <= "2021-06"] <- "t+1"
  housing_data$periods[housing_data$year_mon == "2020-03"] <- "t"
  housing_data$periods <- as.factor(housing_data$period)
  
  # different split
  housing_data$periods2 <- NA
  housing_data$periods2[housing_data$year_mon >= "2018-01" & housing_data$year_mon <= "2018-07"] <- "t-4"
  housing_data$periods2[housing_data$year_mon >= "2018-08" & housing_data$year_mon <= "2019-02"] <- "t-3"
  housing_data$periods2[housing_data$year_mon >= "2019-03" & housing_data$year_mon <= "2019-09"] <- "t-2"
  housing_data$periods2[housing_data$year_mon >= "2019-10" & housing_data$year_mon <= "2020-02"] <- "t-1"
  housing_data$periods2[housing_data$year_mon >= "2020-04" & housing_data$year_mon <= "2021-06"] <- "t+1"
  housing_data$periods2[housing_data$year_mon == "2020-03"] <- "t"
  housing_data$periods2 <- as.factor(housing_data$periods2)
  
  # different split
  housing_data$periods3 <- NA
  housing_data$periods3[housing_data$year_mon >= "2018-01" & housing_data$year_mon <= "2018-07"] <- "t-4"
  housing_data$periods3[housing_data$year_mon >= "2018-08" & housing_data$year_mon <= "2019-02"] <- "t-3"
  housing_data$periods3[housing_data$year_mon >= "2019-03" & housing_data$year_mon <= "2019-09"] <- "t-2"
  housing_data$periods3[housing_data$year_mon >= "2019-10" & housing_data$year_mon <= "2020-02"] <- "t-1"
  housing_data$periods3[housing_data$year_mon >= "2020-03" & housing_data$year_mon <= "2020-05"] <- "t+1"
  housing_data$periods3[housing_data$year_mon >= "2020-06" & housing_data$year_mon <= "2020-09"] <- "t+2"
  housing_data$periods3[housing_data$year_mon >= "2020-10" & housing_data$year_mon <= "2020-12"] <- "t+3"
  housing_data$periods3[housing_data$year_mon >= "2021-01" & housing_data$year_mon <= "2021-03"] <- "t+4"
  housing_data$periods3[housing_data$year_mon >= "2021-04" & housing_data$year_mon <= "2021-06"] <- "t+5"
  housing_data$periods3[housing_data$year_mon == "2020-03"] <- "t"
  housing_data$periods3 <- as.factor(housing_data$periods3)
  
  # return
  return(housing_data)
}

# apply function
wk_prep_periods <- prep_periods(wk_prep_periods)


# baseline regression WK --------------------------------------------------

mod_wk_periods <- feols(ln_flatprice ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
                 etage + etageUNBEKANNT + balkon + balkonUNBEKANNT + zimmeranzahl +
                 as.factor(objektzustand) + objektzustandUNBEKANNT +
                 einbaukueche + einbaukuecheUNBEKANNT + garten + gartenUNBEKANNT + 
                 as.factor(heizungsart) + heizungsartUNBEKANNT + as.factor(ausstattung) + ausstattungUNBEKANNT +
                 badezimmer + badezimmerUNBEKANNT + distance_largcenter + distance_medcenter + distance_smalcenter + 
                 distance_all_airports_building + distance_industry + distance_railroads + distance_streets +
                 con_ring0 +
                 i(con_ring0, factor_var = periods3, ref = "t"), data = wk_prep_periods, fixef = c("months", "r1_id"), cluster = "r1_id")

# test month regression
mod_test_months <- feols(ln_flatprice ~ alter + alter_squ + wohnflaeche + wohnflaeche_squ +
                          etage + etageUNBEKANNT + balkon + balkonUNBEKANNT + zimmeranzahl +
                          as.factor(objektzustand) + objektzustandUNBEKANNT +
                          einbaukueche + einbaukuecheUNBEKANNT + garten + gartenUNBEKANNT + 
                          as.factor(heizungsart) + heizungsartUNBEKANNT + as.factor(ausstattung) + ausstattungUNBEKANNT +
                          badezimmer + badezimmerUNBEKANNT + distance_largcenter + distance_medcenter + distance_smalcenter + 
                          distance_all_airports_building + distance_industry + distance_railroads + distance_streets +
                          con_ring0 +
                          i(con_ring0, factor_var = months, ref = "2020-03"), data = wk_prep_periods, fixef = c("months", "r1_id"), se = "hetero")

# show results ------------------------------------------------------------

etable(mod_wk_periods, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", cluster = "kid2019_gen")
etable(mod_test_months, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", cluster = "r1_id")

# export
esttex(mod_wk_periods, file = file.path(outputPath, "regression/pretrend_periods.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar), signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))
esttex(mod_test_months, file = file.path(outputPath, "regression/pretrend_periods_test_months.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar), signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))


# confidence interval -----------------------------------------------------

##### ggplot graph
# get coefficients
coef <- as.data.frame(cbind(summary(mod_wk_periods)$coefficients, summary(mod_wk_periods)$se))
coef$vars <- rownames(coef)
names(coef) <- c("coef", "se", "vars")

# select only coefficients of interest
coef_interest <- coef[48:52, ]

# change row names
rownames(coef_interest) <- seq(1:nrow(coef_interest))

coef_interest$period <- c("-1", "-2", "-3", "-4", 
                          "1")

# confidence interval min and max (90% interval)
coef_interest <- coef_interest %>% mutate(conf_min = coef - (1.645 * se),
                                          conf_max = coef + (1.645 * se))

# reorder
coef_interest <- coef_interest[c(rev(1:4), 5),]
coef_interest$order_periods <- c(1:4, 6)

# add reference period (t)
reference <- as.data.frame(cbind(coef = c(coef_interest$coef, 0), order_periods = c(coef_interest$order_periods, 5)))
reference <- reference[order(reference$order_periods), ]

# plot
pretrends_plot <- ggplot(data = coef_interest, 
                         mapping = aes(x = order_periods, y = coef))+
  geom_point(size = 2.4)+
  geom_pointrange(mapping = aes(x = order_periods, ymin = conf_min, ymax = conf_max),
                  position = position_dodge(width = 0.4), size = 0.6)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 5)+
  geom_point(mapping = aes(x = 5, y = 0), size = 2.4)+
  scale_x_continuous(breaks = c(1:6),
                     labels = c("t-4", "t-3", "t-2", "t-1", "t", "t+1"))+
  labs(x = "", y = "Coefficients and 90% CI")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.key = element_blank())

pretrends_plot

# pretrends_plot <- pretrends_plot+
#   geom_line(data = reference, mapping = aes(y = coef, x = order_periods, group = 1), linetype = "dashed", size = 1)+
#   scale_x_continuous(breaks = c(1:6),
#                      labels = c("t-4", "t-3", "t-2", "t-1", "t", "t+1"))

ggsave(plot = pretrends_plot, file.path(outputPath, "graphs/pretrends_plot.png"), height = 7, width = 8)



# combined export ---------------------------------------------------------

etable(mod_wk_placebo, mod_wk_periods, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

esttex(mod_wk_placebo, mod_wk_periods, file = file.path(outputPath, "regression/pretrend_placebo_lockdown_periods_wk.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       headers = c("placebo", "pretrends"))

