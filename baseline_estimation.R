################################################################
# Load Data                                                    #
################################################################

wk_housing <- readRDS(file.path(dataFlug, "housing/wk_contour_short.rds"))
hk_housing <- readRDS(file.path(dataFlug, "housing/hk_contour.rds"))
wm_housing <- readRDS(file.path(dataFlug, "housing/wm_contour.rds"))

# load preparation function
source(file.path(codePath, "functions/prep_est.Rd"))


################################################################
# preparation                                                  #
################################################################

# apply prep function -----------------------------------------------------
wk_housing <- prep_est(wk_housing)


# define object characteristics -------------------------------------------
ObjChar_variables <- c("alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
                       "etage", "etageUNBEKANNT", "balkon", "balkonUNBEKANNT",
                       "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
                       "einbaukueche", "einbaukuecheUNBEKANNT", "garten", "gartenUNBEKANNT",
                       "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
                       "badezimmer", "badezimmerUNBEKANNT",
                       "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                       "distance_industry", "distance_railroads", "distance_streets",
                       "distance_all_airports_building")


# baseline formula --------------------------------------------------------

# baseline setting
# key variable: interaction lockdown x all rings together (larger equal 55dB)

reg_base_allring_lockdown <- function(depVar){
  fm <- formula(paste(depVar," ~", 
                      paste(ObjChar_variables, collapse = " + "), 
                      paste("+ fir_lockdown * con_ring0"), 
                      "| months + r1_id"))
  
  return(fm)
}


# function for estimation -------------------------------------------------

est_fm <- function(df, fm){
  feols(fml = fm, data = df)
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
# baseline                                                     #
################################################################

# specify formula
wk_base <- reg_base_allring_lockdown(depVar = "ln_flatprice")

###### estimation

# run regression
wk_base_est <- est_fm(df = wk_housing, fm = wk_base)

# display results
etable(wk_base_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

# export
esttex(wk_base_est, file = file.path(outputPath, "regression/base_wk.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))


################################################################
# clustering                                                   #
################################################################

# cluster on grids
est_fm_gridcluster <- function(df, fm){
  feols(fml = fm, data = df, cluster = "r1_id")
}

wk_base_est_gridcl <- est_fm_gridcluster(df = wk_housing, fm = wk_base)

# cluster on municipality
est_fm_municcluster <- function(df, fm){
  feols(fml = fm, data = df, cluster = "gid2019_gen")
}

wk_base_est_municcl <- est_fm_municcluster(df = wk_housing, fm = wk_base)

# cluster district
est_fm_distcluster <- function(df, fm){
  feols(fml = fm, data = df, cluster = "kid2019_gen")
}

wk_base_est_discl <- est_fm_distcluster(df = wk_housing, fm = wk_base)

etable(wk_base_est_gridcl, wk_base_est_municcl, wk_base_est_discl, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3",
       headers = c("gridclust", "municclust", "distrclust"))

# export
esttex(wk_base_est_gridcl, wk_base_est_municcl, wk_base_est_discl, file = file.path(outputPath, "regression/base_wk_cluster.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), headers = c("gridclust", "municclust", "distrclust"))




################################################################
# baseline: house price                                        #
################################################################


# apply prep function -----------------------------------------------------
hk_housing <- prep_est(hk_housing)


ObjChar_variables <- c("alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
                       "anzahletagen", "anzahletagenUNBEKANNT", "grundstuecksflaeche", "grundstuecksflaeche_squ",
                       "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
                       "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
                       "badezimmer", "badezimmerUNBEKANNT",
                       "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                       "distance_industry", "distance_railroads", "distance_streets",
                       "distance_all_airports_building")

# set up formula
hk_base <- reg_base_allring_lockdown(depVar = "ln_houseprice")

###### estimation

# run regression
hk_base_est <- est_fm(df = hk_housing, fm = hk_base)

# display results
etable(hk_base_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")


################################################################
# baseline: apartment rents                                    #
################################################################

# apply prep function -----------------------------------------------------
wm_housing <- prep_est(wm_housing)

# define object characteristics -------------------------------------------
ObjChar_variables <- c("alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ", 
                       "etage", "etageUNBEKANNT", "balkon", "balkonUNBEKANNT",
                       "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
                       "einbaukueche", "einbaukuecheUNBEKANNT", "garten", "gartenUNBEKANNT",
                       "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
                       "badezimmer", "badezimmerUNBEKANNT",
                       "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                       "distance_industry", "distance_railroads", "distance_streets",
                       "distance_all_airports_building")

# set up formula 
wm_base <- reg_base_allring_lockdown(depVar = "ln_rent_sqmeter")

###### estimation

# run regression
wm_base_est <- est_fm(df = wm_housing, fm = wm_base)

# display results
etable(wm_base_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

################################################################
# baseline: house price & apartment prices                     #
################################################################

# DONT FORGET TO RUN THE PREP FUNCTIONS

################
# combine HK and WK
columnnames <- intersect(names(wk_housing), names(hk_housing))

# prep function
combine_prep <- function(dataframe, type){
  # keep only the columns that are in both data sets
  dataframe_comb <- dataframe %>% select(columnnames)
  
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

wk_housing_comb <- combine_prep(wk_housing, type = "WK")
hk_housing_comb <- combine_prep(hk_housing, type = "HK")

housing_comb <- rbind(wk_housing_comb, hk_housing_comb)



ObjChar_variables <- c("alter", "alter_squ", "wohnflaeche", "wohnflaeche_squ",
                       "as.factor(objektzustand)", "objektzustandUNBEKANNT", "zimmeranzahl",
                       "as.factor(heizungsart)", "heizungsartUNBEKANNT", "as.factor(ausstattung)", "ausstattungUNBEKANNT",
                       "badezimmer", "badezimmerUNBEKANNT", "distance_railroads", "distance_industry",
                       "distance_largcenter", "distance_medcenter", "distance_smalcenter", 
                       "distance_industry", "distance_railroads", "distance_streets",
                       "distance_all_airports_building", "type")


comb_base <- reg_base_allring_lockdown(depVar = "ln_price")

###### estimation

# run regression
comb_base_est <- est_fm(df = housing_comb, fm = comb_base)

# display results
etable(comb_base_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

# display combine
etable(hk_base_est, comb_base_est, wm_base_est, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = "r3", se = "hetero")

# export
esttex(hk_base_est, comb_base_est, wm_base_est, file = file.path(outputPath, "regression/base_hk_wk_wm_combined.tex"), digits = "r3", replace = TRUE, dict = c(tablabel_objchar),
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       headers = c("houses", "hous_apart_sales", "apart_rent"))


