######################################################
# Load data                                          #
######################################################

#----------------------------------------------
# grid data (microm)
griddata_org <- haven::read_dta(
    "M:/_FDZ/RWI-GEO/RWI-GEO-GRID/daten/Original/Stata16/V12/microm_panel_05-20.dta"
)

#----------------------------------------------
# noise contour

# main airports
haupt_contour <- qs::qread(
    file.path(
        data_path,
        "Contour_Maps/main_airports_contour.qs"
    )
) |>
st_make_valid()

#----------------------------------------------
# empty grid for Germany
grid_ger <- st_read(
    file.path(
        data_gebiete,
        "Raster/ger_1km_rectangle/ger_1km_rectangle.shp"
    ),
    quiet = TRUE
)

######################################################
# Preparation                                        #
######################################################

#----------------------------------------------
# prepare griddata

# copy to avoid long leading
griddata <- griddata_org

# keep only 2019 (most recent period)
griddata <- griddata |>
    filter(year == 2019)

# keep only variables of interest
griddata <- griddata |>
    select(
        "r1_id", "r1_mba_a_haeuser", "r1_mba_a_haushalt", "r1_mba_a_wohngeb",
        "r1_kkr_w_summe", "r1_mso_p_singles", "r1_mso_p_paare",
        "r1_mso_p_familien", "r1_alq_p_quote",
        "r1_ewa_a_gesamt", "r1_eag_p_m00bis03":"r1_eag_p_w75undgr"
    )

# define NAs
griddata[griddata < 0] <- NA

# temp save
write.fst(
    griddata,
    file.path(
        data_path,
        "griddata/griddata_prep.fst"
    )
)

#----------------------------------------------
# contour grid

# transform grid to same crs as airports
grid_ger <- st_transform(grid_ger, crs = st_crs(haupt_contour))

# intersect grids and airport contours
# assign grid to largest overlap
airport_grids <- st_join(
    grid_ger,
    haupt_contour,
    largest = TRUE
)

# define treated and not treated
airport_grids <- airport_grids |> 
    mutate(
        # define treated and non-treated
        treated = case_when(
            is.na(icao) ~ "nontreated",
            TRUE ~ "treated"
        ),
        id = NULL,
        # make some redefines for NAs
        icao = case_when(
            is.na(icao) ~ "NOAI",
            TRUE ~ icao
        ),
        DB_Low = case_when(
            is.na(DB_Low) ~ as.integer(-99),
            TRUE ~ DB_Low
        ),
        DB_High = case_when(
            is.na(DB_High) ~ as.integer(-99),
            TRUE ~ DB_High
        )
    ) |>
    st_drop_geometry()

#----------------------------------------------
# buffer around contours

# form union
haupt_contour_union <- haupt_contour |>
    group_by(icao) |>
    summarise(geometry = st_union(geometry)) |>
    as.data.frame()

haupt_contour_union <- st_set_geometry(
    haupt_contour_union,
    haupt_contour_union$geometry
)

# buffer 5km
haupt_contour_buffer_5km <- st_buffer(haupt_contour_union, dist = 5000)

# buffer 1km
haupt_contour_buffer_1km <- st_buffer(haupt_contour_union, dist = 1000)

# join with grids
airport_grids_buffer_5km <- st_join(
    grid_ger,
    haupt_contour_buffer_5km,
    largest = TRUE
)
airport_grids_buffer_1km <- st_join(
    grid_ger,
    haupt_contour_buffer_1km,
    largest = TRUE
)

# define treated and not treated
airport_grids_buffer_5km <- airport_grids_buffer_5km |>
    mutate(
        treated_buffer_5km = case_when(
            is.na(icao) ~ "nontreated",
            !is.na(icao) ~ "treated"
        ),
        id = NULL
    ) |>
    st_drop_geometry()

airport_grids_buffer_1km <- airport_grids_buffer_1km |>
    mutate(
        treated_buffer_1km = case_when(
            is.na(icao) ~ "nontreated",
            !is.na(icao) ~ "treated"
        ),
        id = NULL
    ) |>
    st_drop_geometry()

#----------------------------------------------
# overall

overall <- merge(
    airport_grids,
    airport_grids_buffer_5km,
    by = "idm"
)

overall <- merge(
    overall,
    airport_grids_buffer_1km,
    by = "idm"
)

overall <- overall |>
    mutate(
        treated_indicator = case_when(
            treated_buffer_5km == "treated" & treated == "treated" & treated_buffer_1km == "treated"  ~ "treated",
            treated_buffer_5km == "treated" & treated == "nontreated" & treated_buffer_1km == "nontreated" ~ "control",
            treated_buffer_5km == "treated" & treated == "nontreated" & treated_buffer_1km == "treated" ~ "1kmbuffer",            
            treated_buffer_5km == "nontreated" & treated == "nontreated" & treated_buffer_1km == "nontreated" ~ "outside"
        ),
        icao.x = NULL,
        icao.y = NULL,
        icao = NULL
    ) |>
    rename(
        r1_id = idm,
        noise_low = DB_Low,
        noise_high = DB_High
    )

# drop those that are "outside" i.e. not part of control or treatment group
overall <- overall |>
    filter(treated_indicator != "outside")

# exclude also 1km buffer
overall <- overall |>
    filter(treated_indicator != "1kmbuffer")

# drop treated and treated_buffer
overall$treated <- NULL
overall$treated_buffer_5km <- NULL
overall$treated_buffer_1km <- NULL

#----------------------------------------------
# merge airport grids and griddata

griddata_groups <- merge(
    griddata,
    overall,
    by = "r1_id",
    all.y = TRUE
)

# drop unpopulated grids
griddata_groups <- griddata_groups |>
    filter(r1_ewa_a_gesamt != 0)

# caculate purchasing power by household
griddata_groups <- griddata_groups |>
    mutate(purchpower_household = r1_kkr_w_summe / r1_mba_a_haushalt)

# define age groups
griddata_groups <- griddata_groups |>
    rowwise() |>
    mutate(
        age0to18 = (
            r1_eag_p_m00bis03 + r1_eag_p_w00bis03 + r1_eag_p_m03bis06 + r1_eag_p_w03bis06 +
            r1_eag_p_m06bis10 + r1_eag_p_w06bis10 + r1_eag_p_m10bis15 + r1_eag_p_w10bis15 +
            r1_eag_p_m15bis18 + r1_eag_p_w15bis18
        ),
        age18to65 = (
            r1_eag_p_m18bis20 + r1_eag_p_w18bis20 + r1_eag_p_m20bis25 + r1_eag_p_w20bis25 +
            r1_eag_p_m25bis30 + r1_eag_p_w25bis30 + r1_eag_p_m30bis35 + r1_eag_p_w30bis35 +
            r1_eag_p_m35bis40 + r1_eag_p_w35bis40 + r1_eag_p_m40bis45 + r1_eag_p_w40bis45 +
            r1_eag_p_m45bis50 + r1_eag_p_w45bis50 + r1_eag_p_m50bis55 + r1_eag_p_w50bis55 +
            r1_eag_p_m55bis60 + r1_eag_p_w55bis60 + r1_eag_p_m60bis65 + r1_eag_p_w60bis65
        ),
        age65above = (
            r1_eag_p_m65bis75 + r1_eag_p_w65bis75 + r1_eag_p_m75undgr + r1_eag_p_w75undgr
        ),
        non_singles = r1_mso_p_paare + r1_mso_p_familien,
        non_working = age0to18 + age65above,
        working = 100 - non_working
        ) |>
        as.data.frame()

#----------------------------------------------
# export
write.fst(
    griddata_groups,
    file.path(
        data_path, "griddata/griddata_groups.fst"
    )
)

# -------------------------------------------------------------------------
# descriptives by group (treated vs. control)

griddata_groups <- read.fst(
    file.path(
        data_path, "griddata/griddata_groups.fst"
    )
)

# define variables of interest
griddata_groups <- griddata_groups |>
    select(
        r1_mba_a_haushalt, r1_ewa_a_gesamt, r1_alq_p_quote,
        purchpower_household, r1_mso_p_singles, r1_mso_p_paare, r1_mso_p_familien,
        working, treated_indicator,
    )

des_grid <- describeBy(
    griddata_groups,
    mat = TRUE,
    group = "treated_indicator",
    fast = T,
    na.rm = TRUE,
    digits = 3
)

# add variable names
des_grid$variables <- rownames(des_grid)

# adjust row names
rownames(des_grid) <- seq(1, nrow(des_grid), 1)

# export
write.xlsx(
    des_grid,
    file.path(
        output_path,
        "descriptives/griddata_descriptives.xlsx"
    )
)

######################################################
# t-tests                                            #
######################################################

# define testing function
testing <- function(var) {
    # perform t-test
    test_results <- t.test(
        griddata_groups[[var]] ~ treated_indicator,
        data = griddata_groups,
        alternative = "two.sided",
        var.equal = FALSE
    )

    # extract t-statistic and p-value
    test_df <- as.data.frame(
        cbind(
            var_name = var,
            t_stat = round(
                test_results$statistic,
                digits = 3
            ),
            p_value = round(
                test_results$p.value,
                digits = 3
            )
        )
    )

    # adjust rownames
    rownames(test_df) <- seq(1, nrow(test_df), 1)

    # return results
    return(test_df)
}

# define variables
vars <- c(
    "purchpower_household", "r1_alq_p_quote", "r1_mba_a_haushalt", "r1_ewa_a_gesamt",
    "r1_mso_p_singles", "r1_mso_p_paare", "r1_mso_p_familien", "working"
)

# loop through variables
results_list <- list()
for(v in vars) {
    results_list[[v]] <- testing(var = v)
}

# combine in data frame
results <- rbindlist(results_list)

#----------------------------------------------
# export
write.xlsx(
    results,
    file.path(
        output_path,
        "descriptives/t_testing.xlsx"
    ),
    rowNames = FALSE
)