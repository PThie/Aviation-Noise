################################################################
# Load Data                                                    #
################################################################

# load housing data
red <- qs::qread(
    file.path(
        data_path, "housing/HK_contour.qs"
    )
)

# regional centers
regional_center <- readRDS(
    file.path(
        data_path,
        "raumzentren/raumzentren_nach_gemeinde_prep.rds"
    )
)

# industrial noise
industry_noise <- st_read(
    file.path(
        data_path,
        "umgebungslaerm/industrie/Ballungsraeume/Lden/Aggind_Lden_17.shp"
    ),
    quiet = TRUE
    
)

# railroad noise
rail_noise <- st_read(
    file.path(
        data_path,
        "umgebungslaerm/schiene/Basisdaten/Mrail_Source_17.shp"
    ),
    quiet = TRUE
)

# street noise
streets <- st_read(
    file.path(
        data_path,
        "umgebungslaerm/strasse/Hauptverkehrsstrassen/Basisdaten/Mroad_Source_17.shp"
    ),
    quiet = TRUE
)


################################################################
# Lockdown indicator                                           #
################################################################

red <- red |>
    mutate(
        fir_lockdown = case_when(
            year_mon_end >= "2020-03" ~ 1,
            TRUE ~ 0
        )
    )

################################################################
# Adding three months intervalls for treatment time            #
################################################################

red <- red |>
    mutate(
        arpjun20 = case_when(
            year_mon_end >= "2020-04" & year_mon_end <= "2020-06" ~ 1,
            TRUE ~ 0
        ),
        julsep20 = case_when(
            year_mon_end >= "2020-07" & year_mon_end <= "2020-09" ~ 1,
            TRUE ~ 0
        ),
        octdec20 = case_when(
            year_mon_end >= "2020-10" & year_mon_end <= "2020-12" ~ 1,
            TRUE ~ 0
        ),
        janmar21 = case_when(
            year_mon_end >= "2021-01" & year_mon_end <= "2021-03" ~ 1,
            TRUE ~ 0
        ),
        aprjun21 = case_when(
            year_mon_end >= "2021-04" & year_mon_end <= "2021-06" ~ 1,
            TRUE ~ 0
        ),
        julsep21 = case_when(
            year_mon_end >= "2021-07" & year_mon_end <= "2021-09" ~ 1,
            TRUE ~ 0
        ),
        octdec21 = case_when(
            year_mon_end >= "2021-10" & year_mon_end <= "2021-12" ~ 1,
            TRUE ~ 0
        ),
        janmar22 = case_when(
            year_mon_end >= "2022-01" & year_mon_end <= "2022-03" ~ 1,
            TRUE ~ 0
        ),
        aprjun22 = case_when(
            year_mon_end >= "2022-04" & year_mon_end <= "2022-06" ~ 1,
            TRUE ~ 0
        )
    )

################################################################
# Adding regional centers                                      #
################################################################

#----------------------------------------------
# prepare regional centers

regional_center <- st_set_geometry(
    regional_center,
    regional_center$geometry
)

# transform
regional_center <- st_transform(
    regional_center,
    crs = st_crs(red)
)

# drop if there is no geometry
regional_center <- regional_center |>
    filter(!st_is_empty(geometry))

#----------------------------------------------
# subset for different regional centers

# large center (Oberzentrum == 1)
largcenter <- regional_center |>
    filter(center_identifier == 1)
# medium center (Mittelzentrum == 2)
medcenter <- regional_center |>
    filter(center_identifier == 2)
# small center (Grundzentrum == 3)
smalcenter <- regional_center |>
    filter(center_identifier == 3)

#----------------------------------------------
# distance to the nearest regional centers

red$distance_largcenter <- as.numeric(
    apply(st_distance(red, largcenter), 1, min)
) / 1000

red$distance_medcenter <- as.numeric(
    apply(st_distance(red, medcenter), 1, min)
) / 1000

red$distance_smalcenter <- as.numeric(
    apply(st_distance(red, smalcenter), 1, min)
) / 1000

################################################################
# Adding industry noise                                        #
################################################################

#----------------------------------------------
# prepare noise

# transform
industry_noise <- st_transform(
    industry_noise,
    crs = st_crs(red)
)

# find nearest
nearest_ind <- st_nearest_feature(red, industry_noise)

#----------------------------------------------
# calculate distance
red$distance_industry <- as.numeric(
    st_distance(
        red,
        industry_noise[nearest_ind, ],
        by_element = TRUE
    ) / 1000
)

################################################################
# Adding railroad noise                                        #
################################################################

#----------------------------------------------
# prepare noise

# transform
rail_noise <- st_transform(
    rail_noise,
    crs = st_crs(red)
)

# find nearest
nearest_rail <- st_nearest_feature(red, rail_noise)

#----------------------------------------------
# calculate distance
red$distance_railroads <- as.numeric(
    st_distance(
        red,
        rail_noise[nearest_rail, ],
        by_element = TRUE
    ) / 1000
)

################################################################
# Adding street noise                                          #
################################################################

#----------------------------------------------
# prepare noise

# transform
streets <- st_transform(
    streets,
    crs = st_crs(red)
)

# find nearest
nearest <- st_nearest_feature(red, streets)

#----------------------------------------------
# calculate distance
red$distance_streets <- as.numeric(
    st_distance(red,
    streets[nearest, ],
    by_element = TRUE
    ) / 1000
)

################################################################
# Export                                                       #
################################################################

qs::qsave(
    red,
    file.path(
        data_path, "housing/HK_complete.qs"
    )
)
