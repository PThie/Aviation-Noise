###############################################################
# load data                                                   #
###############################################################

#----------------------------------------------
# housing data
housing_wk <- qs::qread(
    file.path(
        data_path, "housing/WK_complete.qs"
    )
)

#----------------------------------------------
# airport locations
airport_locations <- qs::qread(
    file.path(
        data_path,
        "Flughaefen/airport_locations_prep.qs"
    )
)

#----------------------------------------------
# load state boundaries
bula <- st_read(
    file.path(
        data_gebiete, "Bundesland/2019/VG250_LAN.shp"
    ),
    quiet = TRUE
)
bula <- st_transform(bula, crs = utmcrs)

#----------------------------------------------
# noise contour

# main airports
haupt_contour <- qs::qread(
    file.path(
        data_path,
        "Contour_Maps/main_airports_contour.qs"
    )
)


# # -------------------------------------------------------------------------
# # load noise data
# # Duesseldorf -------------------------------------------------------------
# dus19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged_2019.dta"))
# dus20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged.dta"))

# # Frankfurt ---------------------------------------------------------------
# fra18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2018.dta"))
# fra19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2019.dta"))
# fra20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged.dta"))
# fra21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2021.dta"))

# # Hamburg -----------------------------------------------------------------
# ham19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged_2019.dta"))
# ham20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged.dta"))

# # Hannover ----------------------------------------------------------------
# haj18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2018.dta"))
# haj19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2019.dta"))
# haj20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged.dta"))
# haj21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2021.dta"))

# # Leipzig -----------------------------------------------------------------
# lej19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2019.dta"))
# lej20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged.dta"))
# lej21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2021.dta"))

# # Muenchen ----------------------------------------------------------------
# muc19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged_2019.dta"))
# muc20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged.dta"))

###############################################################
# Mapping Airports                                            #
###############################################################

# keep only the main airports
airports_main <- airport_locations |>
    filter(mainair == 1)

# keep only the airports which included in the study
included_airports <- c("EDDL", "EDDV", "EDDP", "EDDF", "EDDM", "EDDH", "EDDS", "EDDN", "EDDK")
airports_main <- airports_main |>
    filter(icao %in% included_airports == TRUE) |>
    # add labels for included airports
    mutate(
        labels = case_when(
            stringr::str_detect(name, "Stuttgart") == TRUE ~ "Stuttgart",
            stringr::str_detect(name, "Nürnberg") == TRUE ~ "Nuremberg",
            stringr::str_detect(name, "Düsseldorf") == TRUE ~ "Dusseldorf",
            stringr::str_detect(name, "Hannover") == TRUE ~ "Hannover",
            stringr::str_detect(name, "Leipzig") == TRUE ~ "Leipzig",
            stringr::str_detect(name, "Frankfurt") == TRUE ~ "Frankfurt",
            stringr::str_detect(name, "Köln") == TRUE ~ "Cologne",
            stringr::str_detect(name, "Hamburg") == TRUE ~ "Hamburg",
            stringr::str_detect(name, "München") == TRUE ~ "Munich"
        )
    )

# make sf
airports_main <- st_set_geometry(airports_main, airports_main$geometry)

#----------------------------------------------
# mapping airport locations

map_airports <- ggplot()+
    geom_sf(
        data = bula,
        mapping = aes(geometry = geometry),
        fill = NA,
        col = "gray"
    )+
    geom_sf(
        data = airports_main,
        mapping = aes(geometry = geometry),
        size = 2
    )+
    geom_sf_text(
        data = airports_main,
        aes(
            label = labels,
            hjust = -0.2
        )
    )+
    theme_void()

# export
ggsave(
    map_airports,
    file = file.path(
        output_path, "graphs/map_airport_locations.png"
    ),
    dpi = owndpi
)

###############################################################
# Descriptives Flight Activity                                #
###############################################################

#----------------------------------------------
# load data

# create list with all file names
file_list <- list.files(
    path = file.path(
        data_path, "Flughaefen/Luftverkehr"
    ),
    pattern = "*.xlsx",
    full.names = TRUE
)

# read in all excel files at once
df_list_raw <- lapply(
    file.list,
    function(x){as.data.frame(read_excel(x, sheet = "1.1.2"))}
)

# extract the month names from the file list
months_names <- substring(
    file_list,
    first = 68,
    last = nchar(file.list) - 5
)

# assign names to the data list
names(df_list_raw) <- months_names

#----------------------------------------------
# preparation

# prepare function
prep_luftverkehr <- function(data){
    # drop unwanted rows and columns
    data <- data[15:38, 1:2]
    
    # rename columns
    colnames(data) <- c("airports", "flight_activity")
    
    # return
    return(data)
}

# apply preparation function to each element in list
df_list_people <- lapply(
    df_list_raw,
    prep_luftverkehr
)

# turn list into data frame
flight_act <- bind_rows(df_list_people, .id = "id")

# some cleaning
flight_act <- flight_act |>
    mutate(
        # split id into month and year
        month = substr(id, start = 1, stop = 3),
        year = substr(id, start = 4, stop = 5),
        year = paste0("20", year),
        # create year_mon variable
        date = ymd(paste(year, month, "01", sep = "-")),
        year_mon = format(as.Date(date), "%Y-%m"),
        # make flight activity numeric
        flight_activity = as.numeric(flight_activity)
    ) |>
    # keep only relevant columns
    select(
        year_mon, airports, flight_activity
    ) |>
    # restrict to 2018 and later
    filter(year_mon >= "2018-01") |>
    # restrict to airports of interest
    filter(
        airports %in% c(
            "Düsseldorf", "Frankfurt/Main", "Hamburg", "Hannover",
            "Köln/Bonn", "Leipzig/Halle", "München", "Nürnberg", "Stuttgart"
        )
    ) |>
    # sort by months
    arrange(year_mon)

#----------------------------------------------
# calculate average flight activity over time

avg_flight_activity <- flight_act |>
    group_by(year_mon) |>
    summarise(
        avg_flight_act = mean(flight_activity, na.rm = TRUE)
    ) |>
    as.data.frame()

# calculate average for periods before and after lockdown
before_lock <- as.numeric(
        flight_act |>
            filter(year_mon <= "2020-03") |>
            summarise(
                avg_flight_act = mean(flight_activity, na.rm = TRUE)
            )
)

after_lock <- as.numeric(
        flight_act |>
            filter(year_mon > "2020-03") |>
            summarise(
                avg_flight_act = mean(flight_activity, na.rm = TRUE)
            )
)

#----------------------------------------------
# plot average flight activity

# month labels
month_labels <- c(
    "Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
    "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021", "Sep 2021"
)

# define plot date
avg_flight_activity <- avg_flight_activity |>
    mutate(
        plot_date = as.yearmon(year_mon)
    )

# define lockdown time
lock <- as.numeric(
    avg_flight_activity$plot_date[avg_flight_activity$year_mon == "2020-03"]
)

# plot
plot_avg_flightact <- ggplot(avg_flight_activity)+
    geom_line(
        mapping = aes(x = plot_date, y = avg_flight_act, group = 1),
        linewidth = 1
    )+
    scale_x_yearmon(
        breaks = seq(min(avg_flight_activity$plot_date), max(avg_flight_activity$plot_date), 0.34),
        labels = month_labels
    )+
    scale_y_continuous(
        name = "Avg. Flight Activity",
        breaks = seq(2000, 18000, 2000),
        labels = scales::comma
    )+
    geom_segment(
        aes(x = lock, xend = lock, y = 2000, yend = 18000),
        linetype = 3,
        size = 1
    )+
    # add averages before and after lockdown
    geom_segment(
        aes(
            x = as.numeric(min(plot_date)), xend = lock, y = before_lock, yend = before_lock
        ),
        linetype = "twodash",
        linewidth = 1
    )+
    geom_segment(
        aes(
            x = lock, xend = as.numeric(max(plot_date)), y = after_lock, yend = after_lock
        ),
        linetype = "twodash",
        linewidth = 1
    )+
    owntheme

# export
ggsave(
    plot = plot_avg_flightact,
    file.path(
        output_path, "graphs/avg_flight_activity.png"
    ),
    width = 8,
    height = 5
)

###############################################################
# Descriptives Freight Transport                              #
###############################################################

#----------------------------------------------
# preparation
prep_fracht <- function(data){
    # drop unwanted rows and columns
    data <- data[15:38, 10:11]
    
    # rename columns
    colnames(data) <- c("airports", "freight_t")
    
    # return
    return(data)
}

# apply preparation function to each element in list
df_list_freight <- lapply(
    df_list_raw, prep_fracht
)

# turn list into data frame
freight_carry <- bind_rows(df_list_freight, .id = "id")

# some cleaning
freight_carry <- freight_carry |>
    mutate(
        # split id into month and year
        month = substr(id, start = 1, stop = 3),
        year = substr(id, start = 4, stop = 5),
        year = paste0("20", year),
        # create year_mon variable
        date = ymd(paste(year, month, "01", sep = "-")),
        year_mon = format(as.Date(date), "%Y-%m"),
        # make flight activity numeric
        freight_t = as.numeric(freight_t)
    ) |>
    # keep only relevant columns
    select(
        year_mon, airports, freight_t
    ) |>
    # restrict to 2018 and later
    filter(year_mon >= "2018-01") |>
    # restrict to airports of interest
    filter(
        airports %in% c(
            "Düsseldorf", "Frankfurt/Main", "Hamburg", "Hannover",
            "Köln/Bonn", "Leipzig/Halle", "München", "Nürnberg", "Stuttgart"
        )
    ) |>
    # sort by months
    arrange(year_mon)

#----------------------------------------------
# calculate average freight carried over time

avg_freight <- freight_carry |>
    group_by(year_mon) |>
    summarise(
        avg_freight_t = mean(freight_t, na.rm = TRUE)
    ) |>
    as.data.frame()

# calculate average for periods before and after lockdown
before_lock <- as.numeric(
        flight_act |>
            filter(year_mon <= "2020-03") |>
            summarise(
                avg_flight_act = mean(flight_activity, na.rm = TRUE)
            )
)

after_lock <- as.numeric(
        flight_act |>
            filter(year_mon > "2020-03") |>
            summarise(
                avg_flight_act = mean(flight_activity, na.rm = TRUE)
            )
)

###############################################################
# Mapping Contour Maps                                        #
###############################################################

#----------------------------------------------
# subset for Hamburg
contour_ham <- haupt_contour |>
    filter(
        icao == "EDDH"
    )

#----------------------------------------------
# prepare background map

# change to a format which works with extracting map
contour_ham4326 <- st_transform(contour_ham, crs = 4326)

# get bounding box of contour
bbox_ham <- st_bbox(contour_ham4326)

# make background map a bit larger than shape
bbox_ham <- c(bbox_ham[1:2] - 0.002, bbox_ham[3:4] + 0.002)

# rename bbox entries
names(bbox_ham) <- c("left", "bottom", "right", "top")

# get background map from Stamen
background_map <- ggmap::get_stamenmap(
    bbox = bbox_ham,
    color = "bw",
    force = TRUE,
    zoom = 12
)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(
        unlist(attr(map, "bb")), 
        c("ymin", "xmin", "ymax", "xmax")
    )

    # Convert the bbox to an sf polygon, transform it to 3857, 
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(
        st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857)
    )
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates 
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    
    return(map)
}

# apply function
background_map <- ggmap_bbox(background_map)

# transform contour to Googles CRS
contour_ham3857 <- st_transform(contour_ham, crs = 3857)

#----------------------------------------------
# plot original shape

# define colours
colors <- rev(met.brewer(name = "Greek", n = 5, type = "discrete"))

# plot
plot_ham_org <- ggmap::ggmap(background_map)+
    coord_sf(crs = st_crs(3857))+
    geom_sf(
        data = contour_ham3857,
        aes(fill = factor(DB_Low)),
        lwd = 0.7,
        color = "black", 
        inherit.aes = FALSE
    )+
    scale_fill_manual(
        values = c(
            "55" = colors[1],
            "60" = colors[2],
            "65" = colors[3],
            "70" = colors[4],
            "75" = colors[5]
        ),
        name = "Noise contour rings",
        labels = c(
            "55" = "Ring 1: 55-59dB",
            "60" = "Ring 2: 60-64dB",
            "65" = "Ring 3: 65-69dB",
            "70" = "Ring 4: 70-74dB",
            "75" = "Ring 5: \u2265 75dB"
        )
    )+
    xlab("")+
    ylab("")+
    theme(
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.8, fill = NA),
        legend.key = element_blank(),
        legend.position = c(0.82, 0.145),
        legend.text = element_text(size = 13),
        legend.background = element_rect(fill = alpha("white", alpha = 0.5)),
        legend.title = element_text(size = 13)
    )

# export
ggsave(
    plot = plot_ham_org,
    file.path(
        output_path, "graphs/contour_ham_orginal.png"
    ),
    dpi = owndpi
)

#----------------------------------------------
# plot like used in the analysis

# add ring number
contour_ham_own <- contour_ham |>
    mutate(
        rings = case_when(
            DB_Low == 55 ~ 1,
            DB_Low >= 60 ~ 2)
    )

# define colors
colors_own <- rev(met.brewer(name = "Greek", n = 2, type = "discrete"))

# transform contour to Stamen CRS
contour_ham_own3857 <- st_transform(contour_ham_own, crs = 3857)

# plot
plot_ham_own <- ggmap::ggmap(background_map)+
    coord_sf(crs = st_crs(3857))+
    geom_sf(
        data = contour_ham_own3857,
        aes(fill = factor(rings)),
        lwd = 0.7,
        color = "black",
        inherit.aes = FALSE
    )+
    scale_fill_manual(
        values = c(
            "1" = colors_own[1],
            "2" = colors_own[2]
        ),
        name = "Noise contour rings",
        labels = c(
            "1" = "Ring 1: 55-59dB",
            "2" = "Ring 2: \u2265 60dB"
        )
    )+
    xlab("")+
    ylab("")+
    theme(
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.8, fill = NA),
        legend.key = element_blank(),
        legend.position = c(0.82, 0.086),
        legend.text = element_text(size = 13),
        legend.background = element_rect(fill = alpha("white", alpha = 0.5)),
        legend.title = element_text(size = 13)
    )

# export manually, dimensions: 732 x 621
ggsave(
    plot = plot_ham_own,
    file.path(
        output_path, "graphs/contour_ham_own.png"
    ),
    dpi = owndpi
)

#----------------------------------------------
# plot treatment and control around noise shape

# make union
ham_union <- st_union(contour_ham)

# define neutral zone (1km)
ham_neutral_zone <- st_buffer(ham_union, dist = 1000)

# define buffer (5km)
ham_buffer <- st_buffer(ham_union, dist = 5000)

# buffer without the rest (for plotting)
ham_difference <- st_difference(ham_buffer, ham_neutral_zone)

# generate grid pattern for plotting
grid_pattern <- hatchedLayer(
    mode = "sfc",
    pattern = "grid",
    density = 3,
    x = ham_neutral_zone
)

# plot
map_groups <- tm_shape(ham_difference)+
    tm_polygons(col = "grey95")+
    tm_shape(grid_pattern)+
    tm_lines()+
    tm_shape(ham_neutral_zone)+
    tm_borders()+
    tm_shape(ham_union)+
    tm_polygons(col = "grey40")+
    tm_add_legend(
        type = "fill",
        col = "grey95", 
        size = 1,
        labels = "Control group"
    )+
    tm_add_legend(
        type = "symbol",
        shape = 12,
        col = "black", 
        size = 1,
        labels = "Neutral zone"
    )+
    tm_add_legend(
        type = "fill",
        col = "grey40",
        size = 1,
        labels = "Treatment group"
    )+
    tm_layout(legend.text.size = 0.75)

tmap_save(
    map_groups,
    file.path(
        output_path, "graphs/contour_ham_groups.png"
    )
)

###############################################################
# Plotting price development                                  #
###############################################################

#----------------------------------------------
# prepare

# apply preparation for estimation
housing_wk_prep <- prep_est(housing_wk)

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
    owntheme

# export
ggsave(
    plot = plot_wk,
    file.path(
        output_path, "graphs/wk_price_development.png"
    ), 
    width = 8,
    height = 6
)

# CONTINUE HERE
###############################################################
# Descriptives Housing Data                                   #
###############################################################

# functions ---------------------------------------------------------------

##### preping
prep_descriptives <- function(housing.df, price_variable){

  # select the main variables (part of regression)
  housing.df <- housing.df %>% select("alter", "wohnflaeche",
                                      "etage", "balkon", "objektzustand", 
                                      "einbaukueche", "garten", 
                                      "heizungsart", "ausstattung", "zimmeranzahl",
                                      "badezimmer", "distance_largcenter", 
                                      "distance_medcenter", "distance_smalcenter", "distance_industry", "distance_railroads", "distance_streets",
                                      "distance_main_airports_building", "con_ring0", "fir_lockdown", price_variable)
  
  # return
  return(housing.df)
}

##### descriptives
group_descriptives <- function(housing.df, name){
  # calculate descriptives for before and after lockdown
  des <- describeBy(housing.df, group = housing.df$fir_lockdown, mat = TRUE, digits = 3, fast = TRUE, na.rm = TRUE)
  
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
  
  # drop unneeded rows
  des <- des[des$variables != "con_ring01", ]
  des <- des[des$variables != "con_ring02", ]
  des <- des[des$variables != "fir_lockdown1", ]
  des <- des[des$variables != "fir_lockdown2", ]
  
  # rename mean and sd for merge
  names(des)[names(des) == "mean"] <- paste("mean_", name)
  names(des)[names(des) == "sd"] <- paste("sd_", name)
  
  # return
  return(des)
}


# -------------------------------------------------------------------------
# prepare

wk_des_data <- prep_descriptives(wk_prep, price_variable = "kaufpreis")


# -------------------------------------------------------------------------
# apartment purchase (WK)

# subset for treated and control group
wk_des_treat <- wk_des_data %>% filter(con_ring0 == 1)
wk_des_contr <- wk_des_data %>% filter(con_ring0 == 0)

# apply descriptive function
des_treat_wk <- group_descriptives(wk_des_treat, name = "wk_treat")
des_contr_wk <- group_descriptives(wk_des_contr, name = "wk_contr")

# -------------------------------------------------------------------------
# combine

des_table <-  merge(des_treat_wk, des_contr_wk, by = "variables")

# -------------------------------------------------------------------------
# export
write.xlsx(des_table, file.path(outputPath, "descriptives/summary_statistics.xlsx"), rowNames = FALSE)

###############################################################
# Number of Observations                                      #
###############################################################

num_obs <- function(df){
  # treatment group after lockdown
  ring8_after <- length(which(df$con_ring8 == 1 & df$fir_lockdown == 1))
  ring5_after <- length(which(df$con_ring5 == 1 & df$fir_lockdown == 1))
  ring0_after <- length(which(df$con_ring0 == 1 & df$fir_lockdown == 1))
  
  # treatment group before lockdown
  ring8_before <- length(which(df$con_ring8 == 1 & df$fir_lockdown == 0))
  ring5_before <- length(which(df$con_ring5 == 1 & df$fir_lockdown == 0))
  ring0_before <- length(which(df$con_ring0 == 1 & df$fir_lockdown == 0))
  
  # form data frame
  rings_num_obs <- as.data.frame(cbind(c("\u2265 60dB", "\u2265 55dB", "Total"),
                                       c(ring8_before, ring5_before, ring0_before),
                                       c(ring8_after, ring5_after, ring0_after)))
  
  colnames(rings_num_obs) <- c("rings", "treated_before_lockdown", "treated_after_lockdown")
  
  # return
  return(rings_num_obs)
 
}

wk_obs <- num_obs(wk_prep)

# export
write.xlsx(wk_obs, file = file.path(outputPath, "descriptives/obs_num_by_ring.xlsx"), rowNames = FALSE, sheetName = "WK")


###############################################################
# Prepare Noise data                                          #
###############################################################

dus <- rbind(dus19, dus20)
fra <- rbind(fra18, fra19, fra20, fra21)
ham <- rbind(ham19, ham20)
haj <- rbind(haj18, haj19, haj20, haj21)
lej <- rbind(lej19, lej20, lej21)
muc <- rbind(muc19, muc20)

# reassign zeros for MUC and DUS to NA
muc$leq_tag_flug[muc$leq_tag_flug == 0] <- NA
muc$leq_nacht_flug[muc$leq_nacht_flug == 0] <- NA
muc$l_den_flug[muc$l_den_flug == 0] <- NA

dus$leq_tag_flug[dus$leq_tag_flug == 0] <- NA
dus$leq_nacht_flug[dus$leq_nacht_flug == 0] <- NA

# create year month variable
dus$year_mon <- format(as.Date(dus$date, format = "%Y-%m-%d"), "%Y-%m")
fra$year_mon <- format(as.Date(fra$date, format = "%Y-%m-%d"), "%Y-%m")
ham$year_mon <- format(as.Date(ham$date, format = "%Y-%m-%d"), "%Y-%m")
haj$year_mon <- format(as.Date(haj$date, format = "%Y-%m-%d"), "%Y-%m")
lej$year_mon <- format(as.Date(lej$date, format = "%Y-%m-%d"), "%Y-%m")
muc$year_mon <- format(as.Date(muc$date, format = "%Y-%m-%d"), "%Y-%m")

# drop unneeded variables
dus <- dus[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
fra <- fra[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
ham <- ham[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
haj <- haj[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
lej <- lej[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]
muc <- muc[, c("year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug", "airport_name", "station", "lat_station", "lon_station")]

# merge everything together
noise_all <- rbind(dus, fra, ham, haj, lej, muc)


# average lden ------------------------------------------------------------

avg_lden <- noise_all %>% group_by(year_mon) %>% summarise(avg_lden_flug = mean(l_den_flug, na.rm = TRUE))

###############################################################
# Descriptives Noise Data                                     #
###############################################################

# add plot date
avg_lden$plot_date <- as.yearmon(avg_lden$year_mon)

# restrict June 2021 (end of housing data)
avg_lden <- avg_lden %>% filter(year_mon <= "2021-06")

# labels
month_labels <- c("Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
                  "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021")

##### plot
plot_noise <- ggplot(avg_lden)+
  geom_line(mapping = aes(x = plot_date, y = avg_lden_flug, group = 1), size = 1)+
  scale_x_yearmon(breaks = seq(min(avg_lden$plot_date), max(avg_lden$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Avg. Noise Level (in dB)",
                     breaks = seq(45, 56, 1))+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 45, yend = 56), linetype = 3, size = 1)+
  owntheme

plot_noise
ggsave(plot = plot_noise, file.path(outputPath, "graphs/avg_noise_levels.png"), width = 8, height = 5)


##### mean before and after
noise_all %>% filter(year_mon < "2020-03") %>% summarise(mean(l_den_flug, na.rm = TRUE))
noise_all %>% filter(year_mon >= "2020-03") %>% summarise(mean(l_den_flug, na.rm = TRUE))


###############################################################
# Share main airports in analysis - passengers                #
###############################################################
# share of traffic for main airports which are part of the analysis relative to
# all main airports

# loading -----------------------------------------------------------------

# create list with all file names
file.list <- list.files(path = paste0(dataFlug, "Flughaefen/", "Luftverkehr/"), pattern = "*.xlsx", full.names = TRUE)

# read in all excel files at once
df.list <- lapply(file.list, function(x){as.data.frame(read_excel(x, sheet = "1.1.2"))})

# extract the month names from the file list
months_names <- substring(file.list, first = 69, last = nchar(file.list) - 5)

# assign names to the data list
names(df.list) <- months_names


# preparation -------------------------------------------------------------

##### function
# prepare each sheet in list
prep_luftverkehr <- function(data){
  # drop unwanted rows and columns
  data <- data[15:38, 1:2]
  
  # rename columns
  colnames(data) <- c("airports", "flight_activity")
  
  # return
  return(data)
}

##### prepartion

# apply preparation function to each element in list
df.list <- lapply(df.list, prep_luftverkehr)

# turn list into data frame
flight_act <- bind_rows(df.list, .id = "id")

# split id into month and year
flight_act <- flight_act %>% mutate(month = substr(id, start = 1, stop = 3),
                                    year = substr(id, start = 4, stop = 5))

# modify year variable
flight_act$year <- paste0("20", flight_act$year)

# drop id
flight_act$id <- NULL

# create date variable
flight_act$date <- ymd(paste(flight_act$year, flight_act$month, "01", sep = "-"))
flight_act$year_mon <- format(as.Date(flight_act$date), "%Y-%m")

# drop date, month, year variables
flight_act$date <- NULL
flight_act$month <- NULL
flight_act$year <- NULL

# keep only those airports that are also part of analysis
#flight_act <- flight_act %>% filter(airports %in% c("Düsseldorf", "Frankfurt/Main", "Hamburg", 
#                                                    "Hannover", "Köln/Bonn", "Leipzig/Halle", "München", "Nürnberg", "Stuttgart"))

# sort by year-month
flight_act <- flight_act[order(flight_act$year_mon), ]

# make flight_activity numeric
flight_act$flight_activity <- as.numeric(flight_act$flight_activity)

# drop NAs
flight_act <- flight_act %>% filter(!is.na(airports))

# add year
flight_act$year <- format(as.yearmon(flight_act$year_mon), "%Y")


# shares ------------------------------------------------------------------

# total sum
sum_all_flight_act <- flight_act %>% group_by(year) %>% summarise(sum_all_main = sum(flight_activity, na.rm = TRUE))

# sum for main airports in analysis
sum_main_flight_act <- flight_act %>% filter(airports == "Düsseldorf" |
                                               airports == "Frankfurt/Main" |
                                               airports == "Hamburg" |
                                               airports == "Hannover" |
                                               airports == "Köln/Bonn" |
                                               airports == "Leipzig/Halle" |
                                               airports == "München" |
                                               airports == "Nürnberg" |
                                               airports == "Stuttgart") %>% 
                                               group_by(year) %>% summarise(sum_analysis_main = sum(flight_activity, na.rm = TRUE))

# combine both and calculate share
sum_flight_act <- merge(sum_all_flight_act, sum_main_flight_act, by = "year")
sum_flight_act <- sum_flight_act %>% mutate(share = (sum_analysis_main / sum_all_main) * 100)

# export
write.xlsx(sum_flight_act, file.path(outputPath, "descriptives/share_main_airports_analysis_passengers.xlsx"), row.names = FALSE)

###############################################################
# Share main airports in analysis - transport                 #
###############################################################

# loading -----------------------------------------------------------------

# create list with all file names
file.list <- list.files(path = paste0(dataFlug, "Flughaefen/", "Luftverkehr/"), pattern = "*.xlsx", full.names = TRUE)

# read in all excel files at once
df.list <- lapply(file.list, function(x){as.data.frame(read_excel(x, sheet = "1.1.2"))})

# extract the month names from the file list
months_names <- substring(file.list, first = 69, last = nchar(file.list) - 5)

# assign names to the data list
names(df.list) <- months_names

prep_fracht <- function(data){
  # drop unwanted rows and columns
  data <- data[15:38, 10:11]
  
  # rename columns
  colnames(data) <- c("airports", "freight_t")
  
  # return
  return(data)
}

# apply preparation function to each element in list
df.list <- lapply(df.list, prep_fracht)

# turn list into data frame
freight_carry <- bind_rows(df.list, .id = "id")

# split id into month and year
freight_carry <- freight_carry %>% mutate(month = substr(id, start = 1, stop = 3),
                                          year = substr(id, start = 4, stop = 5))

# modify year variable
freight_carry$year <- paste0("20", freight_carry$year)

# drop id
freight_carry$id <- NULL

# create date variable
freight_carry$date <- ymd(paste(freight_carry$year, freight_carry$month, "01", sep = "-"))
freight_carry$year_mon <- format(as.Date(freight_carry$date), "%Y-%m")

# drop date, month, year variables
freight_carry$date <- NULL
freight_carry$month <- NULL

# sort by year-month
freight_carry <- freight_carry[order(freight_carry$year_mon), ]

# make flight_activity numeric
freight_carry$freight_t <- as.numeric(freight_carry$freight_t)

# drop NAs
freight_carry <- freight_carry %>% filter(!is.na(airports))


# shares ------------------------------------------------------------------

# total sum
sum_all_freight_carry <- freight_carry %>% group_by(year) %>% summarise(sum_all_main = sum(freight_t, na.rm = TRUE))

# sum for main airports in analysis
sum_main_freight_carry <- freight_carry %>% filter(airports == "Düsseldorf" |
                                               airports == "Frankfurt/Main" |
                                               airports == "Hamburg" |
                                               airports == "Hannover" |
                                               airports == "Köln/Bonn" |
                                               airports == "Leipzig/Halle" |
                                               airports == "München" |
                                               airports == "Nürnberg" |
                                               airports == "Stuttgart") %>% 
  group_by(year) %>% summarise(sum_analysis_main = sum(freight_t, na.rm = TRUE))

# combine both and calculate share
sum_freight_carry <- merge(sum_all_freight_carry, sum_main_freight_carry, by = "year")
sum_freight_carry <- sum_freight_carry %>% mutate(share = (sum_analysis_main / sum_all_main) * 100)

# export
write.xlsx(sum_freight_carry, file.path(outputPath, "descriptives/share_main_airports_analysis_transport.xlsx"), row.names = FALSE)


# rank airports by transport ----------------------------------------------
# for 2019

rank_freight_carry <- freight_carry %>% 
  filter(year == 2019) %>% 
  group_by(airports) %>% 
  summarise(total_carry = sum(freight_t, na.rm = TRUE))

# sort
rank_freight_carry <- rank_freight_carry[rev(order(rank_freight_carry$total_carry)), ]

# add rank
rank_freight_carry$rank <- seq(1:nrow(rank_freight_carry))

# export
write.xlsx(rank_freight_carry, file.path(outputPath, "descriptives/rank_transport_airports.xlsx"), rowNames = FALSE)
