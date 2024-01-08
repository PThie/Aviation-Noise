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

#----------------------------------------------
# load noise data

noise_data <- read.fst(
    file.path(
        data_path, "Hauptflughaefen_Laerm/avg_month.fst"
    )
)

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
        col = "grey70",
        linewidth = 0.8
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
    file_list,
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
    "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021", "Sep 2021",
    "Jan 2022"
)

# define plot date
avg_flight_activity <- avg_flight_activity |>
    mutate(
        plot_date = as.yearmon(year_mon)
    )

# restrict to June 2022
# because housing data ends there
avg_flight_activity <- avg_flight_activity |>
    filter(year_mon <= "2022-06")

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

###############################################################
# Descriptives Noise Data                                     #
###############################################################

# add plot date
noise_data$plot_date <- as.yearmon(noise_data$year_mon)

# restrict June 2021 (end of housing data)
noise_data <- noise_data |>
    filter(year_mon <= "2021-06")

# labels
month_labels <- c(
    "Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
    "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021"
)

# calculate average before lockdown
before_lock <- as.numeric(
    noise_data |>
        filter(year_mon <= "2020-03") |>
        summarise(
            avg = mean(avg_l_den_flug, na.rm = TRUE)
        )
)

# calculate average after lockdown
after_lock <- as.numeric(
    noise_data |>
        filter(year_mon > "2020-03") |>
        summarise(
            avg = mean(avg_l_den_flug, na.rm = TRUE)
        )
)

##### plot
plot_noise <- ggplot(noise_data)+
    geom_line(
        mapping = aes(x = plot_date, y = avg_l_den_flug, group = 1),
        size = 1
    )+
    scale_x_yearmon(
        breaks = seq(min(noise_data$plot_date), max(noise_data$plot_date), 0.34),
        labels = month_labels
    )+
    scale_y_continuous(
        name = "Avg. Noise Level (in dB)",
        breaks = seq(45, 56, 1)
    )+
    geom_segment(
        mapping = aes(x = lock, xend = lock, y = 45, yend = 56),
        linetype = 3,
        size = 1
    )+
    geom_segment(
        mapping = aes(
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

ggsave(
    plot = plot_noise,
    file.path(output_path, "graphs/avg_noise_levels.png"),
    width = 8,
    height = 5
)

###############################################################
# Share main airports in analysis - passengers                #
###############################################################
# share of traffic for main airports which are part of the analysis relative to
# all main airports

#----------------------------------------------
# reload data
# turn list into data frame
flight_act_complete <- bind_rows(df_list_people, .id = "id")

# some cleaning
flight_act_complete <- flight_act_complete |>
    mutate(
        # split id into month and year
        month = substr(id, start = 1, stop = 3),
        year = substr(id, start = 4, stop = 5),
        year = paste0("20", year),
        # create year_mon variable
        date = ymd(paste(year, month, "01", sep = "-")),
        year_mon = format(as.Date(date), "%Y-%m"),
        year = format(as.yearmon(year_mon), "%Y"),
        # make flight activity numeric
        flight_activity = as.numeric(flight_activity)
    ) |>
    # keep only relevant columns
    select(
        year_mon, year, airports, flight_activity
    ) |>
    # restrict to 2018 and later
    filter(year_mon >= "2018-01") |>
    # sort by months
    arrange(year_mon)

#----------------------------------------------
# total sum
sum_all_flight_act <- flight_act_complete |>
    group_by(year) |>
    summarise(sum_all_main = sum(flight_activity, na.rm = TRUE)) |>
    as.data.frame()

# sum for main airports in analysis
sum_main_flight_act <- flight_act_complete |>
    filter(
        airports == "Düsseldorf" |
        airports == "Frankfurt/Main" |
        airports == "Hamburg" |
        airports == "Hannover" |
        airports == "Köln/Bonn" |
        airports == "Leipzig/Halle" |
        airports == "München" |
        airports == "Nürnberg" |
        airports == "Stuttgart"
    ) |>
    group_by(year) |>
    summarise(
        sum_analysis_main = sum(flight_activity, na.rm = TRUE)
    ) |>
    as.data.frame()

# combine both and calculate share
sum_flight_act <- merge(
    sum_all_flight_act,
    sum_main_flight_act,
    by = "year"
)
sum_flight_act <- sum_flight_act |>
    mutate(
        share = (sum_analysis_main / sum_all_main) * 100
    )

# export
write.xlsx(
    sum_flight_act,
    file.path(
        output_path,
        "descriptives/share_main_airports_analysis_passengers.xlsx"
    ),
    rowNames = FALSE
)

###############################################################
# Share main airports in analysis - transport                 #
###############################################################

# turn list into data frame
freight_carry_complete <- bind_rows(df_list_freight, .id = "id")

# some cleaning
freight_carry_complete <- freight_carry_complete |>
    mutate(
        # split id into month and year
        month = substr(id, start = 1, stop = 3),
        year = substr(id, start = 4, stop = 5),
        year = paste0("20", year),
        # create year_mon variable
        date = ymd(paste(year, month, "01", sep = "-")),
        year_mon = format(as.Date(date), "%Y-%m"),
        year = format(as.yearmon(year_mon), "%Y"),
        # make flight activity numeric
        freight_t = as.numeric(freight_t)
    ) |>
    # keep only relevant columns
    select(
        year_mon, year, airports, freight_t
    ) |>
    # restrict to 2018 and later
    filter(year_mon >= "2018-01") |>
    # sort by months
    arrange(year_mon)

# shares ------------------------------------------------------------------

# total sum
sum_all_freight_carry <- freight_carry_complete |>
    group_by(year) |>
    summarise(
        sum_all_main = sum(freight_t, na.rm = TRUE)
    ) |>
    as.data.frame()

# sum for main airports in analysis
sum_main_freight_carry <- freight_carry_complete |>
    filter(
        airports == "Düsseldorf" |
        airports == "Frankfurt/Main" |
        airports == "Hamburg" |
        airports == "Hannover" |
        airports == "Köln/Bonn" |
        airports == "Leipzig/Halle" |
        airports == "München" |
        airports == "Nürnberg" |
        airports == "Stuttgart"
    ) |>
    group_by(year) |>
    summarise(sum_analysis_main = sum(freight_t, na.rm = TRUE)) |>
    as.data.frame()

# combine both and calculate share
sum_freight_carry <- merge(
    sum_all_freight_carry,
    sum_main_freight_carry,
    by = "year"
)
sum_freight_carry <- sum_freight_carry |>
    mutate(
        share = (sum_analysis_main / sum_all_main) * 100
    )

# export
write.xlsx(
    sum_freight_carry,
    file.path(
        output_path,
        "descriptives/share_main_airports_analysis_transport.xlsx"
    ),
    rowNames = FALSE
)