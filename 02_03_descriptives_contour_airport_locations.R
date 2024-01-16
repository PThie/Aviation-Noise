###############################################################
# Description                                                 #
###############################################################

# This file generates plots for the location of the included airports. It also
# generates examples of the contour data.

###############################################################
# load data                                                   #
###############################################################

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
        data_gebiete,
        "Bundesland/2019/VG250_LAN.shp"
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
# TODO: Needs to be updated since the access now works through an API.

# register stadia map
register_stadiamaps(api_key_stamen, write = FALSE)

# change to a format which works with extracting map
contour_ham4326 <- st_transform(contour_ham, crs = 4326)

# get bounding box of contour
bbox_ham <- st_bbox(contour_ham4326)

# make background map a bit larger than shape
bbox_ham <- c(bbox_ham[1:2] - 0.002, bbox_ham[3:4] + 0.002)

# rename bbox entries
names(bbox_ham) <- c("left", "bottom", "right", "top")

# get background map from Stamen
background_map <- ggmap::get_stadiamap(
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