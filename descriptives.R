###############################################################
# load data                                                   #
###############################################################

# load airport locations
airports <- read_excel(file.path(dataFlug, "Flughaefen/flughaefen_loc/flughaefen_germany_prepared.xlsx"))

# load state boundaries
bula <- st_read(file.path(dataGebiete, "Bundesland/2019/VG250_LAN.shp"))
bula <- st_transform(bula, crs = 32632)

# load contour maps
haupt_contour <- st_read(file.path(dataFlug, "Contour_Maps/Hauptflughaefen/Mair_Lden_17.shp")) 
haupt_contour <- st_transform(haupt_contour, crs = 32632)
haupt_contour <- haupt_contour[, c("ICAO", "DB_Low", "DB_High", "geometry")]

ball_contour <- st_read(file.path(dataFlug, "Contour_Maps/Ballungsraeume/Lden/Aggair_Lden_17.shp"))
ball_contour <- st_transform(ball_contour, crs = 32632)

# load property data
wk <- readRDS(file.path(dataFlug, "housing/wk_contour.rds"))
#hk <- readRDS(file.path(dataFlug, "housing/hk_contour.rds"))

# -------------------------------------------------------------------------
# load noise data
# Duesseldorf -------------------------------------------------------------
dus19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged_2019.dta"))
dus20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Duesseldorf/dus_complete_merged.dta"))

# Frankfurt ---------------------------------------------------------------
fra18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2018.dta"))
fra19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2019.dta"))
fra20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged.dta"))
fra21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Frankfurt/fra_complete_merged_2021.dta"))

# Hamburg -----------------------------------------------------------------
ham19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged_2019.dta"))
ham20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hamburg/ham_complete_merged.dta"))

# Hannover ----------------------------------------------------------------
haj18 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2018.dta"))
haj19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2019.dta"))
haj20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged.dta"))
haj21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Hannover/haj_complete_merged_2021.dta"))

# Leipzig -----------------------------------------------------------------
lej19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2019.dta"))
lej20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged.dta"))
lej21 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Leipzig/lej_complete_merged_2021.dta"))

# Muenchen ----------------------------------------------------------------
muc19 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged_2019.dta"))
muc20 <- haven::read_dta(file.path(dataFlug, "Hauptflughaefen_Laerm/Muenchen/muc_complete_merged.dta"))


###############################################################
# general preparation                                         #
###############################################################

# prepare contour ---------------------------------------------------------

# add ICAO to additional airports
ball_contour$ICAO <- NA
ball_contour$ICAO[ball_contour$Agglomerat == "Essen"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mülheim an der Ruhr"] <- "EDLE"
ball_contour$ICAO[ball_contour$Agglomerat == "Mannheim"] <- "EDFM"
ball_contour$ICAO[ball_contour$Agglomerat == "Dortmund"] <- "EDLW"
ball_contour$ICAO[ball_contour$Agglomerat == "Bremen"] <- "EDDW"
ball_contour$ICAO[ball_contour$Agglomerat == "Mainz"] <- "EDFZ"
ball_contour$ICAO[ball_contour$Agglomerat == "Dresden"] <- "EDDC"

ball_contour <- ball_contour[, c("ICAO", "DB_Low", "DB_High", "geometry")]

###############################################################
# Mapping Airports                                            #
###############################################################

# keep only the main airports
airports_main <- airports[airports$mainair == 1,]

# keep only the airports which included in the study (11)
airports_main <- subset(airports_main, airports_main$IATA_code == "DUS" | airports_main$IATA_code == "HAM" |
                          airports_main$IATA_code == "MUC" | airports_main$IATA_code == "FRA" |
                          airports_main$IATA_code == "HAJ" | airports_main$IATA_code == "LEJ" |
                          airports_main$IATA_code == "STR" | airports_main$IATA_code == "NUE" |
                          airports_main$IATA_code == "CGN")

# keep only relevant columns
airports_main <- airports_main[, c("name", "city", "IATA_code", "longitude", "latitude")]

# rename Muenchen Airport
airports_main$name[8] <- "München Airport"

# add label
airports_main$labels <- c("Stuttgart", "Nuremberg", "Dusseldorf", "Hannover", "Leipzig", "Frankfurt", "Cologne", "Hamburg", "Munich")

# make sf
airports_main <- st_as_sf(airports_main, coords = c("longitude", "latitude"), crs = 4326)
airports_main <- st_transform(airports_main, crs = 32632)

# plot 
map_airports <- tm_shape(bula)+
  tm_borders(lwd = 1, col = "gray")+
  tm_shape(airports_main)+
  tm_dots(size = 0.22)+
  tm_text(text = "labels", 
          xmod = 1,
          ymod = -0.5,
          fontface = "bold",
          scale = 0.9)

map_airports
tmap_save(map_airports, file = file.path(outputPath, "graphs/map_airport_locations.png"))


# plot main and agglomeration airports ------------------------------------

# select airports in agglomeration areas
airports_agg <- airports %>% filter(ICAO_code == "EDLE" |
                                      ICAO_code == "EDFM" |
                                      ICAO_code == "EDLW" |
                                      ICAO_code == "EDDW" |
                                      ICAO_code == "EDFZ" |
                                      ICAO_code == "EDDC")

# keep only relevant columns
airports_agg <- airports_agg[, c("name", "city", "IATA_code", "longitude", "latitude")]

# make sf
airports_agg <- st_as_sf(airports_agg, coords = c("longitude", "latitude"), crs = 4326)
airports_agg <- st_transform(airports_agg, crs = 32632)

map_main_agg_airports <- tm_shape(bula)+
  tm_borders(lwd = 1, col = "gray")+
  tm_shape(airports_agg)+
  tm_dots(size = 0.22, shape = 22)+
  tm_text(text = "city",
          xmod = c(-0.5, -0.5, 1, 1, 1, 1),
          ymod = c(0.5, 0.5, -0.5, 0.5, -0.5, -0.5),
          fontface = "bold",
          scale = 0.9)+
  tm_shape(airports_main)+
  tm_dots(size = 0.22)+
  tm_text(text = "labels", 
          xmod = 1,
          ymod = -0.5,
          fontface = "bold",
          scale = 0.9)+
  tm_add_legend(type = "symbol",
                shape = c(21, 22),
                col = "black",
                labels = c("Main airports", "Agglomeration airports"),
                border.lwd = 1,
                border.alpha = 1)+
  tm_layout(legend.frame = TRUE,
            legend.position = c("right", "top"))

map_main_agg_airports
tmap_save(map_main_agg_airports, file = file.path(outputPath, "graphs/map_main_agg_airport_locations.png"))


###############################################################
# Descriptives Flight Activity                                #
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
flight_act <- flight_act %>% filter(airports %in% c("Düsseldorf", "Frankfurt/Main", "Hamburg", 
                                             "Hannover", "Köln/Bonn", "Leipzig/Halle", "München", "Nürnberg", "Stuttgart"))

# sort by year-month
flight_act <- flight_act[order(flight_act$year_mon), ]

# make flight_activity numeric
flight_act$flight_activity <- as.numeric(flight_act$flight_activity)

# restrict to 2018 and later
flight_act <- flight_act %>% filter(year_mon >= "2018-01")

# calculations ------------------------------------------------------------

# average flight activity by month
avg_flight_activity_df <- flight_act %>% group_by(year_mon) %>% summarise(avg_flight_activity = mean(flight_activity, na.rm = TRUE))

# -------------------------------------------------------------------------
# month labels
month_labels <- c("Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
                  "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021", "Sep 2021")

# plotting ----------------------------------------------------------------
# add plot date
avg_flight_activity_df$plot_date <- as.yearmon(avg_flight_activity_df$year_mon)
avg_flight_activity_df$ref_num_date <- as.numeric(avg_flight_activity_df$plot_date)

# define own theme
owntheme <- theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 17, vjust = 2),
                  axis.text.y = element_text(size = 15),
                  panel.background = element_rect(colour = "white", fill = "white"),
                  axis.line = element_line(size = 0.5, linetype = "solid", color = "black"),
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 11),
                  axis.ticks.length = unit(0.25, "cm"))

# plot average flight activity
plot_avg_flightact <- ggplot(avg_flight_activity_df)+
  geom_line(mapping = aes(x = plot_date, y = avg_flight_activity, group = 1), size = 1)+
  scale_x_yearmon(breaks = seq(min(avg_flight_activity_df$plot_date), max(avg_flight_activity_df$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Avg. Flight Activity",
                     breaks = seq(2000, 18000, 2000),
                     labels = scales::comma)+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 2000, yend = 18000), linetype = 3, size = 1)+
  owntheme

plot_avg_flightact
ggsave(plot = plot_avg_flightact, file.path(outputPath, "graphs/avg_flight_activity.png"), width = 8, height = 5)

# -------------------------------------------------------------------------
# flight activity by airport

# add plot data
flight_act$plot_date <- as.yearmon(flight_act$year_mon)

# define colors
pal <- MetBrewer::met.brewer(name = "Tiepolo", n = 9)

plot_flight_act_airports <- ggplot(flight_act)+
  geom_line(mapping = aes(x = plot_date, y = flight_activity, group = airports, col = airports, lwd = airports))+
  scale_x_yearmon(breaks = seq(min(flight_act$plot_date), max(flight_act$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Flight Activity",
                     breaks = seq(0, 45000, 5000),
                     labels = scales::comma)+
  scale_color_manual(values = pal,
                     name = "Airports", 
                     labels = c("Düsseldorf" = "Dusseldorf",
                                "Frankfurt/Main" = "Frankfurt",
                                "Hamburg" = "Hamburg",
                                "Hannover" = "Hannover",
                                "Köln/Bonn" = "Cologne",
                                "Leipzig/Halle" = "Leipzig",
                                "München" = "Munich",
                                "Nürnberg" = "Nuremberg",
                                "Stuttgart" = "Stuttgart"))+
  scale_size_manual(values = c(1, 1, 1, 1, 2, 2, 1, 1, 1),
                    name = "Airports",
                    labels = c("Düsseldorf" = "Dusseldorf",
                               "Frankfurt/Main" = "Frankfurt",
                               "Hamburg" = "Hamburg",
                               "Hannover" = "Hannover",
                               "Köln/Bonn" = "Cologne",
                               "Leipzig/Halle" = "Leipzig",
                               "München" = "Munich",
                               "Nürnberg" = "Nuremberg",
                               "Stuttgart" = "Stuttgart"))+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 0, yend = 48000), linetype = 3, size = 1)+
  owntheme

plot_flight_act_airports
ggsave(plot = plot_flight_act_airports, file.path(outputPath, "graphs/flight_activity_airports.png"), width = 7, height = 5)
 

###############################################################
# Descriptives Freight Transport                              #
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
freight_carry$year <- NULL

# keep only those airports that are also part of analysis
freight_carry <- freight_carry %>% filter(airports %in% c("Düsseldorf", "Frankfurt/Main", "Hamburg", 
                                                          "Hannover", "Köln/Bonn", "Leipzig/Halle", "München", "Nürnberg", "Stuttgart"))

# sort by year-month
freight_carry <- freight_carry[order(freight_carry$year_mon), ]

# make flight_activity numeric
freight_carry$freight_t <- as.numeric(freight_carry$freight_t)

# restrict to 2018 and later
freight_carry <- freight_carry %>% filter(year_mon >= "2018-01")


# calculations ------------------------------------------------------------

# average flight activity by month
avg_freight_df <- freight_carry %>% group_by(year_mon) %>% summarise(avg_freight_t = mean(freight_t, na.rm = TRUE))

# average freight carry by month without Frankfurt
avg_freight_withoutFra <- freight_carry %>% group_by(year_mon) %>% filter(airports != "Frankfurt/Main") %>% 
  summarise(avg_freight_t = mean(freight_t, na.rm = TRUE))

# -------------------------------------------------------------------------

# define plot date
avg_freight_df$plot_date <- as.yearmon(avg_freight_df$year_mon)
avg_freight_withoutFra$plot_date <- as.yearmon(avg_freight_withoutFra$year_mon)

# plot average freight carry
plot_avg_freight <- ggplot(avg_freight_df)+
  geom_line(mapping = aes(x = plot_date, y = avg_freight_t, group = 1), size = 1)+
  scale_x_yearmon(breaks = seq(min(avg_freight_df$plot_date), max(avg_freight_df$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Avg. Freight Carry (in t)",
                     breaks = seq(35000, 60000, 5000),
                     labels = scales::comma)+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 35000, yend = 60000), linetype = 3, size = 1)+
  owntheme

plot_avg_freight
ggsave(plot = plot_avg_freight, file.path(outputPath, "graphs/avg_freight_carry.png"), width = 8, height = 5)

# plot average freight carry without Frankfurt
plot_avg_freight_withoutFra <- ggplot(avg_freight_withoutFra)+
  geom_line(mapping = aes(x = plot_date, y = avg_freight_t, group = 1), size = 1)+
  scale_x_yearmon(breaks = seq(min(avg_freight_withoutFra$plot_date), max(avg_freight_withoutFra$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Avg. Freight Carry (in t)",
                     breaks = seq(20000, 35000, 5000),
                     labels = scales::comma)+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 20000, yend = 35000), linetype = 3, size = 1)+
  owntheme

plot_avg_freight_withoutFra
ggsave(plot = plot_avg_freight, file.path(outputPath, "graphs/avg_freight_carry_withoutFra.png"), width = 8, height = 5)

# -------------------------------------------------------------------------
# freight carry by airport

# add plot data
freight_carry$plot_date <- as.yearmon(freight_carry$year_mon)

# define colors
pal <- MetBrewer::met.brewer(name = "Tiepolo", n = 9)

plot_freight_airports <- ggplot(freight_carry)+
  geom_line(mapping = aes(x = plot_date, y = freight_t, group = airports, col = airports, lwd = airports))+
  scale_x_yearmon(breaks = seq(min(freight_carry$plot_date), max(freight_carry$plot_date), 0.34),
                  labels = month_labels)+
  scale_y_continuous(name = "Freight Carry (in t)",
                     breaks = seq(0, 200000, 20000),
                     labels = scales::comma)+
  scale_color_manual(values = pal,
                     name = "Airports", 
                     labels = c("Düsseldorf" = "Dusseldorf",
                                "Frankfurt/Main" = "Frankfurt",
                                "Hamburg" = "Hamburg",
                                "Hannover" = "Hannover",
                                "Köln/Bonn" = "Cologne",
                                "Leipzig/Halle" = "Leipzig",
                                "München" = "Munich",
                                "Nürnberg" = "Nuremberg",
                                "Stuttgart" = "Stuttgart"))+
  scale_size_manual(values = c(1, 1, 1, 1, 2, 2, 1, 1, 1),
                    name = "Airports",
                    labels = c("Düsseldorf" = "Dusseldorf",
                               "Frankfurt/Main" = "Frankfurt",
                               "Hamburg" = "Hamburg",
                               "Hannover" = "Hannover",
                               "Köln/Bonn" = "Cologne",
                               "Leipzig/Halle" = "Leipzig",
                               "München" = "Munich",
                               "Nürnberg" = "Nuremberg",
                               "Stuttgart" = "Stuttgart"))+
  geom_segment(aes(x = 2020.167, xend = 2020.167, y = 0, yend = 220000), linetype = 3, size = 1)+
  owntheme

plot_freight_airports
ggsave(plot = plot_freight_airports, file.path(outputPath, "graphs/freight_carry_airports.png"), width = 7, height = 5)

###############################################################
# Mapping Contour Maps                                        #
###############################################################

# subset for Hamburg
contour_ham <- haupt_contour[haupt_contour$ICAO == "EDDH",]


# prepare background map --------------------------------------------------

# change to a format which works with extracting map
contour_ham4326 <- st_transform(contour_ham, crs = 4326)

# get bounding box of contour
bbox_ham <- st_bbox(contour_ham4326)

# make background map a bit larger than shape
bbox_ham <- c(bbox_ham[1:2]-0.002, bbox_ham[3:4]+0.002)

# rename bbox entries
names(bbox_ham) <- c("left", "bottom", "right", "top")

# get background map from Stamen
background_map <- get_stamenmap(bbox = bbox_ham, color = "bw", force = TRUE,
                          zoom = 12) 


# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
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


# plot --------------------------------------------------------------------
##### plot orginal contour

# define colours
colors <- rev(met.brewer(name = "Greek", n = 5, type = "discrete"))

# plot
plot_ham_org <- ggmap(background_map)+
  coord_sf(crs = st_crs(3857))+
  geom_sf(data = contour_ham3857, aes(fill = factor(DB_Low)), lwd = 0.7, color = "black", inherit.aes = FALSE)+
  scale_fill_manual(values = c("55" = colors[1],
                               "60" = colors[2],
                               "65" = colors[3],
                               "70" = colors[4],
                               "75" = colors[5]),
                    name = "Noise contour rings",
                    labels = c("55" = "Ring 1: 55-59dB",
                               "60" = "Ring 2: 60-64dB",
                               "65" = "Ring 3: 65-69dB",
                               "70" = "Ring 4: 70-74dB",
                               "75" = "Ring 5: \u2265 75dB"))+
  xlab("")+
  ylab("")+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.8, fill = NA),
        legend.key = element_blank(),
        legend.position = c(0.82, 0.145),
        legend.text = element_text(size = 13),
        legend.background = element_rect(fill = alpha("white", alpha = 0.5)),
        legend.title = element_text(size = 13))

plot_ham_org
# export manually, dimensions: 732 x 621
#ggsave(plot = plot_ham_org, file.path(outputPath, "graphs/contour_ham_orginal.png"), width = 8, height = 8, units = "cm")


# plot --------------------------------------------------------------------
##### plot like used in analysis

# add ring number
contour_ham_own <- contour_ham %>% mutate(rings = case_when(DB_Low == 55 ~ 1,
                                                        DB_Low >= 60 ~ 2))

# define colors
colors_own <- rev(met.brewer(name = "Greek", n = 2, type = "discrete"))


# transform contour to Stamen CRS
contour_ham_own3857 <- st_transform(contour_ham_own, crs = 3857)


# plot
plot_ham_own <- ggmap(background_map)+
  coord_sf(crs = st_crs(3857))+
  geom_sf(data = contour_ham_own3857, aes(fill = factor(rings)), lwd = 0.7, color = "black", inherit.aes = FALSE)+
  scale_fill_manual(values = c("1" = colors_own[1],
                               "2" = colors_own[2]),
                    name = "Noise contour rings",
                    labels = c("1" = "Ring 1: 55-59dB",
                               "2" = "Ring 2: \u2265 60dB"))+
  xlab("")+
  ylab("")+
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", size = 0.8, fill = NA),
        legend.key = element_blank(),
        legend.position = c(0.82, 0.086),
        legend.text = element_text(size = 13),
        legend.background = element_rect(fill = alpha("white", alpha = 0.5)),
        legend.title = element_text(size = 13))

plot_ham_own

# export manually, dimensions: 732 x 621



# plot --------------------------------------------------------------------
##### plot for groups

# make union
ham_union <- st_union(contour_ham)

# define neutral zone (1km)
ham_neutral_zone <- st_buffer(ham_union, dist = 1000)

# define buffer (5km)
ham_buffer <- st_buffer(ham_union, dist = 5000)

# buffer without the rest (for plotting)
ham_difference <- st_difference(ham_buffer, ham_neutral_zone)

# generate grid pattern for plotting
pacman::p_load(cartography)
grid_pattern <- hatchedLayer(mode = "sfc", pattern = "grid", density = 3, x = ham_neutral_zone)

# plot
map_groups <- tm_shape(ham_difference)+
  tm_polygons(col = "grey95")+
  tm_shape(grid_pattern)+
  tm_lines()+
  tm_shape(ham_neutral_zone)+
  tm_borders()+
  tm_shape(ham_union)+
  tm_polygons(col = "grey40")+
  tm_add_legend(type = "fill",
                col = "grey95", 
                size = 1,
                labels = "Control group")+
  tm_add_legend(type = "symbol",
                shape = 12,
                col = "black", 
                size = 1,
                labels = "Neutral zone")+
  tm_add_legend(type = "fill",
                col = "grey40",
                size = 1,
                labels = "Treatment group")+
  tm_layout(legend.text.size = 0.75)

map_groups
#tmap_save(map_groups, file.path(outputPath, "graphs/contour_ham_groups.png"))

###############################################################
# Plotting price development                                  #
###############################################################


# prepare housing data ----------------------------------------------------

##### function
prep_housing <- function(housing_data){
  # drop geometry
  housing_data <- st_drop_geometry(housing_data)
  
  # restrict to 5km to contour ring
  housing_data <- housing_data %>% filter(distance_main_airports <= 5)
  
  housing_data <- housing_data %>% filter(distance_main_airports >= 1 | distance_main_airports == 0)
  
  # drop Airports Tegel and Schoenefeld
  housing_data <- housing_data %>% filter(closest_main_airports != "EDDT" & closest_main_airports != "EDDB")
  
  # drop March 2020
  housing_data <- housing_data %>% filter(year_mon != "2020-03")
  
  # add quarters to the data
  housing_data$plot_date <- as.yearmon(housing_data$year_mon)
  housing_data$quarter <- as.yearqtr(housing_data$plot_date)
  housing_data$plot_date <- NULL
  
  # add ring for 60dB and above
  housing_data <- housing_data %>% mutate(con_ring8 = case_when(con_ring1 == 1 | con_ring2 == 1 | con_ring3 == 1 | con_ring4 == 1 ~ 1,
                                                                con_ring5 == 1 ~ 0))
  
  housing_data$con_ring8[is.na(housing_data$con_ring8)] <- 0
  
  # return
  return(housing_data)
}

##### apply function
wk_prep <- prep_housing(wk)

# average price -----------------------------------------------------------

# select only prices and time variable
# set type for each data set
wk_prices <- wk_prep %>% select(price_sqmeter, kaufpreis, quarter, year_mon, con_ring0)

# summary function
sum_price_quarter <- function(df, variable, name){
  sum_df <- df %>% group_by(quarter, con_ring0) %>% summarise(mean_price_sqmeter = mean({{variable}}, na.rm = TRUE))
  
  # rename
  colnames(sum_df) <- c("quarter", "con_ring0", paste0("mean_price_sqmeter_", name))
  
  # sort by con_ring0
  sum_df <- sum_df[order(sum_df$con_ring0),]
  
  # return
  return(sum_df)
}

# line thickness
linethick = 1

# mytheme
mytheme <- theme(panel.background = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                 axis.text.y = element_text(size = 15),
                 axis.line = element_line(colour = "black"),
                 axis.title.y = element_text(size = 18),
                 legend.text = element_text(size = 15),
                 axis.ticks.length = unit(0.25, "cm"),
                 legend.key = element_blank())

# WK ----------------------------------------------------------------------

# subset for treated and control and summarise
wk_sum <- wk_prices %>% summarise(sum_price_quarter(., variable = price_sqmeter, name = "apart"))


##### plots
# apartments
plot_wk <- ggplot(data = wk_sum, mapping = aes(x = quarter, group = factor(con_ring0)))+
  geom_line(mapping = aes(y = mean_price_sqmeter_apart, linetype = factor(con_ring0)), lwd = linethick)+
  scale_linetype_manual(values = c("0" = "solid",
                                   "1" = "twodash"),
                        labels = c("0" = "< 55dB (control)",
                                   "1" = "\u2265 55dB (treated)"),
                        name = "")+
  scale_y_continuous(breaks = seq(3000, 5600, 500),
                     labels = scales::comma)+
  scale_x_yearqtr(format = "%Y Q%q", 
                  limits = c(min(wk_sum$quarter), max(wk_sum$quarter)),
                  breaks = seq(min(wk_sum$quarter), max(wk_sum$quarter), 0.25))+
  labs(x = "", y = expression(paste("Price per sq. meter [€/", m^{2}, "]")))+ 
  geom_segment(aes(x = 2020.00, xend = 2020.00, y = 3000, yend = 5600), linetype = 3, lwd = 0.9)+
  mytheme

plot_wk  
ggsave(plot = plot_wk, file.path(outputPath, "graphs/wk_price_development.png"), width = 8, height = 6)





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
