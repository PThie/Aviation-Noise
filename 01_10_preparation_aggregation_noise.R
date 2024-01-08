###############################################################
# Description                                                 #
###############################################################

# This file aggregates the daily noise data to monthly data.

###############################################################
# load data                                                   #
###############################################################

# function
read_noise_airports <- function(city, iata, year) {
    filename <- paste0(
        iata, "_complete_merged_", year, ".dta"
    )
    dta <- haven::read_dta(
        file.path(
            data_path, "Hauptflughaefen_Laerm", city, filename
        )
    )
    return(dta)
}

# Duesseldorf
dus19 <- read_noise_airports(city = "Duesseldorf", iata = "dus", year = 2019)
dus20 <- read_noise_airports(city = "Duesseldorf", iata = "dus", year = 2020)

# Frankfurt
fra18 <- read_noise_airports(city = "Frankfurt", iata = "fra", year = 2018)
fra19 <- read_noise_airports(city = "Frankfurt", iata = "fra", year = 2019)
fra20 <- read_noise_airports(city = "Frankfurt", iata = "fra", year = 2020)
fra21 <- read_noise_airports(city = "Frankfurt", iata = "fra", year = 2021)

# Hamburg
ham19 <- read_noise_airports(city = "Hamburg", iata = "ham", year = 2019)
ham20 <- read_noise_airports(city = "Hamburg", iata = "ham", year = 2020)

# Hannover
haj18 <- read_noise_airports(city = "Hannover", iata = "haj", year = 2018)
haj19 <- read_noise_airports(city = "Hannover", iata = "haj", year = 2019)
haj20 <- read_noise_airports(city = "Hannover", iata = "haj", year = 2020)
haj21 <- read_noise_airports(city = "Hannover", iata = "haj", year = 2021)

# Leipzig
lej19 <- read_noise_airports(city = "Leipzig", iata = "lej", year = 2019)
lej20 <- read_noise_airports(city = "Leipzig", iata = "lej", year = 2020)
lej21 <- read_noise_airports(city = "Leipzig", iata = "lej", year = 2021)

# Muenchen
muc19 <- read_noise_airports(city = "Muenchen", iata = "muc", year = 2019)
muc20 <- read_noise_airports(city = "Muenchen", iata = "muc", year = 2020)

###############################################################
# Preparation                                                 #
###############################################################

#----------------------------------------------
# combine years
dus <- rbind(dus19, dus20)
fra <- rbind(fra18, fra19, fra20, fra21)
ham <- rbind(ham19, ham20)
haj <- rbind(haj18, haj19, haj20, haj21)
lej <- rbind(lej19, lej20, lej21)
muc <- rbind(muc19, muc20)

#----------------------------------------------
# reassign zeros for MUC and DUS to NA
muc <- muc |>
    mutate(
        across(
            .cols = c("leq_tag_flug", "leq_nacht_flug", "l_den_flug"),
            ~ replace(.x, .x == 0, NA)
        )
    )

dus <- dus |>
    mutate(
        across(
            .cols = c("leq_tag_flug", "leq_nacht_flug"),
            ~ replace(.x, .x == 0, NA)
        )
    )

#----------------------------------------------
# create year month variable
dus$year_mon <- format(as.Date(dus$date, format = "%Y-%m-%d"), "%Y-%m")
fra$year_mon <- format(as.Date(fra$date, format = "%Y-%m-%d"), "%Y-%m")
ham$year_mon <- format(as.Date(ham$date, format = "%Y-%m-%d"), "%Y-%m")
haj$year_mon <- format(as.Date(haj$date, format = "%Y-%m-%d"), "%Y-%m")
lej$year_mon <- format(as.Date(lej$date, format = "%Y-%m-%d"), "%Y-%m")
muc$year_mon <- format(as.Date(muc$date, format = "%Y-%m-%d"), "%Y-%m")

#----------------------------------------------
# drop unneeded variables
cols <- c(
    "year_mon", "leq_tag_flug", "leq_nacht_flug", "l_den_flug",
    "airport_name", "station", "lat_station", "lon_station"
)

dus <- dus |>
    select(all_of(cols))
fra <- fra |>
    select(all_of(cols))
ham <- ham |>
    select(all_of(cols))
haj <- haj |>
    select(all_of(cols))
lej <- lej |>
    select(all_of(cols))
muc <- muc |>
    select(all_of(cols))

###############################################################
# Aggregation                                                 #
###############################################################

# merge everything together and aggregate based just on month
all <- rbind(dus, fra, ham, haj, lej, muc)

#----------------------------------------------
# aggregation function
agg_fun <- function(
    data,
    group = c("month", "airports", "individual_airports"),
    airport_name = NA
    ) {
    # define columns
    cols <- list(
        avg_leq_tag_flug = data$leq_tag_flug,
        avg_l_den_flug = data$l_den_flug,
        avg_leq_nacht_flug = data$leq_nacht_flug,
        lat_station = data$lat_station,
        lon_station = data$lon_station
    )
    # define grouping
    if(group == "month") {
        grouping <- list(year_mon = data$year_mon)
    } else if (group == "airports") {
        grouping <- list(
            year_mon = data$year_mon,
            airport = data$airport_name
        )
    } else {
        grouping <- list(
            year_mon = data$year_mon,
            station = data$station
        )
    }

    # aggregate
    agg <- aggregate(
        cols,
        by = grouping,
        FUN = mean,
        na.rm = TRUE
    )

    # add also airport name if individual airports aggregated
    if(group == "individual_airports") {
        agg <- agg |>
            mutate(
                airport = paste0("Airport", airport_name)
            )
    }

    # return
    return(agg)
}

#----------------------------------------------
# apply aggregate function
# for time
avg_month <- agg_fun(data = all, group = "month")

#----------------------------------------------
# across airports
avg_airport <- agg_fun(data = all, group = "airports")

#----------------------------------------------
# individual airports

# Duesseldorf
avg_dus <- agg_fun(data = dus, group = "individual_airports", airport_name = "Duesseldorf")

# Frankfurt
avg_fra <- agg_fun(data = fra, group = "individual_airports", airport_name = "Frankfurt")

# Hamburg
avg_ham <- agg_fun(data = ham, group = "individual_airports", airport_name = "Hamburg")

# Hannover
avg_haj <- agg_fun(data = haj, group = "individual_airports", airport_name = "Hannover")

# Leipzig
avg_lej <- agg_fun(data = lej, group = "individual_airports", airport_name = "Leipzig")

# Muenchen
avg_muc <- agg_fun(data = muc, group = "individual_airports", airport_name = "Muenchen")

###############################################################
# Combine everything                                          #
###############################################################

avg_all <- rbind(avg_dus, avg_fra, avg_ham, avg_haj, avg_lej, avg_muc)

###############################################################
# Export                                                      #
###############################################################

write.fst(avg_dus, path = file.path(data_path, "Hauptflughaefen_Laerm/dus_1920.fst"))
write.fst(avg_fra, path = file.path(data_path, "Hauptflughaefen_Laerm/fra_1920.fst"))
write.fst(avg_ham, path = file.path(data_path, "Hauptflughaefen_Laerm/ham_1920.fst"))
write.fst(avg_haj, path = file.path(data_path, "Hauptflughaefen_Laerm/haj_1920.fst"))
write.fst(avg_lej, path = file.path(data_path, "Hauptflughaefen_Laerm/lej_1920.fst"))
write.fst(avg_muc, path = file.path(data_path, "Hauptflughaefen_Laerm/muc_1920.fst"))
write.fst(avg_all, path = file.path(data_path, "Hauptflughaefen_Laerm/all_1920.fst"))
write.fst(avg_month, path = file.path(data_path, "Hauptflughaefen_Laerm/avg_month.fst"))
write.fst(avg_airport, path = file.path(data_path, "Hauptflughaefen_Laerm/avg_airport.fst"))