###############################################################
# Description                                                 #
###############################################################

# This file generates plots of the flight activity for passenger and cargo.
# It also caluclates the respective shares covered comparing the included
# airports and the total number of transported passengers and cargo.

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
    last = nchar(file_list) - 5
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
    "Jan 2022", "May 2022", "Sep 2022", "Jan 2023"
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
        linewidth = 1
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
    freight_carry |>
        filter(year_mon <= "2020-03") |>
        summarise(
            avg_flight_act = mean(freight_t, na.rm = TRUE)
        )
)

after_lock <- as.numeric(
    freight_carry |>
        filter(year_mon > "2020-03") |>
        summarise(
            avg_flight_act = mean(freight_t, na.rm = TRUE)
        )
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