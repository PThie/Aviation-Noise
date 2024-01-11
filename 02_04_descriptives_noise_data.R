###############################################################
# Description                                                 #
###############################################################

# This file generates the plots for displaying the noise levels based on
# measuring stations around the airports.

###############################################################
# load data                                                   #
###############################################################

#----------------------------------------------
# load noise data

noise_data <- read.fst(
    file.path(
        data_path,
        "Hauptflughaefen_Laerm/avg_month.fst"
    )
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

# define lockdown time
lock <- as.numeric(
    noise_data$plot_date[noise_data$year_mon == "2020-03"]
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
        linewidth = 1
    )+
    geom_segment(
        mapping = aes(
            x = as.numeric(min(plot_date)),
            xend = lock,
            y = before_lock,
            yend = before_lock
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
    height = 5,
    dpi = owndpi
)
