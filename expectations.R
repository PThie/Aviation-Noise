#############################################
# Stocks                                    #
#############################################

# list all files
files <- list.files(
    file.path(
        data_path, "aktien"
    ),
    pattern = "*.csv",
    full.names = TRUE
)

# read function
read_data <- function(company) {
    # list files for company
    fls <- files[stringr::str_detect(files, company) == TRUE]

    # loop through files
    stor <- list()
    for(fl in fls) {
        dta <- data.table::fread(
            fl, sep = ";"
        )

        stor[[fl]] <- dta
    }

    # row bind data
    stock_data <- data.table::rbindlist(stor)

    # make sure that date is not duplicated
    stopifnot(
        length(which(duplicated(stock_data$Datum))) == 0
    )

    # cleaning
    if(company != "DAX"){
    # keep only relevant columns
        stock_data <- stock_data |>
            select(-c(Stuecke, Volumen))
    } else {
        stock_data <- stock_data |>
            select(-c(Volumen))
    }

    # rename
    colnames(stock_data) <- c("date", "open", "high", "low", "close")

    # turn into numbers
    stock_data <- stock_data |>
        mutate(
            open = as.numeric(
                gsub(",", ".", gsub("\\.", "", open))
            ),
            high = as.numeric(
                gsub(",", ".", gsub("\\.", "", high))
            ),
            low = as.numeric(
                gsub(",", ".", gsub("\\.", "", low))
            ),
            close = as.numeric(
                gsub(",", ".", gsub("\\.", "", close))
            ),
            # add date
            date = format(as.Date(date), "%Y-%m-%d"),
            # add month
            month = format(as.Date(date), "%Y-%m"),
            # add stock name
            stock = company
        ) |>
        select(
            stock, date, month, everything()
        )
    # return
    return(stock_data)
}

airbus <- read_data(company = "Airbus")
fraport <- read_data(company = "Fraport")
lufthansa <- read_data(company = "Lufthansa")
mtu <- read_data(company = "MTU_Aero_Engines")
tui <- read_data(company = "TUI")
dax <- read_data(company = "DAX")

# combine all stocks
stocks <- rbind(
    airbus, lufthansa, fraport, mtu, tui
)

#----------------------------------------------
# summarize by month

stocks_month <- stocks |>
    group_by(month) |>
    summarize(
        mean_open = mean(open, na.rm = TRUE),
        mean_high = mean(high, na.rm = TRUE),
        mean_low = mean(low, na.rm = TRUE),
        mean_close = mean(close, na.rm = TRUE)
    )

dax_month <- dax |>
    group_by(month) |>
    summarize(
        mean_open = mean(open, na.rm = TRUE),
        mean_high = mean(high, na.rm = TRUE),
        mean_low = mean(low, na.rm = TRUE),
        mean_close = mean(close, na.rm = TRUE)
    )

#----------------------------------------------
# summarize by month and stock

stocks_month_individual <- stocks |>
    group_by(month, stock) |>
    summarize(
        mean_open = mean(open, na.rm = TRUE),
        mean_high = mean(high, na.rm = TRUE),
        mean_low = mean(low, na.rm = TRUE),
        mean_close = mean(close, na.rm = TRUE)
    )

#----------------------------------------------
# merge with plotting together

# merge
stocks_dax <- merge(
    stocks_month,
    dax_month,
    by = "month"
)

# rename
colnames(stocks_dax) <- c(
    "month", "mean_open_stocks", "mean_high_stocks", "mean_low_stocks", 
    "mean_close_stocks", "mean_open_dax", "mean_high_dax", "mean_low_dax",
    "mean_close_dax"
)

# CONTINUE HERE
#----------------------------------------------
# normalized by 2018

# restrict to 2018
stocks_dax_base <- stocks_dax |>
    filter(month >= "2018-01")


stocks_dax_base <- stocks_dax_base |>
    mutate(
        base_open_stocks = mean_open_stocks[1],
        base_open_dax = mean_open_dax[1],
        open_stocks_relbase = (mean_open_stocks / base_open_stocks) * 100,
        open_dax_relbase = (mean_open_dax / base_open_dax) * 100
    )

#----------------------------------------------
# lockdown data

lockdowns <- data.frame(
    lockdown = c("first", "light", "second", "notbremse"), 
    months_start = as.yearmon(c("2020-03", "2020-11", "2020-12", "2021-04")),
    months_end = as.yearmon(c("2020-05", "2020-12", "2021-03", "2021-06"))
)

#----------------------------------------------
# plot data

# add plot date
stocks_dax_base$plot_date <- as.yearmon(stocks_dax_base$month)

# labels
month_labels <- c(
    "Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
    "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021", "Sep 2021",
    "Jan 2022", "Jun 2022"
)

# mytheme
mytheme <- theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.line = element_line(colour = "black"),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    axis.ticks.length = unit(0.25, "cm"),
    legend.key = element_blank()
)

# line thickness
linethick <- 1

# relative to base month January 2018
plot_relbase <- ggplot()+
    geom_rect(
        data = lockdowns,
        mapping = aes(xmin = months_start, xmax = months_end, ymin = -Inf, ymax = +Inf),
        fill = "grey80",
        alpha = 0.4
    )+
    geom_line(
        data = stocks_dax_base,
        mapping = aes(x = plot_date, y = open_stocks_relbase, col = "stocks", group = 1),
        linewidth = linethick
    )+
    geom_line(
        data = stocks_dax_base,
        mapping = aes(x = plot_date, y = open_dax_relbase, col = "dax", group = 1),
        linewidth = linethick
    )+
    scale_x_yearmon(
        breaks = seq(min(stocks_dax_base$plot_date), max(stocks_dax_base$plot_date), 0.34),
        labels = month_labels
    )+
    scale_y_continuous(
        breaks = seq(60, 130, 10)
    )+
    scale_color_manual(
        values = c(
            "stocks" = "darkorange3",
            "dax" = "royalblue2"
        ),
        name = "",
        labels = c(
            "stocks" = "Aviation stocks",
            "dax" = "DAX 40")
        )+
    geom_hline(
        yintercept = 100,
        linetype = "twodash",
        linewidth = linethick
    )+
    labs(x = "", y = "Opening value rel. to Jan. 2018 (in %)")+
    mytheme

ggsave(
    plot = plot_relbase,
    file.path(
        output_path,
        "graphs/stocks_dax_rel_base.png"
    ),
    units = "cm",
    width = 18,
    height = 15
)
