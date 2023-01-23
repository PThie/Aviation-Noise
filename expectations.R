#############################################
# Google trends                             #
#############################################


# load data ---------------------------------------------------------------


covid <- read.csv(file.path(dataFlug, "google_trends/google_trends_covid.csv"))
corona <- read.csv(file.path(dataFlug, "google_trends/google_trends_corona.csv"))
lockdown <- read.csv(file.path(dataFlug, "google_trends/google_trends_lockdown.csv"))
fallzahlen <- read.csv(file.path(dataFlug, "google_trends/google_trends_fallzahlen.csv"))


# prep data ---------------------------------------------------------------

# prep function
prep_gt <- function(gt_data, keyword){
  # rename
  colnames(gt_data) <- c("popularity")
  
  # delete first row
  gt_data <- gt_data %>% filter(row_number() >= 2)
  
  # replace < 1
  gt_data$popularity[gt_data$popularity == "<1"] <- "0"
  
  # make numeric
  gt_data$popularity <- as.numeric(gt_data$popularity)
  
  # rownames == weeks
  gt_data$week <- rownames(gt_data)
  
  # numbering row
  rownames(gt_data) <- seq(1, nrow(gt_data), 1)
  
  # rename data specific
  names(gt_data)[names(gt_data) == "popularity"] <- paste0("popularity_", keyword)
  
  # order
  gt_data <- gt_data[, c(2, 1)]
  
  # return
  return(gt_data)
}

# apply prep function
covid_prep <- prep_gt(covid, keyword = "covid")
corona_prep <- prep_gt(corona, keyword = "corona")
lockdown_prep <- prep_gt(lockdown, keyword = "lockdown")
fallzahlen_prep <- prep_gt(fallzahlen, keyword = "fallzahlen")

# merge into one data frame
google_data_aux1 <- merge(covid_prep, corona_prep, by = "week")
google_data_aux2 <- merge(lockdown_prep, fallzahlen_prep, by = "week")
google_data <- merge(google_data_aux1, google_data_aux2, by = "week")

# add total popularity scores and mean
num_keywords <- 4
google_data <- google_data %>% rowwise() %>% mutate(total_pop = sum(popularity_covid, popularity_corona, 
                                                                    popularity_lockdown, popularity_fallzahlen, na.rm = TRUE),
                                                    mean_pop = total_pop/num_keywords)


# summarize by month ------------------------------------------------------

# add month
google_data$month <- substr(google_data$week, start = 1, stop = 7)

# summarize by month
google_data_month <- google_data %>% group_by(month) %>% summarise(mean_covid = mean(popularity_covid, na.rm = TRUE),
                                                                   mean_corona = mean(popularity_corona, na.rm = TRUE),
                                                                   mean_lockdown = mean(popularity_lockdown, na.rm = TRUE),
                                                                   mean_fallzahlen = mean(popularity_fallzahlen, na.rm = TRUE),
                                                                   mean_total = mean(total_pop, na.rm = TRUE)) 


# plot monthly data -------------------------------------------------------

# mytheme
mytheme <- theme(panel.background = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                 axis.text.y = element_text(size = 15),
                 axis.line = element_line(colour = "black"),
                 axis.title.y = element_text(size = 18),
                 legend.text = element_text(size = 15),
                 axis.ticks.length = unit(0.25, "cm"))


ggplot(google_data_month, aes(x = month, group = 1))+
  geom_line(aes(y = mean_total))+
  mytheme
  

#############################################
# Politbarometer                            #
#############################################


# load data ---------------------------------------------------------------

polit <- haven::read_dta(file.path(dataFlug, "politbarometer/ZA2391_v13-0-0.dta/ZA2391_v13-0-0.dta"))


#############################################
# Stocks                                    #
#############################################


# load data ---------------------------------------------------------------

airbus <- read.csv(file.path(dataFlug, "aktien/Airbus.csv"), sep = ";")
fraport <- read.csv(file.path(dataFlug, "aktien/Fraport.csv"), sep = ";")
lufthansa <- read.csv(file.path(dataFlug, "aktien/Lufthansa.csv"), sep = ";")
mtu <- read.csv(file.path(dataFlug, "aktien/MTU_Aero_Engines.csv"), sep = ";")
tui <- read.csv(file.path(dataFlug, "aktien/TUI.csv"), sep = ";")
dax <- read.csv(file.path(dataFlug, "aktien/DAX.csv"), sep = ";")
  
# prep data ---------------------------------------------------------------

# prep function
prep_stocks <- function(stock_data, stock_name){
  if(stock_name != "dax"){
    # keep only relevant columns
    stock_data <- stock_data %>% select(-c(Stuecke, Volumen))
  } else {
    stock_data <- stock_data %>% select(-c(Volumen))
  }

  
  # turn into numbers
  stock_data$Erster <- as.numeric(gsub(",", ".", gsub("\\.", "", stock_data$Erster)))
  stock_data$Hoch <- as.numeric(gsub(",", ".", gsub("\\.", "", stock_data$Hoch)))
  stock_data$Tief <- as.numeric(gsub(",", ".", gsub("\\.", "", stock_data$Tief)))
  stock_data$Schlusskurs <- as.numeric(gsub(",", ".", gsub("\\.", "", stock_data$Schlusskurs)))
  
  # rename
  colnames(stock_data) <- c("date", "open", "high", "low", "close")
  
  # set as date
  stock_data$date <- format(as.Date(stock_data$date), "%Y-%m-%d")
  
  # month
  stock_data$month <- format(as.Date(stock_data$date), "%Y-%m")
  
  # add stock name
  stock_data$stock <- stock_name
  
  # reorder
  stock_data <- stock_data[, c(6, 1, 7, 2:5)]
  
  # return
  return(stock_data)
}

# apply prep function
airbus_prep <- prep_stocks(airbus, stock_name = "airbus")
lufthansa_prep <- prep_stocks(lufthansa, stock_name = "lufthansa")
fraport_prep <- prep_stocks(fraport, stock_name = "fraport")
mtu_prep <- prep_stocks(mtu, stock_name = "mtu")
tui_prep <- prep_stocks(tui, stock_name = "tui")
dax_prep <- prep_stocks(dax, stock_name = "dax")

# combine all stocks
stocks <- rbind(airbus_prep, lufthansa_prep, fraport_prep, mtu_prep, tui_prep)


# summarize by month ------------------------------------------------------

stocks_month <- stocks %>% group_by(month) %>% summarize(mean_open = mean(open, na.rm = TRUE),
                                                         mean_high = mean(high, na.rm = TRUE),
                                                         mean_low = mean(low, na.rm = TRUE),
                                                         mean_close = mean(close, na.rm = TRUE))

dax_month <- dax_prep %>% group_by(month) %>% summarize(mean_open = mean(open, na.rm = TRUE),
                                                   mean_high = mean(high, na.rm = TRUE),
                                                   mean_low = mean(low, na.rm = TRUE),
                                                   mean_close = mean(close, na.rm = TRUE))

# summarize by month and stock --------------------------------------------

stocks_month_individual <- stocks %>% group_by(month, stock) %>% summarize(mean_open = mean(open, na.rm = TRUE),
                                                                           mean_high = mean(high, na.rm = TRUE),
                                                                           mean_low = mean(low, na.rm = TRUE),
                                                                           mean_close = mean(close, na.rm = TRUE))


# merge with plotting together --------------------------------------------

# merge
stocks_dax <- merge(stocks_month, dax_month, by = "month")

# rename
colnames(stocks_dax) <- c("month", "mean_open_stocks", "mean_high_stocks", "mean_low_stocks", "mean_close_stocks", 
                          "mean_open_dax", "mean_high_dax", "mean_low_dax", "mean_close_dax")


# scale -------------------------------------------------------------------
# min-max-normalization

# function
min_max_norm <- function(value){
  (value - min(value)) / (max(value) - min(value))
}

# apply min-max normalization
stocks_dax <- stocks_dax %>% mutate(open_stocks_scale = min_max_norm(mean_open_stocks),
                                    high_stocks_scale = min_max_norm(mean_high_stocks),
                                    low_stocks_scale = min_max_norm(mean_low_stocks),
                                    open_dax_scale = min_max_norm(mean_open_dax),
                                    high_dax_scale = min_max_norm(mean_high_dax),
                                    low_dax_scale = min_max_norm(mean_low_dax))


# normalized by 2018 ------------------------------------------------------

# restrict to 2018
stocks_dax_base <- stocks_dax %>% filter(month >= "2018-01")


stocks_dax_base <- stocks_dax_base %>% mutate(base_open_stocks = mean_open_stocks[1],
                                              base_open_dax = mean_open_dax[1],
                                              open_stocks_relbase = (mean_open_stocks / base_open_stocks) * 100,
                                              open_dax_relbase = (mean_open_dax / base_open_dax) * 100)

# change to previous periods ----------------------------------------------

# change to pre-month and pre-year_month
stocks_dax <- stocks_dax %>% mutate(open_change_prevmonth_stocks = ((mean_open_stocks - lag(mean_open_stocks, n = 1)) / lag(mean_open_stocks, n = 1)) * 100,
                                    high_change_prevmonth_stocks = ((mean_high_stocks - lag(mean_high_stocks, n = 1)) / lag(mean_high_stocks, n = 1)) * 100,
                                    low_change_prevmonth_stocks = ((mean_low_stocks - lag(mean_low_stocks, n = 1)) / lag(mean_low_stocks, n = 1)) * 100,
                                    open_change_prevmonth_dax = ((mean_open_dax - lag(mean_open_dax, n = 1)) / lag(mean_open_dax, n = 1)) * 100,
                                    high_change_prevmonth_dax = ((mean_high_dax - lag(mean_high_dax, n = 1)) / lag(mean_high_dax, n = 1)) * 100,
                                    low_change_prevmonth_dax = ((mean_low_dax - lag(mean_low_dax, n = 1)) / lag(mean_low_dax, n = 1)) * 100,
                                    # year-month change
                                    open_change_prevyearmonth_stocks = ((mean_open_stocks - lag(mean_open_stocks, n = 12)) / lag(mean_open_stocks, n = 12)) * 100,
                                    high_change_prevyearmonth_stocks = ((mean_high_stocks - lag(mean_high_stocks, n = 12)) / lag(mean_high_stocks, n = 12)) * 100,
                                    low_change_prevyearmonth_stocks = ((mean_low_stocks - lag(mean_low_stocks, n = 12)) / lag(mean_low_stocks, n = 12)) * 100,
                                    open_change_prevyearmonth_dax = ((mean_open_dax - lag(mean_open_dax, n = 12)) / lag(mean_open_dax, n = 12)) * 100,
                                    high_change_prevyearmonth_dax = ((mean_high_dax - lag(mean_high_dax, n = 12)) / lag(mean_high_dax, n = 12)) * 100,
                                    low_change_prevyearmonth_dax = ((mean_low_dax - lag(mean_low_dax, n = 12)) / lag(mean_low_dax, n = 12)) * 100)

# drop 2017
stocks_dax <- stocks_dax %>% filter(month >= "2018-01")


# lockdown data -----------------------------------------------------------

lockdowns <- data.frame(lockdown = c("first", "light", "second", "notbremse"), 
                        months_start = as.yearmon(c("2020-03", "2020-11", "2020-12", "2021-04")),
                        months_end = as.yearmon(c("2020-05", "2020-12", "2021-03", "2021-06")))

# plot monthly data -------------------------------------------------------

####### prepare
# add plot date
stocks_dax$plot_date <- as.yearmon(stocks_dax$month)
stocks_dax_base$plot_date <- as.yearmon(stocks_dax_base$month)

# labels
month_labels <- c("Jan 2018", "May 2018", "Sep 2018", "Jan 2019", "May 2019", "Sep 2019", 
                  "Jan 2020", "May 2020", "Sep 2020", "Jan 2021", "May 2021", "Sep 2021")

# mytheme
mytheme <- theme(panel.background = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                 axis.text.y = element_text(size = 15),
                 axis.line = element_line(colour = "black"),
                 axis.title.y = element_text(size = 18),
                 legend.text = element_text(size = 15),
                 axis.ticks.length = unit(0.25, "cm"),
                 legend.key = element_blank())

# line thickness
linethick <- 1

###### plots 
# development (i.e no change) (scaled)
plot_stocks_dev <- ggplot(stocks_dax, aes(group = 1))+
  geom_rect(data = lockdowns, mapping = aes(xmin = months_start, xmax = months_end, ymin = -Inf, ymax = +Inf), fill = "grey80", alpha = 0.4)+
  geom_line(aes(x = plot_date, y = open_stocks_scale, col = "stocks"), size = linethick)+
  geom_line(aes(x = plot_date, y = open_dax_scale, col = "dax"), size = linethick)+
  scale_x_yearmon(breaks = seq(min(stocks_dax$plot_date), max(stocks_dax$plot_date), 0.34),
                  labels = month_labels)+
  scale_color_manual(values = c("stocks" = "darkorange3",
                                "dax" = "royalblue2"),
                     name = "",
                     labels = c("stocks" = "Aviation stocks",
                                "dax" = "DAX 40"))+
  labs(x = "", y = "Opening value (scaled)")+
  mytheme

plot_stocks_dev
ggsave(plot = plot_stocks_dev, file.path(outputPath, "graphs/stocks_development_scaled.png"), units = "cm", width = 18, height = 15)


# plot prev month data
plot_stocks_change_prevmonth <- ggplot(stocks_dax, aes(group = 1))+
  geom_rect(data = lockdowns, mapping = aes(xmin = months_start, xmax = months_end, ymin = -Inf, ymax = +Inf), fill = "grey80", alpha = 0.4)+
  geom_line(aes(x = plot_date, y = open_change_prevmonth_stocks, col = "stocks"), size = linethick)+
  geom_line(aes(x = plot_date, y = open_change_prevmonth_dax, col = "dax"), size = linethick)+
  scale_x_yearmon(breaks = seq(min(stocks_dax$plot_date), max(stocks_dax$plot_date), 0.34),
                  labels = month_labels)+
  scale_color_manual(values = c("stocks" = "darkorange3",
                                "dax" = "royalblue2"),
                     name = "",
                     labels = c("stocks" = "Aviation stocks",
                                "dax" = "DAX 40"))+
  labs(x = "", y = "Change opening value (in %)")+
  mytheme

plot_stocks_change_prevmonth
ggsave(plot = plot_stocks_change_prevmonth, file.path(outputPath, "graphs/stocks_change_prevmonth.png"), units = "cm", width = 18, height = 15)


# plot prev year-month data
plot_stocks_change_prevyearmonth <- ggplot(stocks_dax, aes(group = 1))+
  geom_rect(data = lockdowns, mapping = aes(xmin = months_start, xmax = months_end, ymin = -Inf, ymax = +Inf), fill = "grey80", alpha = 0.4)+
  geom_line(aes(x = plot_date, y = open_change_prevyearmonth_stocks, col = "stocks"), size = linethick)+
  geom_line(aes(x = plot_date, y = open_change_prevyearmonth_dax, col = "dax"), size = linethick)+
  scale_x_yearmon(breaks = seq(min(stocks_dax$plot_date), max(stocks_dax$plot_date), 0.34),
                  labels = month_labels)+
  scale_color_manual(values = c("stocks" = "darkorange3",
                                "dax" = "royalblue2"),
                     name = "",
                     labels = c("stocks" = "Aviation stocks",
                                "dax" = "DAX 40"))+
  labs(x = "", y = "Change opening value (in %)")+
  mytheme

plot_stocks_change_prevyearmonth
ggsave(plot = plot_stocks_change_prevyearmonth, file.path(outputPath, "graphs/stocks_change_prevyearmonth.png"), units = "cm", width = 18, height = 15)


# relative to base month January 2018
plot_relbase <- ggplot(stocks_dax_base, aes(group =  1))+
  geom_rect(data = lockdowns, mapping = aes(xmin = months_start, xmax = months_end, ymin = -Inf, ymax = +Inf), fill = "grey80", alpha = 0.4)+
  geom_line(aes(x = plot_date, y = open_stocks_relbase, col = "stocks"), size = linethick)+
  geom_line(aes(x = plot_date, y = open_dax_relbase, col = "dax"), size = linethick)+
  scale_x_yearmon(breaks = seq(min(stocks_dax_base$plot_date), max(stocks_dax_base$plot_date), 0.34),
                  labels = month_labels)+
  scale_color_manual(values = c("stocks" = "darkorange3",
                                "dax" = "royalblue2"),
                     name = "",
                     labels = c("stocks" = "Aviation stocks",
                                "dax" = "DAX 40"))+
  labs(x = "", y = "Opening value rel. to Jan. 2018 (in %)")+
  mytheme
  
plot_relbase
ggsave(plot = plot_relbase, file.path(outputPath, "graphs/stocks_dax_rel_base.png"), units = "cm", width = 18, height = 15)
