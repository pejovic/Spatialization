library(tsibble)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggforce)


times <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                    to   = ymd_h("2015-12-31 23"),
                    by   = dhours(1))  


public_holidays <- as.Date(c("2015-01-01", "2015-01-02", "2015-01-07", "2015-02-15", "2015-02-16", "2015-02-17",
                           "2015-04-10", "2015-04-11", "2015-04-12", "2015-04-13", "2015-05-01", "2015-05-02",
                           "2015-11-11", "2015-12-25"), format = "%Y-%m-%d")

day_hours <- rep(c(1:24), 365)

day_in_month <- lubridate::day(times)

month_in_year <- lubridate::month(times)

day_in_year <- rep(1:365, each = 24)

public_holidays <- day_in_year %in% julian.Date(public_holidays, origin = as.Date("2014-12-31"))
working_days <- !lubridate::wday(times) %in% c(1, 7)
working_time_8_16h <- day_hours %in% c(8:16)
working_time_16_24h <- day_hours %in% c(16:24)
day_light <- day_hours %in% c(7:19)
weekends <- lubridate::wday(times) %in% c(1, 7)
rush_hour <- day_hours %in% c(7:9, 15:17)


activity_df <- data.frame(times, day_in_year, day_in_month, day_hours, month_in_year, public_holidays, working_days, working_time_8_16h, day_light, weekends, rush_hour)

# Na primer neka aktivnost (A1) moze biti predstavljena formulom A1 =((working_time_8_16h + working_time_8_16h)/2) x !weekends x !public_holidays:

activity_df %<>% dplyr::mutate(A1 = ((working_time_8_16h + working_time_8_16h)/2)*!weekends*!public_holidays)

p <- ggplot(activity_df, aes(x = times, y = A1)) +
  geom_point(size = 0.1) +
  geom_line() + 
  theme_bw()


time_seq <- seq.POSIXt(from = ymd_h("2015-03-21 00"),
                       to   = ymd_h("2015-03-25 23"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)















