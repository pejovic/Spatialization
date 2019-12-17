library(tsibble)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggforce)

times <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                    to   = ymd_h("2015-12-31 23"),
                    by   = dhours(1))  

day_hours <- rep(c(1:24), 365)

day_in_month <- lubridate::day(times)

month_in_year <- lubridate::month(times)

day_in_year <- rep(1:365, each = 24)


# Function: Working days
working_days <- !lubridate::wday(times) %in% c(1, 7)

# Function: Public holidays
public_holidays <- as.Date(c("2015-01-01", "2015-01-02", "2015-01-07", "2015-02-15", "2015-02-16", "2015-02-17",
                             "2015-04-10", "2015-04-11", "2015-04-12", "2015-04-13", "2015-05-01", "2015-05-02",
                             "2015-11-11", "2015-12-25"), format = "%Y-%m-%d")

public_holidays <- day_in_year %in% julian.Date(public_holidays, origin = as.Date("2014-12-31"))

# Function: working_time_8_16h
working_time_8_16h <- day_hours %in% c(8:16)

# Function: working_time_16_24h
working_time_16_24h <- day_hours %in% c(16:24)

# Function: Day light
day_light <- day_hours %in% c(7:19)

# Function: weekends
weekends <- lubridate::wday(times) %in% c(1, 7)

# Function: rush_hour
rush_hour <- day_hours %in% c(7:9, 15:17)



activity_df <- data.frame(times, day_in_year, day_in_month, day_hours, month_in_year, public_holidays, working_days, working_time_8_16h, day_light, weekends, rush_hour)




















# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Function hourly.emissions
# Parameters:

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

hourly.emissions <- function(pollutant = pollutant, formula = formula, activity_df = activity_df){
  
  
  
  
}

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read all data files

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
data.spat <- list.files('C:/Users/pbursac/Desktop/data/')

data.spat.list <- list()                                                   
for(i in 1:length(data.spat)){                                             
 data.spat.list[[i]] <- st_read(paste("C:/Users/pbursac/Desktop/data/",data.spat[i], sep = ""))
}


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

pol.list <- list(NOx = NA, SO2 = NA, PM10 = NA, PM2.5 = NA, NMVOC = NA, NH3 = NA)
pol.1 <- data.spat.list[[1]]





# Na primer neka aktivnost (A1) moze biti predstavljena formulom A1 =((working_time_8_16h + working_time_8_16h)/2) x !weekends x !public_holidays:

activity_df %<>% 
  dplyr::mutate(A1 = ((working_time_8_16h + working_time_16_24h))/2*!weekends*!public_holidays) %>% 
  #dplyr::filter(working_time_8_16h == TRUE) %>% 
  dplyr::mutate(A2 = 0.5*sin(((2*pi)/24)*(day_hours)) + 0.5) %>%
  dplyr::mutate(A3 = A2*A1) %>% 
  dplyr::mutate(A4 = 0.5*sin(0.5*(day_hours-16)) + 0.5) %>%
  dplyr::mutate(A5 = A4 * (0.2+working_time_16_24h))


p <- ggplot(activity_df, aes(x = times, y = A5, colour = "red")) +
  geom_point(size = 0.1) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-02 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



p1 <- ggplot() +
  geom_line(data = activity_df, aes(x = times, y = A2, colour = "red")) +
  geom_line(data = activity_df, aes(x = times, y = A1, colour = "blue")) +
  geom_line(data = activity_df, aes(x = times, y = A3, colour = "green")) +
  theme_bw()
p1 + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

