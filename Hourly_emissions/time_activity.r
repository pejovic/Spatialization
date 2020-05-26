library(tsibble)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggforce)

# ibrary(devtools)
# install_github("duplisea/dublogistic")
library(dublogistic)

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
working_time_16_24h <- day_hours %in% c(17:24)

# Function: Day light
day_light <- day_hours %in% c(7:19)

# Function: weekends
weekends <- lubridate::wday(times) %in% c(1, 7)

# Function: rush_hour
rush_hour <- day_hours %in% c(7:9, 15:17)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
activity_df <- data.frame(times, day_in_year, day_in_month, day_hours, month_in_year, public_holidays, working_days, working_time_8_16h, day_light, weekends, rush_hour)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working days
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df %<>% 
  dplyr::mutate(WD = sin(((2*pi)/4)*(working_days)))


p <- ggplot(activity_df, aes(x = times, y = WD, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working days, working weekends
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

wdww <- rep(TRUE, length(day_hours))
activity_df$WDWW <- wdww

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working hours from 8 - 16h
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df %<>% 
  #dplyr::filter(working_time_8_16h == TRUE) %>% 
  dplyr::mutate(WT0816 = dplyr::case_when(working_time_8_16h == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-7)) + 0.5,
                                          working_time_8_16h == FALSE ~ 0))


dplyr::mutate(WT0816 = dplyr::case_when(working_time_8_16h == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-7)) + 0.5,
                                        working_time_8_16h == FALSE ~ 0))



# NOVO
activity_df %<>% 
  dplyr::mutate(WT0816 = dplyr::case_when(working_time_8_16h == TRUE ~ 1,
                                          working_time_8_16h == FALSE ~ 0))



p <- ggplot(activity_df, aes(x = times, y = WT0816, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-31 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working hours from 16 - 24h
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df %<>% 
  #dplyr::filter(working_time_16_24h == TRUE) %>% 
  dplyr::mutate(WT1624 = dplyr::case_when(working_time_16_24h == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-15)) + 0.5,
                                          working_time_16_24h == FALSE ~ 0))


# Novo
activity_df %<>% 
  dplyr::mutate(WT1624 = dplyr::case_when(working_time_16_24h == TRUE ~ 1,
                                          working_time_16_24h == FALSE ~ 0))

p <- ggplot(activity_df, aes(x = times, y = WT1624, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-31 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working hours from 00 - 24h
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

working_time_00_24h <- day_hours %in% c(00:24)
activity_df$working_time_00_24h <- working_time_00_24h
activity_df %<>% 
  # dplyr::filter(working_time_00_24h == TRUE) %>% 
  dplyr::mutate(WT0024 = dplyr::case_when(working_time_00_24h == TRUE ~ 1, 
                                          working_time_00_24h == FALSE ~ 0))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working time for heating season from 06 - 23h
# 15.10 - 15.04
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

times1 <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                     to   = ymd_h("2015-04-15 23"),
                     by   = dhours(1)) 
times2 <- seq.POSIXt(from = ymd_h("2015-10-15 00"),
                     to   = ymd_h("2015-12-31 23"),
                     by   = dhours(1)) 

times3 <- c(times1,times2)

activity_df$indhs[activity_df$times %in% times3] <- TRUE 
activity_df$indhs[!(activity_df$times %in% times3)] <- FALSE 

working_time_06_22h <- day_hours %in% c(06:23)
activity_df$working_time_06_22h <- working_time_06_22h
activity_df %<>% 
  #dplyr::filter(indhs == TRUE) %>% 
  #dplyr::filter(working_time_06_22h == TRUE) %>% 
  dplyr::mutate(WT0622 = dplyr::case_when(indhs == TRUE & working_time_06_22h == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-5)) + 0.5,
                                          indhs == TRUE & working_time_06_22h == FALSE ~ 0,
                                          indhs == FALSE & working_time_06_22h == FALSE ~ 0,
                                          indhs == FALSE & working_time_06_22h == TRUE ~ 0))


# NOVO
activity_df %<>% 
  dplyr::mutate(WT0622 = dplyr::case_when(indhs == TRUE & working_time_06_22h == TRUE ~ 1,
                                          indhs == TRUE & working_time_06_22h == FALSE ~ 0.2,
                                          indhs == FALSE & working_time_06_22h == FALSE ~ 0,
                                          indhs == FALSE & working_time_06_22h == TRUE ~ 0))


p <- ggplot(activity_df, aes(x = times, y = WT0622, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-31 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Daylength from data
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dayl <- readxl::read_xlsx(path = "Hourly_emissions/Data/Sunrise_Sunset_Daylength_Serbia.xlsx", sheet = "Sum") 

timesdl <- seq(from = ymd('2015-01-01'),
               to   = ymd('2015-12-31'),
               by   = 'day')


dayl$time <- timesdl
dayl <- dayl[rep(seq.int(1,nrow(dayl)), each = 24),]


ggplot(dayl, aes(x = time, y = Daylength, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

activity_df$dayl <- dayl$Daylength
day_light <- day_hours %in% c(7:20) #19
activity_df$day_light <- day_light 
activity_df %<>% 
  dplyr::mutate(DL = dplyr::case_when(day_light == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-7)) + dayl,
                                      day_light == FALSE ~ 0)) #dayl



p <- ggplot(activity_df, aes(x = times, y = DL, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ dayl, colour = "blue")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)






# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Weekends
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df %<>% 
  dplyr::mutate(WE = sin(((2*pi)/4)*(weekends)))


p <- ggplot(activity_df, aes(x = times, y = WE, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Working weekends
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df %<>% 
  dplyr::mutate(WW = dplyr::case_when(weekends == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-7)) + 0.5, 
                                      weekends == FALSE ~ 0))



# NOVO 

activity_df %<>% 
  dplyr::mutate(WW = dplyr::case_when(weekends == TRUE ~ 1, 
                                      weekends == FALSE ~ 0))

p <- ggplot(activity_df, aes(x = times, y = WW, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-31 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Roush hour 07-09h
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rush_hour0709 <- day_hours %in% c(8:10)
activity_df$rush_hour0709 <- rush_hour0709

activity_df %<>% 
  dplyr::mutate(RH0709 = dplyr::case_when(rush_hour0709 == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-3)) + 0.5,
                                          rush_hour0709 == FALSE ~ 0))


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Roush hour 15-17h
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rush_hour1517 <- day_hours %in% c(16:18)
activity_df$rush_hour1517 <- rush_hour1517

activity_df %<>% 
  dplyr::mutate(RH1517 = dplyr::case_when(rush_hour1517 == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-11)) + 0.5,
                                          rush_hour1517 == FALSE ~ 0))



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Public holidays
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


activity_df %<>% 
  dplyr::mutate(PH = sin(((2*pi)/4)*(public_holidays)))


p <- ggplot(activity_df, aes(x = times, y = PH, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Seasons
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


t1 <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                 to   = ymd_h("2015-03-20 23"),
                 by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-03-20 23"),
                 to   = ymd_h("2015-06-21 18"),
                 by   = dhours(1)) 

t3 <- seq.POSIXt(from = ymd_h("2015-06-21 18"),
                 to   = ymd_h("2015-09-23 16"),
                 by   = dhours(1)) 
t4 <- seq.POSIXt(from = ymd_h("2015-09-23 16"),
                 to   = ymd_h("2015-12-21 11"),
                 by   = dhours(1)) 

t5 <- seq.POSIXt(from = ymd_h("2015-12-21 11"),
                 to   = ymd_h("2015-12-31 23"),
                 by   = dhours(1)) 

dublogistic.f(L=as.numeric(t2), inflection1=as.numeric(quantile(t2, probs = 0.2)), inflection2=as.numeric(quantile(t2, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=T)


timesS <- c(t1, t2, t3, t4, t5)

activity_df$indS[activity_df$times %in% t1] <- 1
 
activity_df$indS[activity_df$times %in% t2] <- 2 
activity_df$indS[activity_df$times %in% t3] <- 3 
activity_df$indS[activity_df$times %in% t4] <- 4 
activity_df$indS[activity_df$times %in% t5] <- 1 

activity_df$indS <- as.numeric(activity_df$indS)

activity_df %<>% 
  dplyr::mutate(SA = 0.5*sin(((2*pi)/4)*(indS)) + 0.5)


p <- ggplot(activity_df, aes(x = times, y = SA, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Heating Season
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


times1 <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                     to   = ymd_h("2015-04-15 23"),
                     by   = dhours(1)) 
times2 <- seq.POSIXt(from = ymd_h("2015-10-15 00"),
                     to   = ymd_h("2015-12-31 23"),
                     by   = dhours(1)) 

times3 <- c(times1,times2)

activity_df$indhs[activity_df$times %in% times3] <- TRUE 
activity_df$indhs[!(activity_df$times %in% times3)] <- FALSE 

activity_df %<>% 
  dplyr::mutate(HS = sin(((2*pi)/4)*(indhs)))


p <- ggplot(activity_df, aes(x = times, y = HS, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)

dublogistic.f(L=as.numeric(times1), inflection1=as.numeric(quantile(times1, probs = 0.2)), inflection2=as.numeric(quantile(times1, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=T)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Agriculture Season
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# timesAg <- c(t2, t3, t4)
# 
# activity_df$indAg[activity_df$times %in% timesAg] <- TRUE 
# activity_df$indAg[!(activity_df$times %in% timesAg)] <- FALSE 
# 
# activity_df %<>% 
#   dplyr::mutate(SAAG = sin(((2*pi)/4)*(indAg)))
# 
# 
# p <- ggplot(activity_df, aes(x = times, y = SAAG, colour = "red")) +
#   geom_point(size = 0.5) +
#   geom_line() + 
#   theme_bw()
# 
# time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
#                        to   = ymd_h("2015-01-03 24"),
#                        by   = dhours(1)) 
# 
# p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# :::::::::::::::::::::::::::::::::::::::::::: Fertilizing 


t1 <- seq.POSIXt(from = ymd_h("2015-02-01 00"),
                 to   = ymd_h("2015-03-31 24"),
                 by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-05-01 00"),
                 to   = ymd_h("2015-07-31 24"),
                 by   = dhours(1)) 

dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.2)), inflection2=as.numeric(quantile(t1, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=T)$selectivity

activity_df$indSAAG.f[activity_df$times %in% t1] <- dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.2)), inflection2=as.numeric(quantile(t1, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=F)$selectivity
activity_df$indSAAG.f[activity_df$times %in% t2] <- dublogistic.f(L=as.numeric(t2), inflection1=as.numeric(quantile(t2, probs = 0.2)), inflection2=as.numeric(quantile(t2, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=F)$selectivity 
activity_df$SAAG.f

activity_df %<>%
  dplyr::mutate(indSAAG.f = tidyr::replace_na(indSAAG.f, 0)) %>%
  dplyr::rename(SAAG.f = indSAAG.f)


p <- ggplot(activity_df, aes(x = times, y = SAAG.f, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-04-15 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



# :::::::::::::::::::::::::::::::::::::::::::: Farm level 

t11 <- seq.POSIXt(from = ymd_h("2015-05-01 00"),
                 to   = ymd_h("2015-06-30 24"),
                 by   = dhours(1)) 
t22 <- seq.POSIXt(from = ymd_h("2015-09-01 00"),
                 to   = ymd_h("2015-10-31 24"),
                 by   = dhours(1)) 

dublogistic.f(L=as.numeric(t11), inflection1=as.numeric(quantile(t11, probs = 0.2)), inflection2=as.numeric(quantile(t11, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=T)$selectivity

activity_df$indSAAG.fl[activity_df$times %in% t11] <- dublogistic.f(L=as.numeric(t11), inflection1=as.numeric(quantile(t11, probs = 0.2)), inflection2=as.numeric(quantile(t11, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=F)$selectivity
activity_df$indSAAG.fl[activity_df$times %in% t22] <- dublogistic.f(L=as.numeric(t22), inflection1=as.numeric(quantile(t22, probs = 0.2)), inflection2=as.numeric(quantile(t22, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=1, minsel.upper=0, plot=F)$selectivity 


activity_df %<>%
  dplyr::mutate(indSAAG.fl = tidyr::replace_na(indSAAG.fl, 0)) %>%
  dplyr::rename(SAAG.fl = indSAAG.fl)


p <- ggplot(activity_df, aes(x = times, y = SAAG.fl, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-04-15 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)




ggplot() +
  geom_line(data = activity_df, aes(x = times, y = SAAG.f, colour = "red"))+
  geom_line(data = activity_df, aes(x = times, y = SAAG.fl, colour = "blue"))+
  geom_point(data = activity_df, aes(x = times, y = SAAG.f, colour = "red")) +
  geom_point(data = activity_df, aes(x = times, y = SAAG.fl, colour = "blue")) +
  theme_bw()+
  scale_colour_discrete(name = "Colour", labels = c("SAAG.feritilizers", "SAAG.farmlevel"))




# :::::::::::::::::::::::::::::::::::::::::::: Heating demand in agriculture

m <- loess(TEMP~as.numeric(times), activity_df, span = 0.5)
predict(m)

activity_df$temp_smooth <- predict(m)

p <- ggplot(activity_df, aes(x = times, y = temp_smooth)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ TEMP, colour = "orange")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



t1 <- seq.POSIXt(from = ymd_h("2015-01-30 00"),
                 to   = ymd_h("2015-08-30 24"),
                 by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-07-30 00"),
                 to   = ymd_h("2015-10-31 24"),
                 by   = dhours(1)) 

activity_df <- activity_df[, 1:13]
activity_df$temp_smooth <- predict(m)
activity_df$indSAAG.fer[activity_df$times %in% t1] <- dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.1)), inflection2=as.numeric(quantile(t1, probs = 0.7)), slope1=0.000006, slope2=0.000002, max.sel=1, minsel.upper=0, plot=F)$selectivity
#activity_df$indSAAG.fer[activity_df$times %in% t1] <- dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.08)), inflection2=as.numeric(quantile(t1, probs = 0.9)), slope1=0.000003, slope2=0.0000003, max.sel=1, minsel.upper=0, plot=F)$selectivity
activity_df$indSAAG.fer[activity_df$times %in% t2] <- dublogistic.f(L=as.numeric(t2), inflection1=as.numeric(quantile(t2, probs = 0.2)), inflection2=as.numeric(quantile(t2, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=0.5, minsel.upper=0, plot=F)$selectivity 

activity_df %<>%
  dplyr::mutate(indSAAG.fer = tidyr::replace_na(indSAAG.fer, 0)) %>%
  dplyr::rename(SAAG.F = indSAAG.fer)




summary(activity_df)



ggplot() +
  geom_line(data = activity_df, aes(x = times, y = temp_smooth)) + 
  geom_line(data = activity_df, aes(x = times, y = SAAG.F)) + 
  theme_bw() 


sigmoid = function(x) {
  1 / (1 + exp(-x))
}

he <- activity_df %>%
  dplyr::mutate(he_1A4ai = temp_smooth^-1 * SAAG.F) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4ai))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4ai = he_sig) %>%
  dplyr::mutate(he_1A4ai_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4ai, he_1A4ai_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he, aes(x = times, y = he_1A4ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  #geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1A4ai = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))




















# :::::::::::::::::::::::::::::::::::::::::::: SAAG.fertilizing NOVI GRAFIK 

m <- loess(TEMP~as.numeric(times), activity_df, span = 0.5)
predict(m)

activity_df$temp_smooth <- predict(m)

p <- ggplot(activity_df, aes(x = times, y = temp_smooth)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ TEMP, colour = "orange")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



t1 <- seq.POSIXt(from = ymd_h("2015-03-01 00"),
                 to   = ymd_h("2015-05-01 24"),
                 by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-09-01 00"),
                 to   = ymd_h("2015-11-01 24"),
                 by   = dhours(1)) 

activity_df <- activity_df[, 1:09]
# activity_df$temp_smooth <- predict(m)



activity_df$indSAAG.fert[activity_df$times %in% t1] <- dublogistic.f(L=as.numeric(t1), inflection1=as.numeric(quantile(t1, probs = 0.1)), inflection2=as.numeric(quantile(t1, probs = 0.7)), slope1=0.000006, slope2=0.000002, max.sel=1, minsel.upper=0, plot=F)$selectivity
activity_df$indSAAG.fert[activity_df$times %in% t2] <- dublogistic.f(L=as.numeric(t2), inflection1=as.numeric(quantile(t2, probs = 0.2)), inflection2=as.numeric(quantile(t2, probs = 0.8)), slope1=0.000003, slope2=0.000003, max.sel=0.5, minsel.upper=0, plot=F)$selectivity 

activity_df %<>%
  dplyr::mutate(indSAAG.fert = tidyr::replace_na(indSAAG.fert, 0)) %>%
  dplyr::rename(SAAG.F = indSAAG.fert)




summary(activity_df)

ggplot() +
  geom_line(data = activity_df, aes(x = times, y = temp_smooth)) + 
  geom_line(data = activity_df, aes(x = times, y = SAAG.F)) + 
  theme_bw() 


sigmoid = function(x) {
  1 / (1 + exp(-x))
}

he <- activity_df %>%
  dplyr::mutate(he_1A4ai = temp_smooth^-1 * SAAG.F) %>%
  dplyr::mutate(he_sig = sigmoid(scale(he_1A4ai))) %>% # Prebacuje sve na vrednost izmedju 0 i 1
  dplyr::mutate(he_1A4ai = he_sig) %>%
  dplyr::mutate(he_1A4ai_n = he_sig/sum(he_sig))%>%
  select(times, he_1A4ai, he_1A4ai_n)

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-03-31 24"),
                       by   = dhours(1)) 
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
ggplot(he, aes(x = times, y = he_1A4ai)) +
  geom_point(size = 0.1) +
  geom_line(colour = "deepskyblue") + 
  #geom_smooth()+
  theme_bw() + 
  ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)+ 
  # labs( caption = "he_1A4ai = ((WT0622+0.5)) / PH2 * (TEMP*(-1)+30)")+
  theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "black"))












# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Repair - overhaul period
# Q3 2015 -- July 1, 2015 to September 30, 2015
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tRP <- seq.POSIXt(from = ymd_h("2015-07-01 00"),
                  to   = ymd_h("2015-07-14 23"),
                  by   = dhours(1)) 

activity_df$indRP[activity_df$times %in% tRP] <- TRUE 
activity_df$indRP[!(activity_df$times %in% tRP)] <- FALSE 

activity_df %<>% 
  dplyr::mutate(RP = sin(((2*pi)/4)*(indRP)))


p <- ggplot(activity_df, aes(x = times, y = RP, colour = "red")) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-01-03 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Temperature
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

load("Hourly_emissions/Data/TEMP_SLP/ogimet_temp_stfdf.rda")
stfdf_temp

temp <- as.data.frame(stfdf_temp)
temp$time1 <- as.Date.POSIXct(temp$time)

time_per_day <- seq(from = ymd('2015-01-01'),
                    to   = ymd('2015-12-31'),
                    by   = 'day')




temp_2015 <- temp %>% subset(temp$time1 %in% time_per_day)

temp_2015 %<>% 
  tidyr::drop_na(tmean) %>%
  dplyr::group_by(time1) %>%
  dplyr::summarize(meanT = mean(tmean))



temp_2015 <- temp_2015[rep(seq.int(1,nrow(temp_2015)), each = 24),]

activity_df <- data.frame()
activity_df$TEMP <- temp_2015$meanT

# za profiniti - ideja ...
#day_light <- day_hours %in% c(7:20) #19
#activity_df$day_light <- day_light 
#activity_df %<>% 
#  dplyr::mutate(DL = dplyr::case_when(day_light == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours-7)) + dayl,
#                                      day_light == FALSE ~ 0)) #dayl

m <- loess(TEMP~as.numeric(times), activity_df, span = 0.235)
predict(m)

activity_df$temp_smooth <- predict(m)

p <- ggplot(activity_df, aes(x = times, y = TEMP)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ TEMP, colour = "orange")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sea level pressure
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

load("Hourly_emissions/Data/TEMP_SLP/ogimet_slp_stfdf.rda")
stfdf

slp <- as.data.frame(stfdf)
slp$time1 <- as.Date.POSIXct(slp$time)

time_per_day <- seq(from = ymd('2015-01-01'),
                    to   = ymd('2015-12-31'),
                    by   = 'day')




slp_2015 <- slp %>% as.data.frame() %>% subset(time1 %in% time_per_day)

slp_2015 %<>% 
  tidyr::drop_na(slp) %>%
  dplyr::group_by(time1) %>%
  dplyr::summarize(meanSLP = mean(slp))



slp_2015 <- slp_2015[rep(seq.int(1,nrow(slp_2015)), each = 24),]


activity_df$SLP <- slp_2015$meanSLP

p <- ggplot(activity_df, aes(x = times, y = SLP)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw() +
  geom_smooth(formula =  ~ SLP, colour = "blue")

time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Number of flights per hour
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# library(statsr)
# data(nycflights)
# nycflights
# 
# unique(nycflights$year)
# nycflights$year <- 2015
# nycflights$time <- paste(nycflights$year, nycflights$month, nycflights$day, sep = "-") %>% 
#   ymd() %>% 
#   as.Date() 
# nycflights %<>% arrange(time)
# nycflights %<>% dplyr::group_by(time) %>% tally()
# 
# flights_2015 <- nycflights[rep(seq.int(1,nrow(nycflights)), each = 24),]
# flights_2015 %<>% rename(NFH = n)

activity_df$dayl <- dayl$Daylength
day_light <- day_hours %in% c(7:20) #19
activity_df$day_light <- day_light 
activity_df %<>% 
  dplyr::mutate(NFH = dplyr::case_when(day_light == TRUE ~ 0.5*sin(((2*pi)/24)*(day_hours)) + dayl+0.2,
                                      day_light == FALSE ~ dayl)) #dayl



# dplyr::mutate(A4 = 0.5*sin(0.5*(day_hours-16)) + 0.5) 
p <- ggplot(activity_df, aes(x = times, y = NFH)) +
  geom_point(size = 0.5) +
  geom_line() + 
  theme_bw()+
  geom_smooth(formula =  ~ NFH, colour = "green")
time_seq <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                       to   = ymd_h("2015-02-28 24"),
                       by   = dhours(1)) 

p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)





# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Electricity production
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

activity_df <- readRDS(file = "Version_2_update/Temporalization/activity_df_new.rds")

elp_2018 <- readxl::read_xlsx(path ="Version_2_update/Temporalization/Electricity_production_by_month_2018.xlsx")




t1 <- seq.POSIXt(from = ymd_h("2015-01-01 00"),
                     to   = ymd_h("2015-01-31 23"),
                     by   = dhours(1)) 
t2 <- seq.POSIXt(from = ymd_h("2015-02-01 00"),
                     to   = ymd_h("2015-02-28 23"),
                     by   = dhours(1)) 
t3 <- seq.POSIXt(from = ymd_h("2015-03-01 00"),
                 to   = ymd_h("2015-03-31 23"),
                 by   = dhours(1)) 
t4 <- seq.POSIXt(from = ymd_h("2015-04-01 00"),
                 to   = ymd_h("2015-04-30 23"),
                 by   = dhours(1)) 
t5 <- seq.POSIXt(from = ymd_h("2015-05-01 00"),
                 to   = ymd_h("2015-05-31 23"),
                 by   = dhours(1)) 
t6 <- seq.POSIXt(from = ymd_h("2015-06-01 00"),
                 to   = ymd_h("2015-06-30 23"),
                 by   = dhours(1)) 
t7 <- seq.POSIXt(from = ymd_h("2015-07-01 00"),
                 to   = ymd_h("2015-07-31 23"),
                 by   = dhours(1)) 
t8 <- seq.POSIXt(from = ymd_h("2015-08-01 00"),
                 to   = ymd_h("2015-08-31 23"),
                 by   = dhours(1)) 
t9 <- seq.POSIXt(from = ymd_h("2015-09-01 00"),
                 to   = ymd_h("2015-09-30 23"),
                 by   = dhours(1)) 
t10 <- seq.POSIXt(from = ymd_h("2015-10-01 00"),
                 to   = ymd_h("2015-10-31 23"),
                 by   = dhours(1)) 
t11 <- seq.POSIXt(from = ymd_h("2015-11-01 00"),
                 to   = ymd_h("2015-11-30 23"),
                 by   = dhours(1)) 
t12 <- seq.POSIXt(from = ymd_h("2015-12-01 00"),
                 to   = ymd_h("2015-12-31 23"),
                 by   = dhours(1)) 


activity_df$indE[activity_df$times %in% t1] <- elp_2018$`Distribucije [MWh]`[1] 
activity_df$indE[(activity_df$times %in% t2)] <-  elp_2018$`Distribucije [MWh]`[2]
activity_df$indE[activity_df$times %in% t3] <- elp_2018$`Distribucije [MWh]`[3] 
activity_df$indE[(activity_df$times %in% t4)] <-  elp_2018$`Distribucije [MWh]`[4]
activity_df$indE[activity_df$times %in% t5] <- elp_2018$`Distribucije [MWh]`[5] 
activity_df$indE[(activity_df$times %in% t6)] <-  elp_2018$`Distribucije [MWh]`[6]
activity_df$indE[activity_df$times %in% t7] <- elp_2018$`Distribucije [MWh]`[7] 
activity_df$indE[(activity_df$times %in% t8)] <-  elp_2018$`Distribucije [MWh]`[8]
activity_df$indE[activity_df$times %in% t9] <- elp_2018$`Distribucije [MWh]`[9] 
activity_df$indE[(activity_df$times %in% t10)] <-  elp_2018$`Distribucije [MWh]`[10]
activity_df$indE[activity_df$times %in% t11] <- elp_2018$`Distribucije [MWh]`[11] 
activity_df$indE[(activity_df$times %in% t12)] <-  elp_2018$`Distribucije [MWh]`[12]


activity_df %<>% dplyr::mutate(EC = indE)

ggplot()+
  geom_line(data = activity_df, aes(x = times, y = EC), colour = "red")+
  ggtitle("Monthly consumption of electricity in the Republic of Serbia in 2018 in MWh")+
  geom_point(size = 0.5) +
  theme_bw()






# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# FINAL DATA FRAME

saveRDS(activity_df, file = "Version_2_update/Temporalization/activity_df_new.rds")
rm(list = ls())
activity_df <- readRDS(file = "Version_2_update/Temporalization/activity_df_new.rds")
names(activity_df)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





























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