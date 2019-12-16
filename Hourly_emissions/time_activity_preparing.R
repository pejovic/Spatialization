library(tsibble)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggforce)
library(feasts)
library(dplyr)
library(stringr)


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







############# Vremenski brojaci #############################


read_excel_allsheets <- function(filename, tibble = FALSE, Range, sheets) {
  sheets <- readxl::excel_sheets(filename)[sheets]
  ranges <- c("B9:Y71", "B9:Y65", "B9:Y71", "B9:Y69", "B9:Y71", "B9:Y69", "B9:Y71", "B9:Y71", "B9:Y69", "B9:Y71", "B9:Y69", "B9:Y71")
  xlist <- as.list(sheets)
  for(i in 1:12){
    xlist[[i]] <- readxl::read_excel(filename, sheet = sheets[i], range = ranges[i], col_names = TRUE)
  }
  names(xlist) <- sheets
  return(xlist)
}

path <- "D:/Projekti/Spatialisation/03 DETALJNI PODACI SA BROJACA/"

files <- list.files(path, full.names = TRUE)


aa <- lapply(files, function(X) read_excel_allsheets(filename = X, sheets = 2:13))

file.names <- substr(list.files(path, full.names = FALSE), start = 2, stop = 5)


f1091 <- read_excel_allsheets(filename = paste(path, " 1091.xls", sep = ""), sheets = 2:13)

days_seq <- rep(seq(from = as.Date("2015-01-01"),
                    to = as.Date("2015-12-31"),
                    by = "days"), each = 2)

days_months <- lubridate::month(days_seq)

ttt <- function(x){
  days_seq <- rep(seq(from = as.Date("2015-01-01"),
                      to = as.Date("2015-12-31"),
                      by = "days"), each = 2)
  
  days_months <- lubridate::month(days_seq)
  
  x %>% do.call(rbind, .) %>%
    dplyr::rename_all(.,  list(~paste(1:24, "h", sep = ""))) %>%
    dplyr::mutate(days = days_seq, months = days_months) %>%
    dplyr::select(months, days, everything()) %>%
    group_by(days) %>%
    summarise_each(funs(mean), -months) %>%
    tidyr::pivot_longer(-days, names_to = "hours", values_to = "count") %>%
    dplyr::mutate(time = times) %>%
    dplyr::select(time, count)
}

lapply(aa[1:92], ttt)

aaa <- lapply(aa, ttt) %>%
  bind_cols() %>%
  dplyr::select(starts_with("count")) %>%
  dplyr::rename_all(.,  list(~file.names)) %>%
  dplyr::mutate(time = times) %>%
  dplyr::select(time, everything()) %>%
  # as_tsibble() %>%
  # dplyr::select(time, starts_with("101")) %>% # seleckija samo 20
  # ::select(starts_with("100")) %>%
  # apply(., 1, function(x) c = mean(x))
    tidyr::pivot_longer(-time, names_to = "brojac", values_to = "count")


aaa %>% dplyr::select(starts_with("100")) %>%
  rowwise() %>%
  do( (.) %>% as.data.frame %>% mutate(c = mean(.)) ) %>%
  ungroup()


p <- aaa %>% ggplot(., aes(x = time, y = count, color = brojac)) + geom_smooth() + theme_bw()

time_seq <- seq.POSIXt(from = ymd_h("2015-04-01 00"),
                       to   = ymd_h("2015-04-02 23"),
                       by   = dhours(1))

# p + ggforce::facet_zoom(x = times %in% time_seq, horizontal = FALSE, zoom.size = .6)


autoplot(aaa)



aaa <- aa[[1]] %>% do.call(rbind, .) %>%
  dplyr::rename_all(.,  list(~paste(1:24, "h", sep = ""))) %>%
  dplyr::mutate(days = days_seq, months = days_months) %>%
  dplyr::select(months, days, everything()) %>%
  group_by(days) %>%
  summarise_each(funs(mean), -months) %>%
  tidyr::pivot_longer(-days, names_to = "hours", values_to = "count") %>%
  dplyr::mutate(time = times) %>%
  dplyr::select(time, count)


dim(aa)

library(feasts)

aa %>% as.tsibble() %>% autoplot(count)

aa %>% as.tsibble() %>% gg_season(count)

aa %>% as.tsibble() %>% gg_subseries(count)

aa %>% as.tsibble() %>% gg_tsdisplay(count)

add_info <- function(x){
  names(x) <- paste(1:24, "h", sep = "")
  x <- do.call(rbind, x)
  
  
}









# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Ucitavanje fajlova
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# devtools::install_github("basarabam/SerbianCyrLat")
library(SerbianCyrLat)

Sys.setlocale(locale = 'Serbian (Cyrillic)', category = 'LC_ALL')


data.brojaci <- list.files('D:/Projekti/Spatialisation/03 DETALJNI PODACI SA BROJACA/')


list.files('D:/Projekti/Detaljni podaci o saobracaju sa automatskih brojaca saobracaja/03 DETALJNI PODACI SA BROJACA/')

data.br.list <- list()                                                   
for(i in 1:length(data.brojaci)){                                             
  data.br.list[[i]] <- readxl::read_xls(paste("D:/Projekti/Detaljni podaci o saobracaju sa automatskih brojaca saobracaja/03 DETALJNI PODACI SA BROJACA/",data.brojaci[i], sep = ""))
}


Encoding(data.brojaci) <- 'UTF-8' 

lcl(loc = "C")
otpadne_vode <- cyr_lat(data.brojaci)


Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")



Sys.setlocale(category = "LC_ALL", locale = "Serbian (Cyrillic)")
test = enc2utf8(c("привет","пока"))

install.packages("fs")
library(fs)
fs::dir_ls("D:/Projekti/Spatialisation/03 DETALJNI PODACI SA BROJACA/")

data.list <- fs::dir_ls("D:/Projekti/Spatialisation/03 DETALJNI PODACI SA BROJACA/")

data.br.list <- list()                                                   
for(i in 1:length(data.list)){                                             
  data.br.list[[i]] <- readxl::read_xls(data.list[[i]])
}

path <- "D:/Projekti/Spatialisation/03 DETALJNI PODACI SA BROJACA/"



counter_ids <- substr(list.files(path = path), start = 2, stop = 5)

file.rename(from = data.list, to = paste(path, counter_ids))
file.rename(from = paste(path, counter_ids), to = paste(paste(path, counter_ids), "xls", sep = "."))

paste(path, counter_ids)

