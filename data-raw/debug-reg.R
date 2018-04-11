library(forestr)
library(dplyr)
library(lubridate)
library(ggplot2)

library(forestr)
data("weather")

.data = weather
id = quo(id)
key = quo(date)
lon = quo(lon)
lat = quo(lat)
value = quo(rh_min)
min_coverage = 80
max_iter = 10
min_r2 = 0.8

.data <- readRDS("data-raw/base_dia.RDS")
id = quo(nome)
key = quo(data)
lon = quo(lon)
lat = quo(lat)
value = quo(t_min)
min_coverage = 80
max_iter = 10
min_r2 = 0.8



fill_reg(weather, id, date, lon, lat, rh_min, min_r2 = 0.1) %>%
  filter(is.na(rh_min))

x <- readRDS("data-raw/base_dia.RDS")
x_fill <- x %>%
  fill_reg(nome, data, lon, lat, t_med, min_r2 = 0.1)

#
x %>%
  filter(nome == "A769")

x_fill %>%
  filter(nome == "A769")

x %>%
  filter(is.na(t_med))

x_fill %>%
  filter(is.na(t_med))




