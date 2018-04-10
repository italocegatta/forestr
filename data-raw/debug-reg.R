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


readRDS("data-raw/base_dia.RDS") %>%
  fill_reg(nome, data, lon, lat, t_min, min_r2 = 0.1)


weather %>%
  filter(is.na(rh_min)) %>%
  pull(id) %>%
  unique()

weather %>%
  group_by(id, city) %>%
  summarise_at(vars(t_min, rh_min, prec), function(x) sum(is.na(x)))

weather_fill <- weather %>%
  fill_reg(id, date, lon, lat, t_min) %>%
  fill_reg(id, date, lon, lat, rh_min) %>%
  fill_idw(id, lon, lat, prec)

weather_fill %>%
  group_by(id, city) %>%
  summarise_at(vars(t_min, rh_min, prec), function(x) sum(is.na(x)))




weather_fill %>%
  mutate(
    rise = sun_rise(date, lon, lat),
    set = sun_set(date, lon, lat)
  ) %>%
  #filter(is.na(rad)) %>%
  filter(hour(time) > rise, hour(time) < set) %>% View()
  ggplot(aes(time, rad, group = date)) +
  geom_line() +
  facet_wrap(~city)

weather_fill %>%
  filter(is.na(rad)) %>%
  #filter(hour(time) > 5,  hour(time) < 18) %>% View()
  ggplot(aes(factor(hour(time)))) +
    geom_bar() +
    facet_wrap(~city)

weather %>%
  select(city, date, time) %>%
  filter(hour(time) < 3)


as.hms("05:00:00") - as.hms("01:00:00")
