library(forestr)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)


weather %>%
  group_by(id, city) %>%
  summarise_at(vars(t_max:prec), function(x) sum(is.na(x)))

weather_fill <- weather %>%
  fill_reg(id, date, lon, lat, t_max) %>%
  fill_reg(id, date, lon, lat, t_min) %>%
  fill_reg(id, date, lon, lat, rh_max) %>%
  fill_reg(id, date, lon, lat, rh_min) %>%
  fill_reg(id, date, lon, lat, dp_max) %>%
  fill_reg(id, date, lon, lat, dp_min) %>%
  fill_reg(id, date, lon, lat, ap_max) %>%
  fill_reg(id, date, lon, lat, ap_min) %>%
  fill_reg(id, date, lon, lat, ws) %>%
  fill_reg(id, date, lon, lat, wg) %>%
  fill_reg(id, date, lon, lat, wd) %>%
  fill_reg(id, date, lon, lat, rad) %>%
  fill_idw(id, lon, lat, prec)

weather_fill %>%
  group_by(id, city) %>%
  summarise_at(vars(t_max:prec), function(x) sum(is.na(x))) %>%
  ungroup()




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