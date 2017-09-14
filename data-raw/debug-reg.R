library(forestr)
library(dplyr)
library(lubridate)
library(ggplot2)


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
