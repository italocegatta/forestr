library(forestr)
library(dplyr)

weather %>%
  filter(id %in% c("A725", "A763", "A842", "A708")) %>%
  rename(est = id) %>%
  fill_reg(est, date, t_min, lon, lat) %>%
  .$t_min %>% mean


weather %>%
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
  fill_wid(id, date, lon, lat, prec) %>%
  group_by(id, city) %>%
  summarise_at(vars(t_max:prec), function(x) sum(is.na(x)))

weather %>%
  fill_idw("date", "lon", "lat", "prec", 200) %>%
  group_by(id, city) %>%
  summarise_at(vars(t_max:prec), function(x) sum(is.na(x)))

weather %>%
group_by(id, city) %>%
summarise_at(vars(t_max:prec), function(x) sum(is.na(x)))

weather %>%
  group_by(id) %>%
  summarise(n = n())
