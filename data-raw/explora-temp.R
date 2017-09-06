# exploratorio temperatura

library(tidyverse)
library(lubridate)
library(plotly)
library(forestr)

df <- list.files("data-raw/inmet/", full.names = TRUE) %>%
  map_df(read_csv2) %>%
  as_tibble() %>%
  left_join(
    select(inmetdown::aws_stations(), id, estado = state, lon, lat),
    by = c("estacao" = "id")
  ) %>%
  filter(estado == "SP") %>%
  mutate(
    hora = hour(data),
    data_hora = data,
    data = as.Date(data)
  ) %>%
  select(estacao, lon, lat, data_hora, data, hora, t_min, t_max) %>%
  rowwise() %>%
  mutate(t_med = round(mean(c(t_min, t_max)), 1)) %>%
  ungroup()

df_dia <- df %>%
  group_by(estacao, lon, lat, data) %>%
  summarise(
    t_min = summ_var_part(hora, t_min, min, groups = 3, n_min = 5),
    t_med = round(summ_var_part(hora, t_med, mean, groups = 3, n_min = 5), 1),
    t_max = summ_var_part(hora, t_max, max, groups = 3, n_min = 5)
  ) %>%
  ungroup()

df_mes <- df_dia %>%
  mutate(mes = as.Date(cut(data, breaks = "month"))) %>%
  group_by(estacao, lon, lat, mes) %>%
  summarise(
    t_min = summ_var(t_min, min, n_min = 25),
    t_med = round(summ_var(t_med, mean, n_min = 25), 1),
    t_max = summ_var(t_max, max, n_min = 25)
  ) %>%
  ungroup()

df_dia %>%
  gather(temp,valor, t_min:t_max) %>%
  ggplot(aes(data,valor, color = temp)) +
  geom_line() +
  geom_point() +
  facet_wrap(~estacao) +
  theme_bw()

df_mes %>%
  gather(temp,valor, t_min:t_max) %>%
  ggplot(aes(mes,valor, color = temp)) +
    geom_line() +
    geom_point() +
    facet_wrap(~estacao) +
    theme_bw()
