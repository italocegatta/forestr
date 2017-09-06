# rotina para consistencia de chuva

# ideias: relação com UR, Rad e T_med
# Grid para geofacet
library(tidyverse)
library(inmetdown)
library(forestr)
library(lubridate)
library(plotly)

aux_est <- aws_stations() %>%
  select(id, estado = state, lon, lat)

df <- list.files("data-raw/inmet/", full.names = TRUE) %>%
  map_df(read_csv2) %>%
  as_tibble() %>%
  left_join(aux_est, by = c("estacao" = "id")) %>%
  filter(estado == "SP") %>%
  mutate(
    hora = hour(data),
    data_hora = data,
    data = as.Date(data)
  ) %>%
  select(estacao, lon, lat, data_hora, data, hora, t_max:ur_min, rad, prec)

df %>%
  mutate(mes = as.Date(cut(data, breaks = "day"))) %>%
  group_by(estacao, lon, lat, mes) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  filter(estacao == "A764") %>%
  View()

df %>%
  mutate(mes = as.Date(cut(data, breaks = "month"))) %>%
  group_by(estacao, lon, lat, mes) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  {ggplot(., aes(lon, lat, color = prec, size = prec, label = estacao, frame = mes)) +
      geom_point() +
      scale_color_viridis_c() +
      theme_bw()
  } %>%
  ggplotly() %>%
  animation_opts(
    frame = 700,
    transition = 100,
    redraw = FALSE
  ) %>%
  animation_slider()

df %>%
  mutate(semana = as.Date(cut(data, breaks = "week"))) %>%
  group_by(estacao, lon, lat, semana) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  {ggplot(., aes(lon, lat, color = prec, size = prec, label = estacao, frame = semana)) +
    geom_point() +
    scale_color_viridis_c() +
    theme_bw()
  } %>%
  ggplotly() %>%
  animation_opts(
    frame = 500,
    transition = 100,
    redraw = FALSE
  ) %>%
  animation_slider()


df %>%
  group_by(estacao, lon, lat, data) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup() %>%
  {ggplot(., aes(lon, lat, color = prec, size = prec, label = estacao, frame = data)) +
     geom_point() +
     scale_color_viridis_c() +
     theme_bw()
  } %>%
  ggplotly() %>%
  animation_opts(
    frame = 400,
    transition = 100,
    redraw = FALSE
  ) %>%
  animation_slider()

