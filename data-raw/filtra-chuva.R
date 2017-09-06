# identifica estacoes com chuva zerada

library(tidyverse)
library(lubridate)
library(forestr)
library(plotly)
library(sp)
library(automap)

aux_est <- inmetdown::aws_stations() %>%
  select(id, estado = state, lon, lat)

# 5 meses
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
  select(estacao, lon, lat, data_hora, data, hora, prec)

df %>%
  mutate(mes = as.Date(cut(data, breaks = "month"))) %>%
  group_by(estacao, lon, lat, mes) %>%
  summarise(
    prec = summ_var(prec, sum, n_min = 25)
  ) %>%
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

df_resumo <- df %>%
  mutate(dia = as.Date(cut(data, breaks = "day"))) %>%
  group_by(estacao, lon, lat, dia) %>%
  summarise(
    prec = summ_var(prec, sum, n_min = 20)
  ) %>%
  mutate(mes = as.Date(cut(dia, breaks = "month"))) %>%
  group_by(estacao, lon, lat, mes) %>%
  summarise(
    prec = summ_var(prec, sum, n_min = 25)
  ) %>%
  ungroup()


# - -----------------------------------------------------------------------

create_grid <- function(sp, cellsize = 0.1) {
  grd              <- as.data.frame(spsample(sp, type="regular", cellsize = cellsize))
  names(grd)       <- c("lon", "lat")
  coordinates(grd) <- c("lon", "lat")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object

  grd
}

grd <- df_resumo %>%
  filter(mes == "2016-08-01") %>%
  nest() %>%
  mutate(sp = data %>% map(~SpatialPointsDataFrame(cbind(lon = .x$lon, lat = .x$lat), .x))) %>%
  '[['(1,2) %>%
  create_grid()

# BRUTO

df_krig_bruto <-  df_resumo %>%
  filter(!is.na(prec)) %>%
  group_by(mes) %>%
  nest() %>%
  mutate(
    sp = data %>% map(
      ~SpatialPointsDataFrame(cbind(lon = .x$lon, lat = .x$lat), .x)
    )
  ) %>%
  mutate(
    krig = map(
      sp, ~autoKrige(
        formula = prec ~ lon + lat,
        input_data = .x,
        new_data = grd
      )
    )
  ) %>%
  mutate(df = map(krig, ~as.data.frame(.x$krige_output))) %>%
  unnest(df)

df_krig_bruto %>%
{ggplot(., aes(frame = mes)) +
    geom_point(aes(lon, lat, color = var1.pred), size = 3) +
    geom_point(
      data = df_resumo,
      aes(x = lon, y = lat, size= prec, label = estacao),
      shape = 21
    ) +
    scale_color_viridis_c("prec") +
    theme_bw()
} %>%
  ggplotly() %>%
  animation_opts(
    frame = 700,
    transition = 0,
    redraw = FALSE
  ) %>%
  animation_slider()


# FILTRADO

df_filtro <- df_resumo %>%
  filter(!is.na(prec)) %>%
  filter(!(estacao == "A738" & mes %in% as.Date(c("2016-11-01", "2016-12-01")))) %>%
  filter(!(estacao == "A747" & mes %in% as.Date(c("2016-10-01", "2016-11-01"))))

df_krig <- df_filtro %>%
  group_by(mes) %>%
  nest() %>%
  mutate(
    sp = data %>% map(
      ~SpatialPointsDataFrame(cbind(lon = .x$lon, lat = .x$lat), .x)
    )
  ) %>%
  mutate(
    krig = map(
      sp, ~autoKrige(
        formula = prec ~ lon + lat,
        input_data = .x,
        new_data = grd
      )
    )
  ) %>%
  mutate(df = map(krig, ~as.data.frame(.x$krige_output))) %>%
  unnest(df)

# df_krig %>%
#   mutate(var1.pred = ifelse(var1.pred < 0, 0, var1.pred)) %>%
#   ggplot(aes(lon, lat, fill = var1.pred)) +
#   geom_raster() +
#   scale_fill_viridis_c("Chuva")  +
#   facet_wrap(~mes) +
#   theme_bw(18) +
#   theme(legend.justification = "top", legend.key.height = unit(3, "cm"))

df_krig %>%
  {ggplot(., aes(frame = mes)) +
      geom_point(aes(lon, lat, color = var1.pred), size = 3) +
      geom_point(
        data = df_filtro,
        aes(x = lon, y = lat, size= prec, label = estacao),
        shape = 21
      ) +
      scale_color_viridis_c("prec") +
      theme_bw()
  } %>%
    ggplotly() %>%
    animation_opts(
      frame = 700,
      transition = 0,
      redraw = FALSE
    ) %>%
    animation_slider()

