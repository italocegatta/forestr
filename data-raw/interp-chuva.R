library(tidyverse)
library(inmetdown)
library(forestr)
library(lubridate)
library(sf)
library(sp)
library(gstat)

aux_est <- aws_stations() %>%
  select(id, estado = state, lon, lat)

df <- list.files("data-raw/inmet/", full.names = TRUE) %>%
  map_df(read_csv2) %>%
  as_tibble() %>%
  left_join(aux_est, by = c("estacao" = "id")) %>%
  mutate(
    hora = hour(data),
    data_hora = data,
    data = as.Date(data)
  ) %>%
  select(estacao, lon, lat, data_hora, data, hora, t_max:ur_min, rad, prec)

df_mes <- df %>%
  mutate(data = as.Date(cut(data, breaks = "month"))) %>%
  group_by(estacao, lon, lat, data) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  ungroup()

df_mes %>%
  ggplot(aes(data, estacao, fill = prec)) +
  geom_tile() +
  scale_fill_viridis_c()

df_sp <- df_mes
coordinates(df_sp) <- ~lon+lat

grd              <- as.data.frame(spsample(df_sp, type="regular", cellsize = 0.1))
names(grd)       <- c("x", "y")
coordinates(grd) <- c("x", "y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

vgm_model <- fit.variogram(variogram(prec~1, data = df_sp), model = vgm("Exp"))

for (i in unique(df_mes$data)) {

  krig <- autoKrige(prec ~ 1, subset(df_sp, data == i), grd)
ncell(krig)
  p <- krig$krige_output %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) +
  geom_tile(aes(fill=var1.pred)) +
  geom_point(
    data = as.data.frame(subset(df_sp, data == i)),
    aes(lon, lat, size= prec),
    shape = 21
  ) +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

  print(p)
}

raster::raster(krig$krige_output["var1.pred"]) %>%
  raster::writeRaster(filename="test.tif", format="GTiff", overwrite=TRUE)
