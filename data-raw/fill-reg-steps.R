library(tidyverse)
library(forestr)

df = sp_weather
i = 1


# Tem falha? --------------------------------------------------------------

if (is_complete(df, t_min)) return(df)


# Quis estações? ----------------------------------------------------------


list_id_miss <- id_miss(df, id, t_min)
# "A705"

########### inicio do for ou funcional ####################################


# Pode ajustar regressão? -------------------------------------------------

df_i <- dplyr::filter(df, id == list_id_miss[i])

if (prop_miss(df_i, id, t_min) > 70) {
  stop("tentar IDW")
}


# Estação mais proxima ----------------------------------------------------

nearest_i <- near_st(df, id, lon, lat, list_id_miss[i])

df_nxt <- dplyr::filter(df, id == nearest_i)


# Tem recobrimento? -------------------------------------------------------

if (coverage(df_i, df_nxt, id, date, t_min) != 100) {
  stop("Tentar a proxima estacao")
}


# Ajuste é bom? -----------------------------------------------------------

lm_i <- fit_model(df_i, df_nxt, t_min)

if (summary(lm_i)$r.squared < 0.8) {
  stop("proxima estação")
}


# Preenche as falhas ------------------------------------------------------

predict_value(df_i, df_nxt, t_min, lm_i)

