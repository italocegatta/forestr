library(tidyverse)
library(forestr)

df = sp_weather
i = 1


# Tem falha? --------------------------------------------------------------

if (is_complete(head(df), t_min)) return(df)


# Quis estações? ----------------------------------------------------------


k_miss <- key_miss(df, id, t_min)
# "A705"

########### inicio do for ou funcional ####################################


# Pode ajustar regressão? -------------------------------------------------

df_i <- dplyr::filter(df, id == k_miss[i])

if (prop_miss(df_i, id, t_min) > 70) {
  stop("tentar IDW")
}


# Estação mais proxima ----------------------------------------------------

nearest_i <- near_st(df, id, lon, lat, k_miss[i])


# Tem recobrimento? -------------------------------------------------------

missing_cover(df, df_i, id, t_min, date, nearest_i)


# Ajuste é bom? -----------------------------------------------------------

lm_i <- fit_model(df, id, t_min,  k_miss[i], nearest_i)

if (summary(lm_i)$r.squared < 0.8) {
  stop("proxima estação")
}


# Preenche as falhas ------------------------------------------------------

predict_value(df, id, t_min,  k_miss[i], nearest_i)

