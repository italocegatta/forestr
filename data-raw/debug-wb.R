library(tidyverse)
library(forestr)


date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), "month")
cad = 100
lat = -22.93
t_med = c(23.10, 23.50, 23.00, 21.10, 18.70, 17.40, 17.30, 18.90, 20.10, 21.20, 22.00, 22.50)
prec =  c(240.20, 190.90, 147.30, 71.00, 65.10, 48.70, 36.80, 37.40, 65.60, 123.60, 137.50, 217.10)
i = 111 # 103.7919
a = 2.5 # 2.283487

df <- data_frame(date, cad, lat, t_med, prec, i, a)

base <- df %>%
  mutate(etp = round(etp(date, t_med, lat, 111, 2.5), 1)) %>%
  select(date, prec, etp)

etp <- base$etp

base %>%
  mutate(p_etp = prec - etp)

mutate(df, id = 1) %>%
  bind_rows(mutate(data_frame(date, cad, lat, t_med, prec, i = 103.7919, a = 2.283487), id = 2)) %>%
  group_by(id) %>%
  mutate(etp = round(etp(date, t_med, lat, i, a), 1)) %>%
  View()


round(16 * ((10 * (t_med / i))^a) * (n_hours / 12) * (n_days / 30), 1)

df3 <- bind_rows(df, df, .id = "id")

write.csv2(df, "teste_etp.csv")

df3_etp <- df3 %>%
  group_by(id) %>%
  mutate(etp = etp(date, t_med, lat))

