library(tidyverse)
library(forestr)

# pereira

date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), "month")
cad = rep(125, 12)
lat = -7.1430
prec = c(41, 55, 100, 129, 95, 107, 124, 58, 38, 17, 19, 21)
etp = c(108, 109, 115, 107, 95, 80, 62, 78, 77, 102, 108, 117)
p_etp <- prec - etp


# sentelhas

date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), "month")
cad = rep(100, 12)
lat = -22.93
t_med = c(23.10, 23.50, 23.00, 21.10, 18.70, 17.40, 17.30, 18.90, 20.10, 21.20, 22.00, 22.50)
prec =  c(240.20, 190.90, 147.30, 71.00, 65.10, 48.70, 36.80, 37.40, 65.60, 123.60, 137.50, 217.10)
i = 111 # 103.7919
a = 2.5 # 2.283487
etp <- round(etp(date, t_med, lat, 111, 2.5), 1)
p_etp <- prec - etp


if (neg_acum[i] > cad[i]) {
  neg_acum[i] <- cad[i]
}

