
# Setup -------------------------------------------------------------------
library(tidyverse)
library(forestr)

df = sp_weather

# Tem falha? --------------------------------------------------------------


if (all(!is.na(df$t_min))) return(df)


# falhas
# "A705" "A706" "A738" "A763" "A718" "A701" "A768" "A734"

# Pode ajustar regressão? -------------------------------------------------

# id com falha
m <- unique(df[is.na(df$t_min), ]$id)

f <- filter(df, id == "A705")

# porcentagem de falhas (%)
p <- nrow(filter(f, is.na(t_min))) / nrow(f) * 100

if (p > 30) stop("Tentar interpolação")


near_st <- df %>%
  group_by(id, lat, lon) %>%
  summarise() %>%
  ungroup() %>%
  mutate(candidate = search_nearest_st(id, lon, lat)) %>%
  filter(id == "A705") %>%
  .$candidate


yy <- df$t_min[df$id == "A705"]
xx <- df$t_min[df$id == near_st]
lm <- lm(yy ~ xx)
r2 <- summary(lm)$r.squared

if (r2 < 0.7) stop("basca a proxima estação")

d <- f[is.na(f$t_min), ]$date

df[df$date == d & df$id == "A705", ]$t_min <-
  predict(lm , df$t_min[df$id == near_st]




# splt missing values
a <- split(df, is.na(df[["t_min"]]))




# identifica os ids faltantes
k <- unique(a[["TRUE"]][["id"]])

filter(df, id %in% k) %>%
  group_by(id) %>%
  summarise(
    n = n(),
    miss = sum(is.na(t_min)),
    p = round((miss / n) * 100, 1)
  )





# estacoes candidatas
candi <- df %>%
  group_by(id, lat, lon) %>%
  summarise() %>%
  ungroup() %>%
  mutate(candidate = search_nearest_st(id, lon, lat, 1)) %>%
  select(id, candidate) %>%
  filter(id %in% k)

miss_cand <- dplyr::filter(a[["FALSE"]], id %in% c("A718", "A707")) %>%
  mutate(.aux_id = 1:nrow(.)) %>%
  tidyr::spread(id, t_min)

ggplot(miss_cand, aes(A707, A718)) +
  geom_point()

#purrr::map(a, ~dplyr::filter(.x, id %in% c("A718", "A707")))

# separa dados para as id-datas faltantes
b <- a[["FALSE"]][a[["FALSE"]][[key]] %in% k, ]



th(aux, search_nearest_st(id, lon, lat, 1))




# traz a lon e lat dos dados faltantes para a base de interpolacao
b_join <- dplyr::left_join(b, a[["TRUE"]] , by = key)

# calcula da distancia dentre os pontos disponiveis e as id-datas
b_join$dis <- forestr::haversine(
  b_join$lon.x, b_join$lat.x,
  b_join$lon.y, b_join$lat.y
)

# limita o raio de busca para interpolacao
#
# falta retornar erro quando não achar nenhuma estacao
#b_join <- dplyr::filter(b_join, dis < radius)

# calcula o idw para cada id-data faltante
j <- dplyr::summarise_(
  dplyr::group_by_(b_join, ".aux_id.y", key),
  .dots =  list(
    .zz = lazyeval::interp(
      ~forestr::idw(v, d),
      .values = list(
        v = as.name(paste0(x, ".x")),
        d = quote(dis)
      )
    )
  )
)

# junta id-data com idw
j_id <- dplyr::left_join(a[["TRUE"]], j, by = c(key, ".aux_id" = ".aux_id.y"))

# subistitui NA por idw
a[["TRUE"]]$prec <- j_id$.zz

# junta id-data faltante com base interpolacao
z <- dplyr::arrange(dplyr::bind_rows(a), .aux_id)[ , -ncol(a[["TRUE"]])]


