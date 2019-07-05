library(rvest)
library(tidyr)
library(dplyr)
library(purrr)
library(httr)
library(stringr)
library(readr)
library(rstan)
library(forcats)

rm(list=ls())
load('votacoes_2019.RData')
set.seed(3697560)

d <- votos %>% 
  filter(voto %in% c('Sim', 'Não', 'Obstrução', 'Abstenção','-'),
         orientacao %in% c('Sim', 'Não', 'Obstrução')) %>%
  mutate(voto = if_else(voto == orientacao, 1, 0),
         voto = replace(voto, voto %in% c('Abstenção', '-'), 1),
         id_votacao = as.numeric(id_proposicao) * 100 + id_votacao,
         id_parlamentar = as.numeric(id_parlamentar)) %>% 
  select(id_votacao, id_parlamentar, voto)


d %>%
  group_by(voto) %>% 
  count

ids_votacao <- d %>% 
  group_by(id_votacao) %>% 
  summarise() %>% 
  mutate(j = order(id_votacao))

ids_parlamentar <- d %>% 
  group_by(id_parlamentar) %>% 
  summarise() %>% 
  mutate(i = order(id_parlamentar))

d <- d %>% 
  left_join(ids_parlamentar, by = 'id_parlamentar') %>% 
  left_join(ids_votacao, by = 'id_votacao')


d_stan <-
  within(list(), {
    y <- d$voto                      # votos
    ii <- d$i                        # id dos parlamentares
    jj <- d$j                        # id das votacoes
    N <- nrow(d)                     # numero total de votos
    I <- max(d$i)                    # numero de parlamentares
    J <- max(d$j)                    # numero de votacoes
  })

modelo_2pl <- stan_model("2pl.stan")

modelo_base_fit <-
  sampling(modelo_2pl,
           data = d_stan,
           chains = 1,
           iter = 500,
           refresh = 100)

save.image("2pl.RData")
