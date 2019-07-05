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

ids_identificacao <- c(max(d$i) + 1, max(d$i) + 2)

indisciplinado <- ids_votacao %>% 
  mutate(voto = 0,
         i = ids_identificacao[1],
         id_parlamentar = 999998)

disciplinado <- ids_votacao %>% 
  mutate(voto = 1,
         i = ids_identificacao[2],
         id_parlamentar = 999999)

d <- d %>% 
  bind_rows(indisciplinado,
            disciplinado)

d_stan <-
  within(list(), {
    
    # Dados das votacoes e parametros
    voto <- d$voto                  # votos
    id_parlamentar <- d$i           # id dos parlamentares
    id_votacao <- d$j               # id das votacoes
    n <- max(d$i)                   # numero de parlamentares
    k <- max(d$j)                   # numero de votacoes
    m <- nrow(d)                    # numero total de votos
    n_theta <- nrow(ids_parlamentar)# numero de pontos livres
    id_theta <- ids_parlamentar$i   # id de pontos livres
    
    # Identificacao
    n_fixos <- 2                     # numero de pontos fixos
    id_fixos <- ids_identificacao    # id dos pontos fixos
    fixos <- c(0, 1)                # posicao dos pontos fixos
    
    # Prioris
    alfa_mean <- 0                  # priori para alfa - media
    alfa_sd <- 5                    # priori para alfa - desvpad
    beta_mean <- 0                  # priori para beta - media
    beta_sd <- 2.5                  # priori para beta - desvpad
    tau_scale <- 5                  # priori para alfa - scale
    theta0_mean <- 0.5              # priori para theta0 - media
    theta0_sd <- 5                  # priori para theta0 - desvpad
  })

posicoes_iniciais <- d %>% 
  group_by(id_parlamentar) %>% 
  summarise(votos_totais = n(),
            votos_disciplinados = sum(voto),
            pct_disciplina = votos_disciplinados / votos_totais) 

# d_init <- list(
#   list(
#     theta = posicoes_iniciais$pct_disciplina[1:(nrow(posicoes_iniciais) - 2)]))

modelo_base <- stan_model("modelo_base.stan")

modelo_base_fit <-
  sampling(modelo_base,
           data = d_stan,
           chains = 1,
           iter = 500,
           refresh = 100)

save.image("teste_stan.RData")

# legislators_init_1 <- list(
#   list(
#     theta = posicoes_iniciais$pct_disciplina[1:(nrow(posicoes_iniciais) - 2)]))
# 
# legislators_data_1 <-
#   within(list(), {
#     y <- d$voto
#     y_idx_leg <- d$i
#     y_idx_vote <- d$j
#     Y_obs <- length(y)
#     N <- max(d$i)
#     K <- max(d$j)
#     # priors
#     alpha_loc <- 0
#     alpha_scale <- 5
#     beta_loc <- 0
#     beta_scale <- 2.5
#     N_xi_obs <- 2
#     idx_xi_obs <- ids_identificacao
#     xi_obs <- c(0,1)
#     N_xi_param <- nrow(ids_parlamentar)
#     idx_xi_param <- ids_parlamentar$i
#     tau_scale <- 5
#     zeta_loc <- 0
#     zeta_scale <- 10
#   })
# 
# mod_ideal_point_1 <- stan_model("ideal_point_1.stan")
# 
# legislators_fit_1 <-
#   sampling(mod_ideal_point_1, data = legislators_data_1,
#            chains = 1, iter = 500,
#            init = legislators_init_1,
#            refresh = 100,
#            pars = c("alpha", "beta", "xi"))


d_stan <-
  within(list(), {
    y <- d$voto                      # votos
    ii <- d$i                        # id dos parlamentares
    jj <- d$j                        # id das votacoes
    N <- nrow(d)                     # numero total de votos
    I <- max(d$i)                    # numero de parlamentares
    J <- max(d$j)                    # numero de votacoes
  })
