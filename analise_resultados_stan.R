library(rvest)
library(tidyr)
library(dplyr)
library(purrr)
library(httr)
library(stringr)
library(readr)
library(rstan)
library(forcats)

load('teste_stan.RData')

## Resultados
d_model <- as.data.frame(modelo_base_fit)

theta_partido <- function(party){

  ids <- parlamentares %>% 
    filter(partido == party) %>% 
    pull(i)
  
  theta_p <- d_model %>% 
    select(ids) %>% 
    mutate(soma = rowSums(select_all(.))/length(.)) %>% 
    pull(soma)
  
  data.frame(party,
             theta_p)
}
theta_partido_d %>% 
  filter(partido == "PT") %>% 
  head()
theta_partido_d %>% 
  filter(partido == "PSL") %>% 
  head()


theta_partido_d <- 
  parlamentares %>% 
  group_by(partido) %>% 
  summarise() %>% 
  pull(partido) %>% 
  map(theta_partido) %>% 
  reduce(bind_rows)

theta_partido_d %>% head()

theta_partido_d %>% 
  group_by(partido) %>% 
  summarise(media = mean(theta_p),
            q025 = quantile(theta_p, 0.025),
            q975 = quantile(theta_p, 0.975)) %>%
  ggplot() +
  geom_pointrange(aes(mean, q025, q975, color = partido))



  geom_density()



resultado <- as_tibble(
  summary(modelo_base_fit,
          par = "theta"))


parlamentares <- votos %>% 
  filter(voto %in% c('Sim', 'Não', 'Obstrução', 'Abstenção','-'),
         orientacao %in% c('Sim', 'Não', 'Obstrução')) %>% 
  mutate(voto = if_else(voto == orientacao, 1, 0),
         voto = replace(voto, voto %in% c('Abstenção', '-'), 1)) %>% 
  mutate(id_parlamentar = as.numeric(id_parlamentar)) %>% 
  group_by(id_parlamentar, parlamentar) %>% 
  summarise(partido = first(partido),
            votos_totais = n(),
            votos_disciplinados = sum(voto),
            pct_disciplina = votos_disciplinados/votos_totais) %>% 
  left_join(ids_parlamentar, by = 'id_parlamentar') %>% 
  ungroup

partidos <- votos %>% 
  filter(voto %in% c('Sim', 'Não', 'Obstrução', 'Abstenção','-'),
         orientacao %in% c('Sim', 'Não', 'Obstrução')) %>% 
  mutate(voto = if_else(voto == orientacao, 1, 0),
         voto = replace(voto, voto %in% c('Abstenção', '-'), 1)) %>% 
  mutate(id_parlamentar = as.numeric(id_parlamentar)) %>% 
  group_by(partido) %>% 
  summarise(votos_totais = n(),
            votos_disciplinados = sum(voto),
            pct_disciplina = votos_disciplinados/votos_totais) %>% 
  arrange(-pct_disciplina)

sumario <-
  bind_cols(ids_parlamentar,
            as_tibble(
              summary(modelo_base_fit,
                      par = "theta")$summary)
  ) %>% 
  left_join(parlamentares, by = 'id_parlamentar') %>% 
  mutate(parlamentar = fct_reorder(parlamentar, mean))

sumario %>% 
  select(pct_disciplina, mean, parlamentar, 
         votos_disciplinados, votos_totais) %>% 
  arrange(-pct_disciplina)

sumario %>% 
  select(pct_disciplina, mean, parlamentar, partido,
         votos_disciplinados, votos_totais) %>% 
  arrange(pct_disciplina)

sumario %>% 
  filter(id_parlamentar == 999998)

head(sumario)
sumario %>% 
  ggplot() +
  geom_density(aes(x = mean))

sumario %>% 
  ggplot() +
  geom_density(aes(x = pct_disciplina))

sumario %>% 
  ggplot() +
  geom_point(aes(x = pct_disciplina, y = mean))

sumario %>% 
  ggplot(
    aes(x = parlamentar, 
        y = mean,
        ymin = `97.5%`, 
        ymax = `2.5%`)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = expression(theta[i]), x = "") +
  theme(legend.position = "bottom") 

sumario %>%
  group_by(partido) %>% 
  mutate(n_partido = n()) %>% 
  ungroup() %>% 
  filter(n_partido > 50) %>% 
  ggplot(
    aes(x = parlamentar, 
        y = mean,
        ymin = `97.5%`, 
        ymax = `2.5%`,
        color = partido)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = expression(theta[i]), x = "") +
  theme(legend.position = "bottom") +
  facet_wrap(~partido, 
             scales = "free",
             ncol = 2)

sumario %>%
  group_by(partido) %>% 
  summarise(mean = mean(mean)) %>% 
  mutate(partido = fct_reorder(partido, mean)) %>% 
  ggplot(
    aes(x = partido,
        y = mean)) +
  geom_point() +
  coord_flip() +
  labs(y = expression(theta[i]), x = "") 

