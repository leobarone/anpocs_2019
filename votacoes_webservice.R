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

# safely_read_xml <- function(url_camara){
#   url_camara <- 'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario?tipo="AA'
#   p <- safely(read_xml(url_camara))
# }
# 
# refaz_query = function(funcao, n_tentativas = 10, sleep = 0.1){
#   
#   resultado <- NULL
#   tentativa <- 1
#   funcao_possibly <- possibly(funcao, otherwise = NULL)
#   
#   while(is.null(resultado) && tentativa <= n_tentativas){
#     tentativa <- tentativa + 1
#     resultado <- funcao_possibly()
#     Sys.sleep(sleep)
#     sleep <- sleep + 0.1
#   }
#   
#   return(resultado)
# }

tipos_proposicao <- function(){
  
  'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarSiglasTipoProposicao' %>%
    read_xml() %>%
    xml_nodes(xpath = '//sigla') %>%
    xml_attrs() %>%
    reduce(bind_rows) %>% 
    mutate_all(str_trim) %>%
    rename(tipo = tipoSigla)
  
}

obter_partidos <- function(){
  
  xml_data <- 'https://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterPartidosCD' %>%
    read_xml()
    
    id_partido <- xml_data %>%
      xml_nodes(xpath = '//partido/idPartido') %>% 
      xml_text()
      
    partido <- xml_data %>%
      xml_nodes(xpath = '//partido/siglaPartido') %>% 
      xml_text()
    
    nome_partido <- xml_data %>%
      xml_nodes(xpath = '//partido/nomePartido') %>% 
      xml_text()
    
    data_criacao <- xml_data %>%
      xml_nodes(xpath = '//partido/dataCriacao') %>% 
      xml_text()
    
    data_extincao <- xml_data %>%
      xml_nodes(xpath = '//partido/dataExtincao') %>% 
      xml_text()
    
    tibble(id_partido,
           partido,
           nome_partido,
           data_criacao,
           data_extincao) %>%
      mutate_all(str_trim)
}

proposicao <- function(id_proposicao){
  
  xml_data <-  'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?' %>%
    GET(query = list(IdProp = id_proposicao)) %>%
    read_xml()
  
  info_attr <- xml_data %>%
    xml_nodes(xpath = '//proposicao') %>%
    xml_attrs() %>%
    unlist() %>%
    str_trim()
  
  info_text <- xml_data %>%
    xml_nodes(xpath = '//proposicao//*') %>%
    xml_text()
  
  info_text_names <- xml_data %>%
    xml_nodes(xpath = '//proposicao//*') %>%
    xml_name()
  
  info <- c(info_attr, info_text) %>%
    as.list()
  
  names(info) <- c('tipo', 'numero', 'ano', info_text_names)
  
  return(info)
  
}

proposicoes_votadas <- function(ano){
  
  xml_data <-  'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoesVotadasEmPlenario?' %>%
    GET(query = list(ano = ano,
                     tipo = '')) %>%
    read_xml() 
  
  id_proposicao <- xml_data %>%
    xml_nodes(xpath = '//codProposicao') %>%
    html_text()
  
  nome_proposicao <-  xml_data %>%
    xml_nodes(xpath = '//nomeProposicao') %>%
    html_text()
  
  data_votacao <-  xml_data %>%
    xml_nodes(xpath = '//dataVotacao') %>%
    html_text()
  
  tibble(id_proposicao,
         nome_proposicao,
         data_votacao)
  
}

votacao_proposicao <- function(id_proposicao){
  # id_proposicao <- proposicoes_2019$id_proposicao[1]
  prop <- proposicao(id_proposicao)
  
  xml_data <-  'https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?' %>%
    GET(query = list(tipo = prop$tipo,
                     numero = prop$numero,
                     ano = prop$ano)) %>%
    read_xml() 
  
  meta <- xml_data %>%
    xml_nodes(xpath = '//Votacao') %>%
    xml_attrs() %>%
    map(as.list)
  
  bancadas <- map(
    1:length(meta),
    function(x){
      xml_data %>%
        xml_nodes(xpath = paste0('//Votacao[', x, ']//bancada')) %>%
        xml_attrs() %>%
        reduce(bind_rows) %>%
        rename(sigla = Sigla) %>%
        mutate_all(str_trim)
      
    }
  )
  
  votos <- map(
    1:length(meta),
    function(x){
      xml_data %>%
        xml_nodes(xpath = paste0('//Votacao[', x, ']//Deputado')) %>%
        xml_attrs() %>%
        reduce(bind_rows) %>%
        rename(parlamentar = Nome,
               id_parlamentar = ideCadastro,
               partido = Partido,
               uf = UF,
               voto = Voto) %>%
        mutate_all(str_trim)
    }
  )
  
  l <- list(meta = meta,
       bancadas = bancadas,
       votos = votos) %>%
    transpose() %>%
      c(id_proposicao = id_proposicao,
        tipo = prop$tipo,
        numero = prop$numero,
        ano = prop$ano)
}


proposicoes_2019 <- proposicoes_votadas(2019)


lista_votos_2019 <- proposicoes_2019$id_proposicao %>% 
  map(votacao_proposicao)

# lista_votos_2019 <- NULL
# j = 1
# for (id in proposicoes_2019$id_proposicao){
#   print(j)
#   lista_votos_2019 <- c(lista_votos_2019,
#                            votacao_proposicao(id))
#   j = j + 1
# }

for (i in 1:length(lista_votos_2019)){
  lista_votos_2019[[i]]$id_votacao <- i
}

lista_votos_2019[[105]]
id_votacoes <- tibble(
  id_proposicao = lista_votos_2019 %>%
    map_chr("id_proposicao"),
  id_votacao = lista_votos_2019 %>%
    map_dbl("id_votacao"),
  tipo = lista_votos_2019 %>%
    map_chr("tipo"),
  numero = lista_votos_2019 %>%
    map_chr("numero"),
  ano = lista_votos_2019 %>%
    map_chr("ano"),
  objeto_votacao = lista_votos_2019 %>%
    map(~.x[[1]]$meta$ObjVotacao))

lista_votos_2019 %>%
  map(~.x[[1]]$votos) %>%
  map(nrow)

lista_votos_2019 %>%
  map(~.x[[1]]$bancadas) %>%
  map(nrow)

bancadas <- lista_votos_2019 %>%
  map(function(x) {
    data.frame(x[[1]]$bancadas, x$id_proposicao, x$id_votacao) %>%
      rename(id_votacao = x.id_votacao,
             id_proposicao = x.id_proposicao,
             bancada = sigla)
  }) %>%
  reduce(bind_rows)

votos <- lista_votos_2019 %>%
  map(function(x) {
    data.frame(x[[1]]$votos, x$id_proposicao, x$id_votacao) %>%
      rename(id_votacao = x.id_votacao,
             id_proposicao = x.id_proposicao)
  }) %>%
  reduce(bind_rows)

bancadas %>%
  group_by(bancada) %>% 
  summarise() %>% 
  rename(partido = bancada) %>% 
  anti_join(
    obter_partidos(),
    by = 'partido'
  ) %>% write_csv("bancadas_a_nomear.csv")

source("bancada_partido.R")

bancadas <- bancadas %>%
  full_join(bancada_partido, by = 'bancada')

bancadas %>%
  group_by(partido) %>% 
  summarise() %>% 
  anti_join(
    obter_partidos(),
    by = 'partido'
  )

votos <- votos %>%
  mutate(
    partido = replace(partido, partido == 'Avante', 'AVANTE'),
    partido = replace(partido, partido == 'Patriota', 'PATRIOTA'),
    partido = replace(partido, partido == 'Podemos', 'PODE'),
    partido = replace(partido, partido == 'Solidaried', 'SOLIDARIEDADE')
  ) %>% 
  left_join(bancadas,
            by = c('partido', 'id_proposicao', 'id_votacao'))

votos %>%
  group_by(id_parlamentar) %>%
  count %>%
  arrange(-n)

votos %>% 
  group_by(voto) %>% 
  count()

votos %>% 
  group_by(orientacao) %>% 
  count()

# save.image("votacoes_2019.RData")
rm(list=ls())
load('votacoes_2019.RData')
setwd(3697560)

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

posicoes_iniciais <- ids_parlamentar %>%
  mutate(x = NA_real_,
         init = 0.5) %>% 
  bind_rows(data.frame(x = c(0, 1),
                       i = ids_identificacao,
                       id_parlamentar = c(999998, 999999),
                       init = c(0.5, 0.5)))

d %>% 
  group_by(id_parlamentar) %>% 
  summarise(votacoes = n(),
            disciplina = sum(voto)) %>%
  left_join(parlamentares, by = 'id_parlamentar') %>% 
  arrange(-pct_disciplina) %>% 
  View()

d_stan <-
  within(list(), {
    
    # Dados das votacoes e parametros
    y <- d$voto                     # votos
    i_y <- d$i                      # id dos parlamentares
    j_y <- d$j                      # id das votacoes
    n <- max(d$i)                   # numero de parlamentares
    k <- max(d$j)                   # numero de votacoes
    l <- nrow(d)                    # numero total de votos
    n_theta <- nrow(ids_parlamentar)# numero de pontos livres
    id_theta <- ids_parlamentar$i   # id de pontos livres
    
    # Identificacao
    n_fixo <- 2                     # numero de pontos fixos
    id_fixo <- ids_identificacao    # id dos pontos fixos
    fixos <- c(0, 1)                # posicao dos pontos fixos
    
    # Prioris
    alfa_mean <- 0                  # priori para alfa - media
    alfa_sd <- 5                    # priori para alfa - desvpad
    beta_mean <- 0                  # priori para beta - media
    beta_sd <- 2.5                  # priori para beta - desvpad
    tau_scale <- 5                  # priori para alfa - scale
    theta0_mean <- 0                # priori para theta0 - media
    theta0_sd <- 10                 # priori para theta0 - desvpad
  })

d_init <- list(list(theta = runif(781)))

# modelo_base <- stan_model("modelo_base.stan")

modelo_base_fit <-
  sampling(modelo_base,
           data = d_stan,
           chains = 1, iter = 500,
           init = d_init,
           refresh = 100)

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
            pct_disciplina = votos_disciplinados/votos_totais)


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
  ggplot() +
  geom_density(aes(x = mean))

sumario %>% 
  ggplot() +
  geom_density(aes(x = pct_disciplina))

sumario %>% 
  group_by(partido) %>% 
  summarise(mean = mean(mean),
            q025= quantile(mean, p = 0.025),
            q975= quantile(mean, p = 0.975)) %>% 
  mutate(partido = fct_reorder(partido, mean)) %>% 
  ggplot(
    aes(x = partido, 
        y = mean,
        ymin = q025, 
        ymax = q975,
        color = partido)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = expression(theta[i]), x = "", colour = "Partido") +
  theme(legend.position = "bottom")

sumario %>% 
  group_by(partido) %>% 
  summarise(mean = mean(pct_disciplina),
            q025= quantile(pct_disciplina, p = 0.025),
            q975= quantile(pct_disciplina, p = 0.975)) %>% 
  mutate(partido = fct_reorder(partido, mean)) %>% 
  ggplot(
    aes(x = partido, 
        y = mean,
        ymin = q025, 
        ymax = q975,
        color = partido)) +
  geom_pointrange() +
  coord_flip() +
  labs(y = expression(theta[i]), x = "", colour = "Partido") +
  theme(legend.position = "bottom")

rm(list=ls())
load('teste_stan.RData')
