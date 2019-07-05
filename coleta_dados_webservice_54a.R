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

bancadas <- NULL
votos <- NULL

for (ano in 2011:2014){
  proposicoes_ano <- proposicoes_votadas(ano)
  
  lista_votos <- proposicoes_ano$id_proposicao %>% 
    map(votacao_proposicao)
  
  for (i in 1:length(lista_votos_ano)){
    lista_votos_ano[[i]]$id_votacao <- i
  }
  
  id_votacoes <- tibble(
    id_proposicao = lista_votos_ano %>%
      map_chr("id_proposicao"),
    id_votacao = lista_votos_ano %>%
      map_dbl("id_votacao"),
    tipo = lista_votos_ano %>%
      map_chr("tipo"),
    numero = lista_votos_ano %>%
      map_chr("numero"),
    ano = lista_votos_ano %>%
      map_chr("ano"),
    objeto_votacao = lista_votos_ano %>%
      map(~.x[[1]]$meta$ObjVotacao))

  bancadas_ano <- lista_votos_2019 %>%
    map(function(x) {
      data.frame(x[[1]]$bancadas, x$id_proposicao, x$id_votacao) %>%
        rename(id_votacao = x.id_votacao,
               id_proposicao = x.id_proposicao,
               bancada = sigla)
    }) %>%
    reduce(bind_rows)
  
  bancadas <- bancadas %>% 
    bind_rows(bancada_ano)
  
  votos_ano <- lista_votos_2019 %>%
    map(function(x) {
      data.frame(x[[1]]$votos, x$id_proposicao, x$id_votacao) %>%
        rename(id_votacao = x.id_votacao,
               id_proposicao = x.id_proposicao)
    }) %>%
    reduce(bind_rows)
  
  votos <- votos %>% 
    bind_rows(votos_ano)
  
}

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

votos <- votos %>%
  mutate(
    partido = replace(partido, partido == 'Avante', 'AVANTE'),
    partido = replace(partido, partido == 'Patriota', 'PATRIOTA'),
    partido = replace(partido, partido == 'Podemos', 'PODE'),
    partido = replace(partido, partido == 'Solidaried', 'SOLIDARIEDADE')
  ) %>% 
  left_join(bancadas,
            by = c('partido', 'id_proposicao', 'id_votacao'))

