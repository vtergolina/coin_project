# OBJETIVO: IMPORTAR LISTA COM DADOS DE CRIPTOMOEDAS (LISTA BINANCE0)
setwd("/home/bgrillob/criptoBot/")
library(rvest)
library(magrittr)
library(dplyr)
criptoMoedas <- read_html("https://coinmarketcap.com/exchanges/binance/")
listaMoedas <- criptoMoedas %>%
  html_nodes(xpath = '/html/body/div[4]/div/div[1]/div[4]/div/div[2]/table') %>%
  html_table() %>%
  .[[1]] %>%
  select(
    Currency,
    Pair
  ) %>%
  within(., {
    Pair <- Pair %>% 
      strsplit(., split = "/") %>%
      lapply(., function(x) {x[1]}) %>%
      unlist
  }) %>%
  group_by(
    Currency
  ) %>%
  summarise(Pair = unique(Pair))

write.table(listaMoedas, "MoedasBinance.csv", sep = ";")
