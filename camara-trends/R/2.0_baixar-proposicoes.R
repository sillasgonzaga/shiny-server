library(tidyverse)
library(xml2)
library(feather)
library(httr)
library(lubridate)
library(glue)
library(stringr)
library(magrittr)
library(abjutils)
library(janitor)
source("R/funcoes.R")

ids <- readRDS("data/IDs/novas.Rds")

###### apagar

ids <- c(readRDS("data/IDs/historico.Rds"),
         readRDS("data/IDs/novas.Rds"),
         readRDS("data/IDs/periodico.Rds")
)

ids <- unique(ids)

# iterar em todas as proposicoes novas
system.time(df.prop <- ids %>% map_df(extrair_proposicao))

df.prop <- clean_names(df.prop)

# salvar dados
saveRDS(df.prop, "data/bases/base.Rds") # leva 10 min pra baixar 1592 PLs
