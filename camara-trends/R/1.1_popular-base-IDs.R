### 01: Montar base historica

library(tidyverse)
library(xml2)
library(feather)
library(httr)
library(lubridate)
library(glue)

# da url, so preciso de dois campos: ID, nome e datApresentacao
api.sigla <- "PL"
api.ano <- "2017"

intervalo.dias <- 35
data <- today() - intervalo.dias
api.data.inicial <- format.Date(data, "%d/%m/%Y")
api.data.final <- ""

url <- "www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoes?sigla={api.sigla}&numero=&ano={api.ano}&datApresentacaoIni={api.data.inicial}&datApresentacaoFim={api.data.final}&idTipoAutor=&parteNomeAutor=&siglaPartidoAutor=&siglaUFAutor=&generoAutor=&codEstado=&codOrgaoEstado=&emTramitacao="
url <- glue(url)

# checar se API está funcionando
resp <- GET(url)
stopifnot(resp$status_code == 200)

# ler codigo fonte da resposta
xml.resp <- url %>% 
  curl::curl() %>% 
  read_xml()

# transformar em lista
lst <- xml.resp %>%
  as_list()

## Se um erro ocorreu (existe o componente descricao), pare o script
if (!is.null(lst$descricao)) {
  msg <- "Não há proposicões no intervalo de busca"
  #message(msg)
  stop(msg)
}

# obter id
proposicao.id <- lst %>% map("id") %>% unlist() %>% unname()

# salvar dados
saveRDS(proposicao.id, "data/IDs/periodico.Rds")