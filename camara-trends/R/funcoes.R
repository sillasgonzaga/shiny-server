### funcoes de ajuda para o pacote


extrair_proposicao <- function(id.prop) {
  # montar url de extracao
  id <- id.prop
  url <- "http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp={id}"
  url <- glue(url)
  
  # checar se API está funcionando
  resp <- GET(url)
  stopifnot(resp$status_code == 200)
  
  # ler codigo fonte da resposta
  xml.resp <- url %>% 
    curl::curl() %>% 
    read_xml()
  
  lst <- xml.resp %>% as_list() %>% unlist()
  # criar data frame vazio para popular com valores
  mat <- matrix(NA, ncol = length(lst), nrow = 1) %>% as.data.frame()
  # popular linha
  mat[1,] <- unname(lst)
  # popular nomes das colunas
  names(mat) <- names(lst)
  
  ## data cleaning: formatar colunas
  df <- mat
  
  limpar_colunas <- function(x) {
    x <- str_trim(x)
    x <- str_to_lower(x)
    x <- abjutils::rm_accent(x)
    x <- ifelse(x == "", NA, x)
  }
  
  # limpar todas as colunas exceto a de ementa (setima coluna)
  col.ementa <- which(str_to_lower(names(df)) == "ementa")
  cols <- 1:ncol(df)
  cols <- cols[-col.ementa]
  
  df %<>% modify_at(cols, limpar_colunas)
  df %<>% as.data.frame()
  #browser()
  # limpar nomes de colunas
  #df <- janitor::clean_names(df)
  df
}
