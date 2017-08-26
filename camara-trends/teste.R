library(xml2)

url <- "www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ListarProposicoes?sigla=PL&numero=&ano=2017&datApresentacaoIni=&datApresentacaoFim=&idTipoAutor=&parteNomeAutor=&siglaPartidoAutor=&siglaUFAutor=&generoAutor=&codEstado=&codOrgaoEstado=&emTramitacao="


df.api.antiga <- GET(url)

df.api.antiga %>% 
  content(type = "text/xml", encoding = "UTF-8") %>% 
  xmlParse() %>% 
  xmlToDataFrame() %>% 
  head()

# metodo xml2
x <- df.api.antiga %>% 
  content(type = "text/xml", encoding = "UTF-8") %>% 
  #read_xml() %>% 
  xml_find_all("proposicao")

x %>% xml_attrs()
x %>% xml_siblings()

x %>% 
  xml_find_all("tipoProposicao")