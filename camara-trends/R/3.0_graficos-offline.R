library(tidyverse)
library(magrittr)
library(lubridate)
library(data.table)
library(stringr)
library(janitor)
library(DT)
library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(ggthemes)
suppressMessages(library(visNetwork))
# configurar apenas inteiros nos breaks do ggplot2
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 
# definir cor
azul1 <- "#014d64"
azul2 <- "#01a2d9"


df <- readRDS("data/bases/base.Rds")
df %<>% clean_names()
df$dataapresentacao %<>% dmy %>% as.Date()



tem_palavra_chave <- function(tema, x, y) {
  str_detect(x, tema) | str_detect(y, tema)
}

busca <- "emprego"

df.busca <- df %>% 
  mutate(res_tema = tem_palavra_chave(busca, tema, indexacao))

### view 1: fluxo do termo ao longo do tempo
df.busca %>% 
  group_by(data = dataapresentacao) %>% 
  summarise(tema = sum(res_tema, na.rm = TRUE)) %>% 
  mutate(indice.pop = tema / max(tema)) %>% 
  ggplot(aes_string(x = "data", y = "tema")) + 
    geom_line(color = azul1) + 
    #geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Data", y = "Quantidade de Pls") + 
    theme_minimal() + 
    theme(panel.border = element_blank()) + 
    scale_y_continuous(breaks = int_breaks)

### view 2: partidos que mais apresentaram sobre o tema
df.busca %>% 
  filter(res_tema & !is.na(partidoautor)) %>% 
  count(partidoautor) %>% 
  mutate(partidoautor = str_to_upper(partidoautor)) %>% 
  ggplot(aes(x = reorder(partidoautor, -n), y = n)) + 
    geom_col(fill = azul1) +
    labs(x = NULL, y = "Quantidade de PLs") +
    theme_minimal() + 
    theme(panel.border = element_blank()) + 
    scale_y_continuous(breaks = int_breaks)
  

### view 3: nuvem de palavras
df.busca %>% 
  filter(res_tema) %>% 
  pull(indexacao) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  tm_map(removeWords, c(busca, stopwords("pt"))) %>% 
  wordcloud(random.order = FALSE)


### view 4: rede de palavras
ind <- which(df.busca$res_tema & str_count(df.busca$tema, ";") == 1)
vetor.sna <- df.busca$tema[ind]

# definir busca exata
ind <- which(str_detect(df.busca$tema, busca) & str_count(df.busca$tema, ";") == 0)
busca.exata <- unique(df.busca$tema[ind])


vetor.sna <- strsplit(vetor.sna, "; ")

res <-lapply(vetor.sna, combn, 2, simplify = FALSE) #create cominations
out <-matrix(unlist(res), ncol=2, byrow=TRUE) #create dataframe of combinations

g <- graph_from_edgelist(out, directed = FALSE)
# adicionar grupos
V(g)$color <- azul1

wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

layout.by.attr <- function(graph, wc = 1, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

out[,1] %<>% wrap_strings(12)
out[,2] %<>% wrap_strings(12)
g <- graph_from_edgelist(out, directed = FALSE)

visIgraph(g, randomSeed = 123, physics = FALSE, layout = "layout.by.attr") %>% 
  #visInteraction(hover = TRUE, navigationButtons = TRUE) %>% 
  visOptions(highlightNearest = list(enabled = TRUE)) %>% 
  visNodes(font = list(size = 20)) %>% 
  visEdges(width = 0.01)
# http://www.r-graph-gallery.com/86-network-representation-with-igraph
# http://www.r-graph-gallery.com/portfolio/network/



### view 6: ultimas proposicoes aprovadas
df.busca %>% 
  filter(res_tema) %>% 
  select(dataapresentacao, nomeproposicao, autor, ufautor, partidoautor,
         ementa, situacao, linkinteiroteor) %>% 
  # corrigir formatacao de colunas
  mutate(
    nomeproposicao = str_to_upper(nomeproposicao),
    autor = str_to_title(autor),
    ufautor = str_to_upper(ufautor),
    partidoautor = str_to_upper(partidoautor),
    ementa = str_to_title(ementa),
    situacao = str_to_title(situacao)
  ) %>% 
  datatable()


