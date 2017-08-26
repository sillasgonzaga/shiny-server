rm(list = ls())

## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(abjutils)
library(lubridate)
library(data.table)
library(stringr)
library(janitor)
library(DT)
library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(visNetwork)
# configurar apenas inteiros nos breaks do ggplot2
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 
# definir cor
azul1 <- "#014d64"
azul2 <- "#01a2d9"

layout.by.attr <- function(graph, wc = 1, cluster.strength=1,layout=layout.auto) {  
  # source:
  # https://stackoverflow.com/questions/38999656/increasing-spaces-between-vertices-for-r-igraph
  
  g <- graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight <- 1
  
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

header <- dashboardHeader(
  title = "Câmara Trends",
  dropdownMenuOutput("messageMenu")
)

sidebar <- dashboardSidebar(
  textInput("keyword", "Digite um tema para a pesquisa",
            value = "Educação",
            placeholder = "Ex.: Educação, Saúde, Segurança, Emprego, etc."
            ),
  actionButton("goButton", "Pesquisar"),
  # tab com dashboard e graficos
  tabItems(
    
  )
)

body <- dashboardBody(
  tabsetPanel(
    tabPanel(title = "Dashboard",
      fluidRow(
        box(
          plotOutput("box1"), title = "PLs sobre o tema ao longo do tempo",
          width = 12, collapsible = TRUE#, height = 250
          )
        ),
      # linha de baixo: 3 graficos
      fluidRow(
        box(
          plotOutput("box2"), title = "Partido com mais PLs sobre o tema", width = 4#, height = 250
          ),
        box(
          plotOutput("box3"), title = "Nuvem de palavras", width = 4#, height = 250
          ),
        box(
          visNetworkOutput("box4"), title = "Rede de palavras", width = 4#, height = 250
          )
      )),
    tabPanel(title = "Tabela",
      fluidRow(
        DT::dataTableOutput("tabela")
      ))
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  
  ### ler arquivo
  df <- reactiveFileReader(1000, session, "data/bases/base.Rds", readRDS)
  df <- isolate(df())
  df %<>% clean_names()
  df$dataapresentacao %<>% dmy %>% as.Date()
  
  # limpar string de input e filtrar data frame
  pesquisa <- reactive({
    input$keyword %>% str_trim() %>% str_to_lower() %>% rm_accent()
  })
  
  df.pesquisa <- eventReactive(input$goButton, {

    tem_palavra_chave <- function(string, x, y) {
      str_detect(x, string) | str_detect(y, string)
    }
    
    x <- df %>% 
      mutate(achou_keyword = tem_palavra_chave(pesquisa(), tema, indexacao))
    x
  })
  
  ### view 1: fluxo do termo ao longo do tempo
  output$box1 <- renderPlot({
    df.pesquisa() %>% 
      group_by(data = dataapresentacao) %>% 
      summarise(tema = sum(achou_keyword, na.rm = TRUE)) %>% 
      mutate(indice.pop = tema / max(tema)) %>% 
      ggplot(aes_string(x = "data", y = "tema")) + 
        geom_line(color = azul1) + 
        #geom_smooth(method = "loess", se = FALSE) +
        labs(x = "Data", y = "Quantidade de Pls") + 
        theme_minimal() + 
        theme(panel.border = element_blank()) + 
        scale_y_continuous(breaks = int_breaks)
  })
  
  ### view 2: partidos que mais apresentaram sobre o tema
  output$box2 <- renderPlot({
    df.pesquisa() %>% 
      filter(achou_keyword & !is.na(partidoautor)) %>% 
      count(partidoautor) %>% 
      mutate(partidoautor = str_to_upper(partidoautor)) %>% 
      ggplot(aes(x = reorder(partidoautor, n), y = n)) + 
        geom_col(fill = azul1) +
        labs(x = NULL, y = "Quantidade de PLs") +
        theme_minimal() + 
        theme(panel.border = element_blank()) + 
        scale_y_continuous(breaks = int_breaks) +
        coord_flip()
    
  })
  
  ### view 3: nuvem de palavras
  output$box3 <- renderPlot({
    df.pesquisa() %>% 
      filter(achou_keyword) %>% 
      pull(indexacao) %>% 
      VectorSource() %>% 
      Corpus() %>% 
      tm_map(removeWords, c(pesquisa(), stopwords("pt"))) %>% 
      wordcloud(random.order = FALSE)
    
  })
  
  
  ### view 4: rede de palavras
  output$box4 <- renderVisNetwork({
    # filtrar linhas que contem palavra chave e que so possuem duas keywords diferentes
    ind <- which(df.pesquisa()$achou_keyword & str_count(df.pesquisa()$tema, ";") == 1)
    vetor.sna <- df.pesquisa()$tema[ind]
    vetor.sna <- strsplit(vetor.sna, "; ")
    
    # definir busca exata
    ind <- which(str_detect(df.pesquisa()$tema, pesquisa()) & str_count(df.pesquisa()$tema, ";") == 0)
    busca.exata <- unique(df.pesquisa()$tema[ind])
    
    
    res <-lapply(vetor.sna, combn, 2, simplify = FALSE) #create cominations
    out <-matrix(unlist(res), ncol=2, byrow=TRUE) #create dataframe of combinations
    
    # adicionar quebra de linha
    wrap_strings <- function(vector_of_strings,width){
      as.character(sapply(vector_of_strings, FUN=function(x){
        paste(strwrap(x, width=width), collapse="\n")
      }))
    }
    
    out[,1] %<>% wrap_strings(10)
    out[,1] %<>% wrap_strings(10)
    
    
    
    
    g <- graph_from_edgelist(out, directed = FALSE)
    V(g)$color <- azul1
  
    
    visIgraph(g, randomSeed = 123, physics = FALSE, layout = "layout_in_circle") %>% 
      #visInteraction(hover = TRUE, navigationButtons = TRUE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE)) %>% 
      visNodes(font = list(size = 20)) %>% 
      visEdges(width = 0.01)
    
    
  })
  
  ### view 5: tabela com PLs
  output$tabela <- DT::renderDataTable({
    df.pesquisa() %>% 
      filter(achou_keyword) %>% 
      select(dataapresentacao, nomeproposicao, autor, ufautor, partidoautor,
             ementa, situacao, link = linkinteiroteor) %>% 
      # corrigir formatacao de colunas
      mutate(
        nomeproposicao = str_to_upper(nomeproposicao),
        autor = str_to_title(autor),
        ufautor = str_to_upper(ufautor),
        partidoautor = str_to_upper(partidoautor),
       # ementa = str_to_title(ementa),
        situacao = str_to_title(situacao)
      ) %>%
      # criar link clicavel
      mutate(
        link = paste0("<a href='", link,"' target='_blank'>", link, "</a>")
      ) %>% 
      set_names(c("Data de Apresentação", "Nome", "Autor", "UF do autor",
                  "Partido", "Ementa", "Situação", "Link"))
  }, escape = FALSE)
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    atualizacao <- file.info("data/bases/base.Rds")$mtime
    atualizacao <- format(atualizacao, "%d/%m/%Y %H:%M")
    msg <- messageItem(from = "Última atualização", message = atualizacao)
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", msg)
  })
  
}

shinyApp(ui, server)