# bigquery

#Your project ID will be mybigquery-177800 

library(bigrquery)
library(tidyverse)
library(stringr)

project <- "mybigquery-177800" # put your project ID here
sql <- "SELECT year, month, day, weight_pounds FROM [publicdata:samples.natality] LIMIT 5"
query_exec(sql, project = project)

con <- dbConnect(bigquery(), project = project, dataset = "CAMARA_TRENDS")

# carregar tabela para o banco
#dbWriteTable(con, name = "CAMARA_TRENDS")

insert_dataset(project, "CAMARA_TRENDS")
data(iris)

copy_table(src = iris, dest = "TB_IRIS", project = project)

iris2 <- iris
colnames(iris2) <- c("v1", "v2", "v3", "v4", "v5")

job <- insert_upload_job(project, dataset = "CAMARA_TRENDS", table = "TB_IRIS", values = iris2)


### subir tabela para banco
x1 <- readRDS("data/bases/base.Rds")
job <- insert_upload_job(project, dataset = "CAMARA_TRENDS", table = "TB_PLS", values = x1)
wait_for(job)
