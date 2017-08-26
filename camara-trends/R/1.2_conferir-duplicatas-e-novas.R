# importar base historica e periodica

hist <- readRDS("data/IDs/historico.Rds")
period <- readRDS("data/IDs/periodico.Rds")

# conferir novas
novas <- period[!period %in% hist]

# salvar novas IDs para baixar dados no script 2.0
saveRDS(novas, "data/IDs/novas.Rds")