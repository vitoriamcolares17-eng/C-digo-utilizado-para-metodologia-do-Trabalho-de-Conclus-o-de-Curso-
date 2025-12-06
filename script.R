# Carregar pacotes
library(terra)       # para raster
library(sf)          # para dados vetoriais
library(dplyr)       # manipulação de dados
library(ggplot2)     # Visualização
library(lubridate)   # datas e tempos
library(raster)

# Definir ano da análise
ano <- 2018

# Raster reprojetado
uso_solo <- rast(paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/mapbiomas_", ano, ".tif"))

# Reclassificação do raster MapBiomas (para o ano especifico) - Coleção 10 (2025)
reclass_map_values <- c(
  "1" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, "49" = 1,   # Florestas Naturais
  "10" = 10, "11" = 10, "12" = 10, "32" = 10, "29" = 10, "50" = 10,   # Vegetação Campestre / Arbustiva
  "14" = 18, "15" = 18, "18" = 18, "19" = 18, "39" = 18, "20" = 18,   # Agricultura e Pastagem
  "40" = 18, "62" = 18, "41" = 18, "36" = 18, "46" = 18, "47" = 18,
  "35" = 18, "48" = 18,   # Agricultura 
  "9" = 9,   # Silvicultura
  "21" = 21,   # Mosaico de Usos
  "22" = 22, "23" = 22, "24" = 22, "25" = 22, "30" = 22, "75" = 22,   # Área Urbana
  "26" = 26, "33" = 26, "31" = 26,   # Água
  "27" = 27   # Não Observado
)

reclass_matrix <- matrix(
  c(as.numeric(names(reclass_map_values)), as.numeric(reclass_map_values)),
  ncol = 2, byrow = FALSE
)

# Esse será o raster final utilizado, nessa etapa ele está recebendo a matriz de reclassificação
uso_solo_reclass <- classify(uso_solo, rcl = reclass_matrix)

# Extração da área de cada classe
s_area <- cellSize(uso_solo_reclass, unit="ha")
areas_classes <- as.data.frame(zonal(s_area, uso_solo_reclass, fun="sum", na.rm=TRUE))
names(areas_classes) <- c("classe", "hectare")

# Salvando raster reclassificado na pasta
writeRaster(uso_solo_reclass, "C:/Users/colar/Documents/vcolares_tcc_proj/input/2023/2023_classes.tif", overwrite = TRUE)

# Importando os Focos de calor 
focos <- st_read(paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/focos.shp"))

# Converter a coluna de data/hora
focos[["DataHora"]] <- as.POSIXct(focos[["DataHora"]], format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

# Criar colunas de ano e mês
focos$ano <- format(focos$DataHora, "%Y")
focos$mes <- format(focos$DataHora, "%m")
focos_mensal <- focos %>%       ## Soma de focos por mês
  group_by(mes) %>%
  summarise(total_focos = n())

# Criar coluna de estação do ano
focos <- focos %>%
  mutate(estacao = case_when(
    mes %in% c("Dec", "Jan", "Feb") ~ "Verão",
    mes %in% c("Mar", "Apr", "May") ~ "Outono",
    mes %in% c("Jun", "Jul", "Aug") ~ "Inverno",
    mes %in% c("Sep", "Oct", "Nov") ~ "Primavera" ))
focos_estacao <- focos %>%       ## Soma de focos por estação
  group_by(estacao) %>%
  summarise(total_focos = n())

# Garantir mesmo CRS
focos <- st_transform(focos, crs(uso_solo_reclass))

# Identificar em qual classe cada foco caiu
focos$classe <- terra::extract(uso_solo_reclass, focos)[,2]

# Contar focos por classe
focos_por_classe <- focos %>%
  st_drop_geometry() %>%
  group_by(classe) %>%
  summarise(total_focos = n(), .groups="drop")

#Juntar dados e calcular densidade
dados_finais <- left_join(focos_por_classe, areas_classes, by="classe") %>%
  mutate(
    densidade_focos = (total_focos / hectare) * 100000  # focos por 100 hectares
  ) %>%
  arrange(desc(round(densidade_focos,2)))

print(dados_finais)

# Salvar saída automática
write.csv(dados_finais,
          paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/resumo_", ano, "classe.csv"),
          row.names = FALSE)

write.csv2(dados_finais, "C:/Users/colar/Documents/vcolares_tcc_proj/input/2023/resumoclassesnovo.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

##################################### ANALISE POR CADA COBERTURA ######################################### 

# Área por cobertura
s_area <- cellSize(uso_solo, unit="ha")
areas_cobertura <- as.data.frame(zonal(s_area, uso_solo, fun="sum", na.rm=TRUE))
names(areas_cobertura) <- c("cobertura", "hectare")

# Focos de calor 
focos <- st_read(paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/focos.shp"))
names(focos)[names(focos) == "SAMPLE_1"] <- "cobertura"

totalfocos <- focos %>%
  st_drop_geometry() %>%
  group_by(cobertura) %>%
  summarise(total_focos = n(), .groups="drop")
names(totalfocos)[names(totalfocos) == "classe"] <- "cobertura"

# Garantir mesmo CRS
focos <- st_transform(focos, crs(uso_solo))

#Juntar dados e calcular densidade
dados_finais <- left_join(totalfocos, areas_cobertura, by="cobertura") %>%
  mutate(
    densidade_focos = (total_focos / hectare) * 100000   # focos por 100 hectares
  ) %>%
  arrange(desc(round(densidade_focos,2)))

write.csv2(dados_finais, "C:/Users/colar/Documents/vcolares_tcc_proj/input/2023/resumocompletonovo.csv",
           row.names = FALSE, fileEncoding = "UTF-8")


# Salvar saída automática
write.csv(dados_finais,
          paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/resumo_", ano, "completo.csv"),
          row.names = FALSE)

##################################### ANALISE PELAS MICRORREGÕES DO RS ######################################### 
# ---------------------------------------
# PACOTES
# ---------------------------------------
library(sf)
library(terra)
library(dplyr)

ano <- 2023

uso_solo <- rast(
  paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/mapbiomas_", ano, ".tif")
)

reclass_map_values <- c(
  "1"=1,"3"=1,"4"=1,"5"=1,"6"=1,"49"=1,
  "10"=10,"11"=10,"12"=10,"32"=10,"29"=10,"50"=10,
  "14"=18,"15"=18,"18"=18,"19"=18,"39"=18,"20"=18,
  "40"=18,"62"=18,"41"=18,"36"=18,"46"=18,"47"=18,
  "35"=18,"48"=18,
  "9"=9,
  "21"=21,
  "22"=22,"23"=22,"24"=22,"25"=22,"30"=22,"75"=22,
  "26"=26,"33"=26,"31"=26,
  "27"=27
)

reclass_matrix <- matrix(
  c(as.numeric(names(reclass_map_values)), as.numeric(reclass_map_values)),
  ncol = 2
)

uso_solo_reclass <- classify(uso_solo, reclass_matrix)

focos <- st_read(
  paste0("C:/Users/colar/Documents/vcolares_tcc_proj/input/", ano, "/focos.shp")
)

focos <- st_transform(focos, crs(uso_solo_reclass))

focos$classe <- terra::extract(uso_solo_reclass, focos)[,2]

micro <- st_read(
  "C:/Users/colar/Documents/vcolares_tcc_proj/input/microrregioes/microrregioes_.shp"
)

micro <- st_transform(micro, st_crs(focos))

focos_micro <- st_join(focos, micro, left = FALSE)

focos_micro_classe <- focos_micro %>%
  st_drop_geometry() %>%
  group_by(nomemicro, classe) %>%
  summarise(total_focos = n(), .groups = "drop")

classe_predominante <- focos_micro_classe %>%
  group_by(nomemicro) %>%
  slice_max(total_focos, n = 1, with_ties = FALSE) %>%
  ungroup()

write.csv2(
  classe_predominante,
  paste0(
    "C:/Users/colar/Documents/vcolares_tcc_proj/output/classe_dominante_micro_",
    ano,
    ".csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ano de interesse
ano_alvo <- 2018
classe_silvicultura <- 9   # ajuste se o código for outro

# focos de 2018
focos_2018 <- focos_sf %>%
  filter(ano == ano_alvo)

# extrair classe do solo para os focos
focos_2018$classe_solo <- terra::extract(
  uso_solo_reclass,
  vect(focos_2018)
)[,2]

# manter apenas focos em silvicultura
focos_silvicultura <- focos_2018 %>%
  filter(classe_solo == classe_silvicultura)

# juntar com microrregiões
focos_micro <- st_join(
  focos_silvicultura,
  micro,
  join = st_within
)

# contagem final por microrregião
focos_silvicultura_micro_2018 <- focos_micro %>%
  st_drop_geometry() %>%
  group_by(nomemicro) %>%      # ajuste para o nome real do campo
  summarise(total_focos = n()) %>%
  arrange(desc(total_focos))

write.csv2(
  focos_silvicultura_micro_2018,
  paste0(
    "C:/Users/colar/Documents/vcolares_tcc_proj/output/focos_silvicultura_micro_2018_",
    ano,
    ".csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
