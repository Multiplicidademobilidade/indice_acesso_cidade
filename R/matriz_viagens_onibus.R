
# Esse script contem uma funcao que faz a requisicao na Distance Matrix API e 
# retorna um arquivo .Rdata para cada cidade e ano

# A API Distance Matrix retorna:
# Distancia (m), Tempo total (s) e Status (OK ou ROUTE_NOT_FOUND)

# Falta:
# Definir os horarios de partida
# Numerar script de acordo com o processo de trabalho 


# SETUP
source('fun/setup.R')
library(gmapsdistance) # Distance Matrix API


mapeamento_distance_matrix <- function(ano, munis="all", resol="8"){
  api_key <-  set.api.key("API_KEY") # Precisa da API KEY para funcionar
  
  # Criar estrutura de pasta
  files_folder <- "../../indice-mobilidade_dados"
  load_folder <- sprintf("%s/09_hex_municipios/%s", files_folder, ano)
  save_folder <- sprintf("%s/10_matriz_viagens_onibus", files_folder)
  save_folderA <- sprintf("%s/%s", save_folder, ano)
  
  if ("10_matriz_viagens_onibus" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
    dir.create(save_folder)
  }
  if (ano %nin% list.dirs(save_folder, recursive = FALSE, full.names = FALSE)){
    dir.create(save_folderA)
  }

  fazer_requisicao <- function(sigla_munis){
    # Carregar grid de hexagonos
    file <- read_rds(sprintf("%s/hex_%s_%s_0%s.rds", load_folder, sigla_munis, ano, resol))

    # Centro dos hexagonos
    file <- file %>%
      mutate(center_hex = st_centroid(geometry))
    # Separando Lat e Long
    file <- file %>%
      mutate(lat = unlist(map(file$center_hex,2)),
             long = unlist(map(file$center_hex,1)))
  
    # Preparando string para o padrao da API
    file <- file %>%
      mutate(centro = str_c(lat,long,sep='+'))  
    
     # TESTEI A FUNCAO PARA UM DATAFRAME REDUZIDO
  
    file2 <- head(file, 5)
  
   # Aplicar transformacoes
    # O objetivo e criar um dataframe com todos os pares de origens e destinos
    # e assim fazer as requisicoes no modo "pairwise"
    file_aux <- file2 %>% expand(id_hex, centro)%>% 
      rename(destino = centro)
    file2 <- file2 %>% inner_join(file_aux, by= "id_hex")%>%
      dplyr::select(-(center_hex:long))%>%
      rename(origem = centro)%>%
      dplyr::filter(origem != destino) 
  
   # Fazer requisicoes
    
    origin <- file2$origem
    destination <- file2$destino
    
    matriz = as.data.frame(gmapsdistance(
      origin = origin,
      destination = destination,
      mode = "transit", # transporte publico
      shape = 'long', # or long # shape = long parece fazer mais sentido
      departure = 'now', # pico e fora-pico?
      combinations = 'pairwise',
      key = api_key))
    
  # 4.0 Salvar
   # write_rds(matriz, 
             # sprintf("~/repos/acesso_oport/hex_municipio/%s/hex_%s_%s_%s.rds", ano, sigla_muni, resol, ano), compress = 'gz')
    save(matriz, file=sprintf("%s/matriz_%s_0%s_%s.Rdata", save_folderA, sigla_munis, resol, ano))

  }
  
  # 5.0 Aplicar funcao
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  lapply(X = x, FUN = fazer_requisicao)

}


# Executar funcao
mapeamento_distance_matrix(ano=2019, munis="nat")


# ================================
# Abaixo estao codigos escritos durante a elaboracao deste script 
# eliminar numa proxima versao

load("../natal8_long.Rdata")
write_rds()

natal <- read_rds("../../indice-mobilidade_dados/09_hex_municipios/2019/hex_nat_2019_08.rds")

# 1.0 Carregamento das bases
natal <- load("../natal8_long.Rdata")
natal <- read_rds("../../indice-mobilidade_dados/09_hex_municipios/2019/hex_nat_2019_08.rds")

# 2.0 Cria????o da query

# Centro dos hexagonos
# natal8$center_hex <- lapply(natal8$id_hex, h3_to_point)

natal <- natal %>%
  mutate(center_hex = st_centroid(geometry))
# Separando Lat e Long
natal <- natal %>%
  mutate(lat = unlist(map(natal$center_hex,2)),
         long = unlist(map(natal$center_hex,1)))

# Preparando string para API
natal <- natal %>%
  mutate(centro = str_c(lat,long,sep='+'))

natal5 <- head(natal, 5)

#teste2 <- teste %>% expand(id_hex, origin, destination)

natal_aux <- natal5 %>% expand(id_hex, centro)%>%
  rename(destino = centro)

natal5 <- natal5 %>% inner_join(natal_aux, by= "id_hex")%>%
  dplyr::select(-(center_hex:long))%>%
  rename(origem = centro)%>%
  dplyr::filter(origem != destino)

results_long <- results_long%>%
  rename(origem = Time.or)

teste <- inner_join(natal5, results_long, by="origem")  

#teste5 <- dplyr::select(as.data.frame(teste4), -(center_hex:long))
#colnames(teste5)[6] <- "origem"

# reordenar
teste5 <- dplyr::select(teste5, id_hex, h3_resolution, sigla_muni, geometry, origem, destino)
# filtrar origem = destino
teste6 <- dplyr::filter(teste5, origem != destino)

# Testar pairwise 

# 3.0 Requisicao na API
api_key <-  set.api.key("API_KEY") # Precisa da API KEY para funcionar

origin <- natal5$origem
destination <- natal5$destino

# Filtrar origem = destino
natal

results_long2 = as.data.frame(gmapsdistance(
  origin = origin,
  destination = destination,
  mode = "transit", # transporte p??blico
  shape = 'long', # or long # shape = long parece fazer mais sentido
  departure = 'now', # pico e fora-pico?
  combinations = 'pairwise',
  key = api_key))

save(results_long2, file='natal5_pair.Rdata')
