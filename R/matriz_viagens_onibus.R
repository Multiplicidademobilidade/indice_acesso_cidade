
# Esse script contem duas funções:

# A primeira pega a matriz de hexagonos, prepara um dataframe contendo todas as 
# origens, destinos e filtra de acordo com a distancia, populacao e oportunidades
# Os resultados sao salvos num dataframe auxiliar

# A segunda carrega o dataframe auxiliar, transforma o latlong dos centroides em
# uma string para a API e faz as requisicoes, salvando o arquivo ao final

# A API Distance Matrix retorna:
# Distancia (m), Tempo total (s) e Status (OK ou ROUTE_NOT_FOUND)

#### 0) Setup ------------
source('fun/setup.R')
library(gmapsdistance) # Distance Matrix API
library(tictoc)

#### 1) Funcao para preparar uma matriz de entrada ------------
# Recebe o .rds dos hexágonos e prepara uma matriz de origens e destinos
preparar_matriz <- function(ano=2019, munis="all", resol=7){
  
  # ano <- 2019; resol <- 8; sigla_munis <- 'goi'
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  #subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano)
  subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano) # Nao esta sendo utilizado
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder10 <- sprintf("%s/16_matriz_viagens_onibus", files_folder)
  save_folderA <- sprintf("%s/%s/dataframes", subfolder10, ano)
  dir.create(save_folderA, recursive = TRUE, showWarnings = FALSE)
  
  prepara_e_salva <- function(sigla_munis){
    #file <- read_rds(sprintf("%s/hex_%s_%s_0%s.rds", load_folder, sigla_munis, ano, resol))
    message(Sys.time(), ' - Trabalhando na cidade: ', sigla_munis, '\n')
    
    # Carregar grid de hexagonos
#    file <- 
#      read_rds(sprintf("%s/hex_%s_0%s_%s.rds", subfolder12, sigla_munis, resol, ano)) %>%
#      st_drop_geometry()
    
    # Carregar agregados populacionais
    file <- 
      read_rds(sprintf("%s/hex_agregado_%s_0%s_%s.rds", subfolder14, sigla_munis, resol, ano)) %>%
      mutate(oportunidades = empregos_total + saude_total + edu_total + cras_total) %>%
      dplyr::select(c(id_hex, sigla_muni, pop_total, oportunidades))%>%
      st_drop_geometry()
    
#    # Juntando dataframes e filtrando pop > 0
#    file <- left_join(file, file2) %>% dplyr::filter(pop_total != 0)
    
    # Criar dataframe temporário
    file_aux <- 
      dplyr::select(file, id_hex) %>% 
      # Duplica id_hex
      mutate(hex_dest = id_hex) %>% 
      # Expand (cria todas as combinações de origens/destinos)
      expand(id_hex, hex_dest)%>%
      dplyr::filter(id_hex != hex_dest)
    
    # Calcula as distâncias
    file_aux$distancia <- grid_distance(origin = file_aux$id_hex, destination = file_aux$hex_dest)
    
    # 1. Filtra distancias
    file_aux <- file_aux%>%
      dplyr::filter(distancia != 0 & distancia <=5)
    
    file2 <- 
      dplyr::select(file, c(id_hex, pop_total)) %>% 
      #dplyr::select(-(hex_dest)) %>%
      left_join(file_aux, by= "id_hex")
    
    file2 <- left_join(file2, dplyr::select(file, c(id_hex, oportunidades)), by = c("hex_dest" = "id_hex"))
    
    file2 <- file2 %>%
      dplyr::filter(pop_total > 0)%>%
      dplyr::filter(oportunidades > 0)

    # Salvar
     write_rds(file2,
               sprintf("%s/df_%s_0%s_%s.rds", save_folderA, sigla_munis, resol, ano), 
               compress = 'gz')
    #save(file, file=sprintf("%s/df_%s_0%s_%s.Rdata", save_folderA, sigla_munis, resol, ano))
  }
  # Aplicar funcao
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  lapply(X = x, FUN = prepara_e_salva)
  
}

# Preciso fazer um por um
files_folder = "../../indice-mobilidade_dados/16_matriz_viagens_onibus/2019/dataframes"

cidade <- "ula"
preparar_matriz(munis = cidade)
df <- read_rds(sprintf("%s/df_%s_07_2019.rds", files_folder, cidade))

# ==================================

#### 2) Funcao para realizar as requisições na API ------------

# faz a leitura do arquivo .Rdata localizado na pasta dataframes
# aplica filtros de acordo com a distancia especificada
# faz as requisições
map_distance_matrix <- function(ano, munis="all", resol="07"){
  api_key <-  set.api.key("API_KEY") # Precisa da API KEY para funcionar
  
  # Criar estrutura de pasta
  files_folder <- sprintf("../../indice-mobilidade_dados")
  subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano) # É preciso carregar os hex_muni de novo para trazer os centroides corrigidos
  subfolder16 <- sprintf("%s/16_matriz_viagens_onibus/%s", files_folder, ano)
  df_folder <- sprintf("%s/dataframes", subfolder16)
  
  if (ano %nin% list.dirs("../../indice-mobilidade_dados/16_matriz_viagens_onibus", recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder16)
  }
  
  if ("dataframes" %nin% list.dirs(subfolder16, recursive = FALSE, full.names = FALSE)){
    dir.create(df_folder)
  }

  fazer_requisicao <- function(sigla_munis){
    # Carregar dataframes
    file <- read_rds(sprintf("%s/df_%s_%s_%s.rds", df_folder, sigla_munis, resol, ano)) 
    tic("Preparando strings")
    # Teste com 5 linhas
    #file <- head(file, 10)
    
    file_aux <- read_rds(sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_munis, resol, ano))%>%
      dplyr::select(id_hex, ponto_viario)%>%
      st_drop_geometry()%>%
      mutate(ponto_viario = ifelse(as.character(ponto_viario) == "c(0, 0)", h3_to_point(id_hex), ponto_viario))
    
    # Agora eu vou fazer dois joins, um pra origem e outro pra destino
    # Origem
    file <- file %>%
      left_join(file_aux, by="id_hex")%>%
      rename(ponto_origem = ponto_viario)
    # Destino
    file <- file %>%
      left_join(file_aux, by = c("hex_dest" = "id_hex"))%>%
      rename(ponto_destino = ponto_viario)
    
    # 2. Separo Lat e Long
    # Origem
    file <- file %>%
      mutate(lat_orig = unlist(map(file$ponto_origem,2)),
             long_orig = unlist(map(file$ponto_origem,1)))
    #    # Destino
    file <- file %>%
      mutate(lat_dest = unlist(map(file$ponto_destino,2)),
             long_dest = unlist(map(file$ponto_destino,1)))
    #    
    #    # 3. Junto numa string para o padrao da API
    #    # Origem
    file <- file %>%
      mutate(origem = str_c(lat_orig, long_orig, sep='+'))
    # Destino
    file <- file %>%
      mutate(destino = str_c(lat_dest, long_dest, sep='+'))  
    # Seleciona colunas
    file <- file %>%
      dplyr::select(id_hex, pop_total, oportunidades, hex_dest, distancia, origem, destino)
    
    # Para o problema de BHO, eliminou-se o hexágono da base
#    file <- file %>% 
#      dplyr::filter(id_hex != "87a881a58ffffff" & hex_dest != "87a881a58ffffff")
    
    toc()
    
   # ===== REQUISICOES ===== 
   # Aqui eu testei a API com um tamanho reduzido
    #file <- head(file, 5)
   # Fazer requisicoes
    tic("Requisições na API")
    origin <- file$origem
    destination <- file$destino
    
    matriz <- as.data.frame(gmapsdistance(
      origin = origin,
      destination = destination,
      mode = "transit", # transporte publico
      shape = 'long', # or long # shape = long parece fazer mais sentido
      #departure = 'now', 
      dep_date = "2022-02-16", # dois meses no futuro parece ser o limite
      dep_time = "07:00:00",~
      combinations = 'pairwise', # Faz o calculo linha a linha
      key = api_key))
    
    toc()
    
    # Seleciona e organiza colunas para salvar
    matriz <- merge(file, matriz, by.x = c("origem", "destino"), by.y = c("Time.or", "Time.de"))%>%
      dplyr::select(-c(Distance.or, Distance.de, Status.or, Status.de))%>%
      dplyr::rename(distancia_hex = distancia, tempo_viagem = Time.Time, distancia_viagem = Distance.Distance, status = Status.status)%>%
      dplyr::select(c(id_hex, pop_total, hex_dest, oportunidades, distancia_hex, origem, destino, tempo_viagem, distancia_viagem, status))
     
  # 4.0 Salvar
   write_rds(matriz, 
             sprintf("%s/matriztp_%s_%s_%s.rds", subfolder16, sigla_munis, resol, ano), compress = 'gz')
    #save(matriz, file=sprintf("%s/df_%s_0%s_%s.Rdata", files_folder, sigla_munis, resol, ano))

   return(matriz)
  }
  
  # 5.0 Aplicar funcao
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  lapply(X = x, FUN = fazer_requisicao)

}

# Executar funcao  ------------
map_distance_matrix(ano=2019, munis="bho")

# ==================================
files_folder = "../../indice-mobilidade_dados/16_matriz_viagens_onibus/2019"
file <- read_rds(sprintf("%s/matriztp_bho_07_2019.rds", files_folder))

