
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
#library(gmapsdistance) # Distance Matrix API
source('fun/gmapsdistance.R') # Vamos utilizar uma versao editada num primeiro momento

#### 1) Funcao para preparar uma matriz de entrada ------------
# Recebe o .rds dos hexágonos e prepara uma matriz de origens e destinos
preparar_matriz <- function(ano=2019, munis="all", resol=7){
  
  # ano <- 2019; resol <- 8; sigla_munis <- 'goi'
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  #subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano) # Nao esta sendo utilizado
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder16 <- sprintf("%s/16_ttmatrix_motorizados/01_onibus", files_folder)
  df_folder <- sprintf("%s/%s/dataframes", subfolder16, ano)
  
  if (dataframes %nin% list.dirs("../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus/2019", recursive = FALSE, full.names = FALSE)){
    dir.create(df_folder)
  }
  
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
               sprintf("%s/df_%s_0%s_%s.rds", df_folder, sigla_munis, resol, ano), 
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


#### 2) Funcao para realizar as requisições na API ------------

# faz a leitura do arquivo .Rdata localizado na pasta dataframes
# aplica filtros de acordo com a distancia especificada
# faz as requisições
map_distance_matrix <- function(ano, munis="all", resol="07", rodada=1){
  api_key <-  set.api.key("AIzaSyCOH4rvtara3oRGNXx7gecSKFV7n5VFgr4") # Precisa da API KEY para funcionar
  
  # Criar estrutura de pasta
  files_folder <- sprintf("../../indice-mobilidade_dados")
  subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano) # É preciso carregar os hex_muni de novo para trazer os centroides corrigidos
  subfolder16 <- sprintf("%s/16_ttmatrix_motorizados/01_onibus/%s", files_folder, ano)
  df_folder <- sprintf("%s/dataframes", subfolder16)
  
  if (ano %nin% list.dirs("../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus", recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder16)
  }
  
  if ("dataframes" %nin% list.dirs(subfolder16, recursive = FALSE, full.names = FALSE)){
    dir.create(df_folder)
  }

  fazer_requisicao <- function(sigla_munis){
    # Carregar dataframes
    
    tic("Preparando strings")
    # Teste com 5 linhas
    #file <- head(file, 8)
    
    if (rodada == 1){
      file <- read_rds(sprintf("%s/df_%s_%s_%s.rds", df_folder, sigla_munis, resol, ano)) 
      coluna <- "centroides_muni"
      file_aux <- read_rds(sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_munis, resol, ano))%>%
        dplyr::select(id_hex, coluna)%>%
        st_drop_geometry()
      
    }else if (rodada == 2){
      target <- c("ROUTE_NOT_FOUND", NA)
      file <- read_rds(sprintf("%s/matriztp_%s_%s_%s.rds", subfolder16, sigla_munis, resol, ano))%>%
        dplyr::filter(status %in% target)%>%
        dplyr::select(id_hex:distancia)
      
      coluna <- "ponto_viario"
      file_aux <- read_rds(sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_munis, resol, ano))%>%
        dplyr::select(id_hex, coluna)
      file_aux <- file_aux%>%
        st_drop_geometry()%>%
        mutate(ponto_viario = ifelse(as.character(ponto_viario) == "c(0, 0)", h3_to_point(id_hex), ponto_viario))
    }else{
      stop("Valor incorreto, selecione 1 para rodar a partir do centroide ou 2 para rodar a partir do ponto viario")
    }
    
#    file_aux <- read_rds(sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_munis, resol, ano))%>%
#      dplyr::select(id_hex, rodada)%>%
#      st_drop_geometry()%>%
#      mutate(ponto_viario = ifelse(as.character(ponto_viario) == "c(0, 0)", h3_to_point(id_hex), ponto_viario))
    
    # Primeira requisicao = centroides_muni
    # Ponto de Origem
    file <- file %>%
      left_join(file_aux, by="id_hex")%>%
      rename(ponto_origem = coluna)
    # Ponto de Destino
    file <- file %>%
      left_join(file_aux, by = c("hex_dest" = "id_hex"))%>%
      rename(ponto_destino = coluna)
    
    # String Origem
    file <- file %>%
      mutate(lat_orig = str_extract(ponto_origem, pattern = "([:punct:]\\d+.\\d+)\\)"),
             long_orig = str_extract(ponto_origem, pattern = "[:punct:]\\d+.\\d+"))
    file <- file %>%
      mutate(origem = str_c(lat_orig, long_orig, sep='+'))
    file$origem <- gsub(")", "", file$origem)
    
    # String Destino
    file <- file %>%
      mutate(lat_dest = str_extract(ponto_destino, pattern = "([:punct:]\\d+.\\d+)\\)"),
             long_dest = str_extract(ponto_destino, pattern = "[:punct:]\\d+.\\d+"))
    file <- file %>%
      mutate(destino = str_c(lat_dest, long_dest, sep='+'))
    file$destino <- gsub(")", "", file$destino)
    
    file <- file %>%
      dplyr::select(id_hex, pop_total, hex_dest, oportunidades, distancia, origem, destino)
    
    # Para o problema de BHO, eliminou-se o hexágono da base:
#    file <- file %>% 
#      dplyr::filter(id_hex != "87a881a58ffffff" & hex_dest != "87a881a58ffffff")
#    file <- file%>%
#      mutate(origem = ifelse(as.character(id_hex) == "87a881a58ffffff", "-19.993018+-43.960748", origem))%>%
#      mutate(destino = ifelse(as.character(hex_dest) == "87a881a58ffffff", "-19.993018+-43.960748", destino))
    
    toc()
    
   # ===== REQUISICOES ===== 
   # Teste com dataframe reduzido
  #  file <- head(file, 8)
    
    tic("Requisições na API")
    # Fazer requisicoes
    request <- function(origem, destino){
      as.data.frame(gmapsdistance(
        origin = origem,
        destination = destino,
        mode = "transit", # transporte publico
        transit_mode = "bus", 
        shape = 'long', 
        #departure = 'now', # data e horario especificados abaixo
        dep_date = "2022-02-16", 
        dep_time = "07:00:00",
        combinations = 'pairwise', 
        key = api_key)) 
    }
    
    file <- file%>%
      add_column(T15 = NA)%>%
      add_column(T30 = NA)%>%
      add_column(T45 = NA)%>%
      add_column(T60 = NA)%>%
      add_column(T60_ = NA)%>%
      add_column(status = NA)
    
    # LOOP FOR ESTA DANDO PROBLEMA, TESTAR E CORRIGIR NA LINHA 319
    
    # "data" is our dataset, containing pairs of origins and destinations
    for (i in 1:nrow(file)){
      print(i)
      # Two temporary variables (req, req_aux) are created with our request results
      # "req" and "req_aux" will be replaced in the next iteration without being stored
      req <- map2(file$origem[i], file$destino[i], purrr::safely(request))
      print(req[[1]]$result)
      result <- as.data.frame(req[[1]]$result) 
      print(result)
      print('------------------entrando nas condicionais-------------------')
      
      # Se o resultado for nulo eu nao faco nada
      # if (is_null(result)){
      if (is_null(req[[1]]$result)){
        print("============== NULL ENCONTRADO =================")
        next
      }
      # Se o resultado nao for nulo eu tenho Status
      if (result$Status == "OK"){ # Se o Status for OK eu tenho Time
        file$T15[i] <- ifelse(result$Time <= 900, 1, 0)
        file$T30[i] <- ifelse(result$Time > 900 & result$Time <= 1800, 1, 0)
        file$T45[i] <- ifelse(result$Time > 1800 & result$Time <= 2700, 1, 0)
        file$T60[i] <- ifelse(result$Time > 2700 & result$Time <= 3600, 1, 0)
        file$T60_[i] <- ifelse(result$Time > 3600, 1, 0)
        file$status[i] <- result$Status
        # Se o Status nao for OK eu pego o Status (ROUTE_NOT_FOUND)
      }else { 
        file$T15[i] <- result$Status
        file$T30[i] <- result$Status
        file$T45[i] <- result$Status
        file$T60[i] <- result$Status
        file$T60_[i] <- result$Status
        file$status[i] <- result$Status
      }
      print('---------------------------saindo-----------------\n\n\n\n')
    }

#    # Inputs
#    origem <- file$origem
#    destino <- file$destino
#    
#    requisicao <- map2(origem, destino, purrr::safely(request))
#    # Outputs
#    matriz <- data_frame()
#    for (i in 1:length(requisicao)){
#      if (!(is_empty(requisicao[[i]]$result))){
#        matriz <- rbind(matriz, requisicao[[i]]$result)
#      }
#      else (matriz[ nrow(matriz) + 1 , ] <- NA)
#    }
    

#    file <- file %>%
#      mutate(ID = row_number())
#    matriz <- matriz %>%
#      mutate(ID = row_number())
    
#    matriz <- merge(file, matriz, by="ID")
    matriz <- file
    toc()
    
    # Seleciona e organiza colunas para salvar
#    matriz <- merge(file, matriz, by.x = c("origem", "destino"), by.y = c("Time.or", "Time.de"))%>%
#      dplyr::select(-c(Distance.or, Distance.de, Status.or, Status.de))%>%
#      dplyr::rename(distancia_hex = distancia, tempo_viagem = Time.Time, distancia_viagem = Distance.Distance, status = Status.status)%>%
#      dplyr::select(c(id_hex, pop_total, hex_dest, oportunidades, distancia_hex, origem, destino, tempo_viagem, distancia_viagem, status))
     
  # 4.0 Salvar
  print("---------- Salvando arquivo ----------")
  if (rodada == 1){
    write_rds(matriz, 
              sprintf("%s/matriztp_%s_%s_%s.rds", subfolder16, sigla_munis, resol, ano), compress = 'gz') #matriztp_%s_%s_%s.rds
  }else {
    target <- c("ROUTE_NOT_FOUND", NA)
    file_aux2 <- read_rds(sprintf("%s/matriztp_%s_%s_%s.rds", subfolder16, sigla_munis, resol, ano))%>% # CORRIGIR AQUI
      dplyr::filter(status %nin% target)
    matriz <- rbind(file_aux2, matriz)
    write_rds(matriz, 
              sprintf("%s/matriztp2_%s_%s_%s.rds", subfolder16, sigla_munis, resol, ano), compress = 'gz')
  }
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
map_distance_matrix(ano=2019, munis="rio", rodada=2)


#### 3) Checar resultados ------------

# Gerar relatorio
relatorio_requisicao <- function(sigla_muni, resol, ano){
  matriz_folder = "../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus/2019"
  file <- read_rds(sprintf("%s/matriztp2_%s_%s_%s.rds", matriz_folder, sigla_muni, resol, ano))
  erros <- nrow(filter(file, is.na(status)))
  not_found <- nrow(filter(file, status=="ROUTE_NOT_FOUND"))
  print(sprintf("Gerando Relatório para cidade: %s | resolução: %s | ano: %s", sigla_muni, resol, ano))
  print(sprintf("Número de erros encontrados: %s", erros))
  print(sprintf("Número de rotas não encontradas: %s", not_found))
}

relatorio_requisicao(sigla_muni= "rio", resol="07", ano=2019)

# ----

# Carregar arquivos
matriz_folder = "../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus/2019"
file <- read_rds(sprintf("%s/matriztp_rec_07_2019.rds", matriz_folder))#%>%
#dplyr::filter(Status!="ROUTE_NOT_FOUND")#%>%
#dplyr::select(id_hex:distancia)

target <- c("ROUTE_NOT_FOUND", NA)
file_aux2 <- read_rds(sprintf("%s/matriztp_cgr_07_2019.rds", matriz_folder))%>% # CORRIGIR AQUI
  dplyr::filter(status %nin% target)

file%>%
  dplyr::filter(Status %in% target)

target <- c("ROUTE_NOT_FOUND", NA)
file2 <- read_rds(sprintf("%s/matriztp_sne_07_2019.rds", matriz_folder))%>%
  dplyr::filter(status %in% target)%>%
  dplyr::select(id_hex:distancia)

file2 <- read_rds(sprintf("%s/matriztp2_bho_07_2019.rds", matriz_folder))


#file <- dplyr::filter(file, id_hex=="87a881a4dffffff")


# ===== Adequar matrizes antigas =====

adequa_matrizes <- function(muni){
  
  matriz_folder = "../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus/2019"
  #file <- read_rds(sprintf("%s/matriztp_nat_07_2019.rds", matriz_folder))
  #file2 <- read_rds(sprintf("%s/matriztp_spo_07_2019.rds", matriz_folder))
  file <- read_rds(sprintf("%s/matriztp_%s_07_2019.rds", matriz_folder, muni))
    
    file$T15 <- ifelse(file$Time <= 900, 1, 0)
    file$T30 <- ifelse(file$Time > 900 & file$Time <= 1800, 1, 0)
    file$T45 <- ifelse(file$Time > 1800 & file$Time <= 2700, 1, 0)
    file$T60 <- ifelse(file$Time > 2700 & file$Time <= 3600, 1, 0)
    file$T60_ <- ifelse(file$Time > 3600, 1, 0)
    file <- file%>%
              dplyr::rename(status = Status)%>%
              dplyr::relocate(status, .after = T60_)%>%
              dplyr::select(-c(ID, Time, Distance))
    
  write_rds(file, 
            sprintf("%s/matriztp_%s_07_2019.rds", matriz_folder, muni), compress = 'gz')

}

adequa_matrizes("rec")

matriz_folder = "../../indice-mobilidade_dados/16_ttmatrix_motorizados/01_onibus/2019"
file <- read_rds(sprintf("%s/matriztp2_for_07_2019.rds", matriz_folder))
