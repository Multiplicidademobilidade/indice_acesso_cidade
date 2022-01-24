
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

#### 1) Funcao para preparar uma matriz de entrada ------------
# Recebe o .rds dos hexágonos e prepara uma matriz de origens e destinos
preparar_matrizes_od <- function(ano = 2019, munis = "all", resol = 7){
  
  # ano <- 2019; resol <- 7; sigla_munis <- 'vta'
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14  <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder16  <- sprintf("%s/16_ttmatrix_motorizados/%s", files_folder, ano)
  subfolder16A <- sprintf("%s/16_ttmatrix_motorizados/%s/00_matrizes_od/", files_folder, ano)
  dir.create(subfolder16A, recursive = TRUE, showWarnings = FALSE)
  
  prepara_e_salva <- function(sigla_munis){
    #file <- read_rds(sprintf("%s/hex_%s_%s_0%s.rds", load_folder, sigla_munis, ano, resol))
    message(Sys.time(), ' - Trabalhando na cidade: ', sigla_munis, '\n')
    
    # Checar se arquivo resultante já existe. Se sim, avisar e pular a cidade
    out_file <- sprintf("matriz_od_%s_0%s_%s.rds", sigla_munis, resol, ano)
    
    if (out_file %nin% list.files(subfolder16A)){
      # Carregar agregados populacionais
      open_file <- sprintf("%s/hex_agregado_%s_0%s_%s.rds", subfolder14, sigla_munis, resol, ano)
      hex_agregado <- read_rds(open_file)
      
      # Simplificar o dataframe - queremos os dados de população e oportunidades
      hex_agregado <- 
        hex_agregado %>% 
        st_drop_geometry() %>% 
        mutate(oportunidades = empregos_total + saude_total + edu_total + cras_total) %>%
        dplyr::select(c(id_hex, sigla_muni, pop_total, oportunidades))
        
      
      # Criar dataframe temporário com combinações origem-destino e distâncias
      # entre os hexágonos
      file_aux <- 
        hex_agregado %>% 
        dplyr::select(id_hex) %>% 
        # Duplica id_hex
        mutate(hex_dest = id_hex) %>% 
        # Expand (cria todas as combinações de origens/destinos)
        expand(id_hex, hex_dest)%>%
        # Retirar linhas em que origem == destino
        dplyr::filter(id_hex != hex_dest)
      
      # Criar exceção para a cidade de Manaus, que tem muitos (muitos!) hexágonos
      # e que, por isso, vai dar problemas de falta de memória no RStudio ao
      # tentar processar o grid_distance()
      if (sigla_munis != 'man'){
        # Calcular as distâncias entre os hexágonos
        file_aux$distancia <- grid_distance(origin = file_aux$id_hex, destination = file_aux$hex_dest)
        
        # Filtrar distâncias: queremos hexágonos a até 5 de distância
        file_aux <- file_aux %>% dplyr::filter(distancia != 0 & distancia <= 5)
        
      } else{
        message('Dividindo hexágonos de Manaus em duas partes\n')
        
        # Dividir dataframe de Manaus em dois para conseguir rodar
        full_size <- nrow(file_aux) # 5.356.910
        half_size <- full_size / 2
        df_man1 <- file_aux %>% slice(1:half_size)
        df_man2 <- file_aux %>% slice(half_size+1:full_size)
        rm(file_aux)
        
        message('\nProcessando Manaus - parte 1\n')
        # Calcular as distâncias entre os hexágonos - df_man1
        df_man1$distancia <- grid_distance(origin = df_man1$id_hex, destination = df_man1$hex_dest)
        # Filtrar distâncias: queremos hexágonos a até 5 de distância
        df_man1 <- df_man1 %>% dplyr::filter(distancia != 0 & distancia <= 5)
        
        message('Processando Manaus - parte 2\n')
        # Calcular as distâncias entre os hexágonos - df_man2
        df_man2$distancia <- grid_distance(origin = df_man2$id_hex, destination = df_man2$hex_dest)
        # Filtrar distâncias: queremos hexágonos a até 5 de distância
        df_man2 <- df_man2 %>% dplyr::filter(distancia != 0 & distancia <= 5)
        
        # Reconstituir file_aux com a junção das duas partes
        file_aux <- rbind(df_man1, df_man2)
        rm(df_man1, df_man2)
        
      }
      
      
      # Preparar arquivo de saída, somente com origens e destinos que têm
      # oportunidades ou população
      matriz_od <- 
        file_aux %>% 
        # Juntar dados de população, tendo como base a coluna id_hex
        left_join(subset(hex_agregado, select = c(id_hex, pop_total)), by = 'id_hex') %>% 
        # Juntar dados de oportunidades, tendo como base a coluna hex_dest
        left_join(subset(hex_agregado, select = c(id_hex, oportunidades)), by = c("hex_dest" = "id_hex")) %>% 
        # Selecionar somente linhas onde há população na origem
        dplyr::filter(pop_total > 0) %>%
        # Selecionar somente linhas onde há oportunidades no destino
        dplyr::filter(oportunidades > 0)
  
      
      # Salvar resultados
      write_rds(matriz_od, sprintf('%s/%s', subfolder16A, out_file), compress = 'gz')

      
    } else {
      message('Arquivo para a cidade ', sigla_munis, " já existe, pulando...\n\n")
    }
  
  }
  
  # Aplicar funcao
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  lapply(X = x, FUN = prepara_e_salva)
  
}


# Preparar matrizes od para as cidades - rodam todas menos Manaus
# preparar_matrizes_od(ano = 2019, munis = "man", resol = 7)
preparar_matrizes_od(ano = 2019, munis = "all", resol = 7)




library('jsonlite')

# Ler API Key do Bing
api_key <- data.table::fread("../../bing_api.txt", header = FALSE)
api_key <- api_key$V1

# Constrói e retorna a URL de query GET para o Bing Maps
build_bing_query <- function(orig_dest, mode = 'transit', t_unit = 'second', apikey = api_key){
  head <- 'https://dev.virtualearth.net/REST/v1/Routes/DistanceMatrix?'
  orig_dest    <- sprintf('%s', orig_dest)
  travel_mode  <- sprintf('&travelMode=%s', mode)
  time_unit    <- sprintf('&timeUnit=%s', t_unit)
  tail         <- sprintf('&key=%s', apikey)
  
  full_query <- sprintf('%s%s%s%s%s', head, orig_dest, travel_mode, time_unit, tail)
  
  return(full_query)
}

# Constrói e retorna a URL de query GET para o Bing Maps - versão 2
build_bing_query2 <- function(orig, dest, mode = 'transit', t_unit = 'second', apikey = api_key){
  head <- 'https://dev.virtualearth.net/REST/v1/Routes/DistanceMatrix?'
  origins      <- sprintf('origins=%s', orig)
  destinations <- sprintf('&destinations=%s', dest)
  travel_mode  <- sprintf('&travelMode=%s', mode)
  time_unit    <- sprintf('&timeUnit=%s', t_unit)
  tail         <- sprintf('&key=%s', apikey)
  
  full_query <- sprintf('%s%s%s%s%s%s', head, origins, destinations, travel_mode, time_unit, tail)
  
  return(full_query)
}


# Faz a query no Bing Maps e retorna o tempo de viagem ou que a query falhou
do_bing_query <- function(bing_url){
  r <- GET(bing_url)
  if (http_status(r)$message == 'Success: (200) OK'){
    # Fazer um parse do JSON resultante da query
    r_json <- parse_json(r, simplifyVector = FALSE)
    # Extrair tempo de viagem do JSON
    tt <- r_json$resourceSets[[1]]$resources[[1]]$results[[1]]$travelDuration
    return(tt)
    
  } else {
    return('FAILED')
  }
}


# # São Paulo
# bing_url <- build_bing_query2(orig = '-23.546440,-46.69086', dest = '-23.5341299,-46.6620708', apikey = api_key)
# r <- do_bing_query(bing_url)
# 
# # Vitória
# bing_url <- build_bing_query2(orig = '-20.259275,-40.322926', dest = '-20.280582,-40.327371', apikey = api_key)
# r <- GET(bing_url)
# r_json <- parse_json(r, simplifyVector = FALSE)


# URL de query de rota - precisa criar uma nova chave API
# https://docs.microsoft.com/en-us/bingmaps/rest-services/routes/calculate-a-route
# http://dev.virtualearth.net/REST/v1/Routes/Transit?wayPoint.1=-25.4018668701558,-49.3717785923046&viaWaypoint.2=-25.3620550649379,-49.3365370303007&optimize= timeWithTraffic&timeType=Departure&dateTime=2021-01-19T07:00:00&key=api_key


#### 2) Funcao para realizar as requisições na API ------------

# faz a leitura do arquivo .Rdata localizado na pasta dataframes
# aplica filtros de acordo com a distancia especificada
# faz as requisições
map_distance_matrix <- function(ano, munis = "all", resol = "07"){
  api_key <-  set.api.key("API_KEY") # Precisa da API KEY para funcionar
  
  # ano <- 2019; resol <- '07'; sigla_munis <- 'cur'
  
  # Estrutura de pastas
  files_folder <- sprintf("../../indice-mobilidade_dados")
  subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano)
  subfolder16A <- sprintf("%s/16_ttmatrix_motorizados/%s/00_matrizes_od/", files_folder, ano)
  
  
  fazer_requisicao <- function(sigla_munis){
    # Carregar matriz com hexágonos de origem e destino
    matriz_od <- read_rds(sprintf("%s/matriz_od_%s_%s_%s.rds", subfolder16A, sigla_munis, resol, ano)) 
    
    # Ler arquivo com os dados dos hexágonos do município
    hex_muni <- read_rds(sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_munis, resol, ano))
    # Selecionar somente colunas de interesse
    hex_muni <- hex_muni %>% st_drop_geometry() %>% dplyr::select(id_hex, centroides_muni, ponto_viario)
    # mutate(ponto_viario = ifelse(as.character(ponto_viario) == "c(0, 0)", h3_to_point(id_hex), ponto_viario))
    
    
    # -------------------------------------------------------
    # Primeira query: usando centroides_muni como pontos
    # de origem e destino
    # -------------------------------------------------------
    matriz_od2 <- 
      matriz_od %>% 
      # Descartar colunas que não serão usadas
      dplyr::select(-c(pop_total, oportunidades)) %>% 
      # Juntar dados de latlong para o hexágono de origem
      left_join(subset(hex_muni, select = c(id_hex, centroides_muni)), by = 'id_hex') %>% 
      # Renomear coluna de origem
      rename(pto_orig = centroides_muni) %>% 
      # Juntar dados de latlong para o hexágono de destino
      left_join(subset(hex_muni, select = c(id_hex, centroides_muni)), by = c("hex_dest" = "id_hex")) %>% 
      # Renomear coluna de destino
      rename(pto_dest = centroides_muni) %>% 
      # Recriar colunas de latlong
      mutate(pto_orig = as.character(pto_orig),
             pto_orig = str_replace(pto_orig, 'c\\(', ''),
             pto_orig = str_replace(pto_orig, '\\)', ''),
             pto_dest = as.character(pto_dest),
             pto_dest = str_replace(pto_dest, 'c\\(', ''),
             pto_dest = str_replace(pto_dest, '\\)', '')) %>%
      separate(pto_orig, into = c('lon_orig', 'lat_orig'), sep = ', ', remove = TRUE) %>%
      separate(pto_dest, into = c('lon_dest', 'lat_dest'), sep = ', ', remove = TRUE) %>%
      mutate(orig =    str_c(lat_orig, lon_orig, sep = ','),
             dest =    str_c(lat_dest, lon_dest, sep = ','),
             od_str = sprintf('origins=%s&destinations=%s', orig, dest)) %>% 
      dplyr::select(-c('lon_orig', 'lat_orig', 'lon_dest', 'lat_dest'))
      
    matriz_od2 <- head(matriz_od2)
    
    # Criar URL completa para query no Bing Maps
    bing_url2 <- matriz_od2 %>% dplyr::select(od_str) %>% apply(., 1, build_bing_query, t_unit = 'minute')
    # matriz_od2 <- matriz_od2 %>% cbind(bing_url)
    
    tempos <- sapply(bing_url2, do_bing_query, simplify = TRUE, USE.NAMES = FALSE)
    matriz_od2 <- matriz_od2 %>% dplyr::select(-od_str) %>% cbind(tempos)
    
    
    
    
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

