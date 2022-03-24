# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Faz o download de dados de malha de ruas do OSM em formato .pbf, que será utilizado no r5r

#' Esse script extrai a malha viária de cada cidade, de acordo com a bounding box
#' do arquivo raster de topografia. 
#' 
#' Instruções sobre como fazer isso foram retiradas de 
#' https://docs.analysis.conveyal.com/prepare-inputs#cropping
#' 


# carregar bibliotecas
source('fun/setup.R')

# ----------------------------------------
# 1. Criar estrutura de pastas
# ----------------------------------------
ano_pbf = 2021 # Ano a que se refere o arquivo PBF
files_folder <- "../../indice_acesso_cidade_dados"
subfolder11 <- sprintf("%s/11_malha_viaria/%s", files_folder, ano_pbf)
dir.create(sprintf("%s", subfolder11), recursive = TRUE, showWarnings = FALSE)


# ----------------------------------------
# 2. Baixar arquivo PBF
# ----------------------------------------
# Baixar o arquivo PBF do Brasil manualmente do site download.geofabrik.de/ e 
# salvar na pasta criada ('../../indice_acesso_cidade_dados/11_malha_viaria/[ano]/')


# ----------------------------------------
# 3. Extrair malha viária do arquivo PBF
# ----------------------------------------
# Para rodar esta etapa, é preciso ter o programa 'osmosis' instalado

# Checar e filtrar brazil-latest.osm.pbf
filtrar_malha_viaria_br <- function(ano, pbf_folder) {
  # ----------------------------------------
  # Código aparentemente para Windows
  # ----------------------------------------
  #' #' construir paths com a localização do Osmosis e dos arquivos PBF
  #' osmosis_path <- sprintf("../../data-raw/malha_viaria/osmosis/bin/osmosis.bat")
  #' 
  #' # PBF do Brasil inteiro
  #' br_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest.osm.pbf", ano)
  #' 
  #' # PBF do Brasil inteiro, filtrado (somente malha viária, sem polígonos)
  #' br_filtered_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest-filtered.osm.pbf", ano)
  #' 
  #' 
  #' # testar se o arquivo PBF do Brasil existe
  #' if (!file.exists(br_pbf)) {
  #'   stop(sprintf("Malha viária não encontrada. Faça o download em download.geofabrik.de e salve o arquivo em %s", br_pbf))
  #' }
  #' 
  #' 
  #' # testar se o arquivo filtrado já existe, para não repetir o processo desnecessariamente  
  #' if (file.exists(br_filtered_pbf)) {
  #'   stop(sprintf("Malha viária filtrada já existe. Para gerar uma nova, remova o arquivo %s antes.", br_filtered_pbf))
  #' }
  #' 
  #' # Constrói linha de comando para executar o Osmosis
  #' osmosis_cmd <- sprintf(
  #'   paste("%s --read-pbf %s",
  #'         "    --tf accept-ways highway=* public_transport=platform railway=platform park_ride=*",
  #'         "--tf accept-relations type=restriction",
  #'         "--used-node",
  #'         "--write-pbf %s"),
  #'   osmosis_path, br_pbf, br_filtered_pbf)
  #' 
  #' # Chama o Osmosis
  #' shell(osmosis_cmd, translate = TRUE)  
  
  
  # ----------------------------------------
  # Código para Linux
  # ----------------------------------------
  #' construir paths com a localização do Osmosis e dos arquivos PBF
  osmosis_path <- sprintf("/usr/bin/osmosis")
  
  # PBF do Brasil inteiro
  br_pbf <- sprintf("%s/brazil-latest.osm.pbf", pbf_folder)
  
  # PBF do Brasil inteiro, filtrado (somente malha viária, sem polígonos)
  br_filtered_pbf <- sprintf("%s/brazil-latest-filtered.osm.pbf", pbf_folder)
  
  
  # testar se o arquivo PBF do Brasil existe
  if (!file.exists(br_pbf)) {
    stop(sprintf("Malha viária não encontrada. Faça o download em download.geofabrik.de e salve o arquivo em %s", br_pbf))
  }
  
  
  # testar se o arquivo filtrado já existe, para não repetir o processo desnecessariamente  
  if (file.exists(br_filtered_pbf)) {
    stop(sprintf("Malha viária filtrada já existe. Para gerar uma nova, remova o arquivo %s antes.", br_filtered_pbf))
  }
  
  # Chama o Osmosis
  message('\nIniciando o osmosis - este passo deve demorar cerca de 20 minutos para rodar.\n')
  arg1 <- sprintf("--read-pbf %s", br_pbf)
  arg2 <- sprintf("--write-pbf %s", br_filtered_pbf)
  system2(command = osmosis_path,
          args = c(arg1,
                   "--tf accept-ways highway=* public_transport=platform railway=platform park_ride=*",
                   "--tf accept-relations type=restriction",
                   "--used-node",
                   arg2
          )
  )
}

filtrar_malha_viaria_br(ano = ano_pbf, pbf_folder = subfolder11)


# função para extrair a malha viária do município -------------------------
extrai_malha_viaria <- function(muni, ano_base, ano_arq_pbf) {
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder10 <- sprintf("%s/10_topografia", files_folder)
  
  ## encontrar bounding box da cidade, a partir do grid de topografia
  topo_file <- sprintf("%s/%s/topografia_%s.tif", subfolder10, muni, muni)
  topo_raster <- raster::raster(topo_file)
  muni_bbox <- raster::extent(topo_raster)
  
  # ----------------------------------------
  # Código aparentemente para Windows
  # ----------------------------------------
  #' construir paths com a localização do Osmosis e dos arquivos PBF de 
  #' origem e destino  # 
  # osmosis_path <- sprintf("../../data-raw/malha_viaria/osmosis/bin/osmosis.bat")
  # 
  # # PBF do Brasil inteiro
  # ano_brasil <- ifelse(ano %in% c(2017:2020), 2020, ano)
  # br_pbf <- sprintf("../../data-raw/malha_viaria/%s/br/brazil-latest-filtered.osm.pbf", ano_brasil)
  # 
  # # PBF do município
  # muni_pbf <- sprintf("../../data-raw/malha_viaria/%s/%s/%s_%s.osm.pbf", ano, muni, muni, ano)
  # 
  # 
  # # Cria pasta do município, caso não exista
  # muni_path <- sprintf("../../data-raw/malha_viaria/%s/%s", ano, muni)
  # if (!dir.exists(muni_path)) {
  #   dir.create(path = muni_path, recursive = TRUE)
  # }
  # 
  # # Constrói linha de comando para executar o Osmosis
  # osmosis_cmd <- sprintf("%s --read-pbf %s --bounding-box left=%s bottom=%s right=%s top=%s --write-pbf %s",
  #                        osmosis_path, br_pbf, 
  #                        muni_bbox@xmin, muni_bbox@ymin, muni_bbox@xmax, muni_bbox@ymax,
  #                        muni_pbf)
  # 
  # # Chama o Osmosis
  # tictoc::tic(msg = muni)
  # shell(osmosis_cmd, translate = TRUE)
  # tictoc::toc()
  
  
  # ----------------------------------------
  # Código para Linux
  # ----------------------------------------
  #' construir paths com a localização do Osmosis e dos arquivos PBF de 
  #' origem e destino
  osmosis_path <- sprintf("/usr/bin/osmosis")
  
  # PBF do Brasil inteiro
  br_pbf <- sprintf("%s/brazil-latest.osm.pbf", subfolder11)
  
  # PBF do município
  muni_pbf <- sprintf("%s/%s/%s_%s.osm.pbf", subfolder11, muni, muni, ano_base)
  
  # Cria pasta do município, caso não exista
  muni_path <- sprintf("%s/%s", subfolder11, muni)
  if (!dir.exists(muni_path)) {
    dir.create(path = muni_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Roda apenas se o arquivo ainda não existir
  if (!file.exists(muni_pbf)) {
    # Marcação de tempo - início
    start_time <- Sys.time()
    
    # Chama o Osmosis
    message('\nIniciando o osmosis para ', muni, '\n')
    arg1 <- sprintf("--read-pbf %s", br_pbf)
    
    # O município de Osasco vai dar erro no momento de criação da rede (script 4.1)
    if (muni == 'oco') {
      arg2 <- sprintf("--bounding-box left=%s bottom=%s right=%s top=%s completeWays='yes'", 
                      muni_bbox@xmin, muni_bbox@ymin, muni_bbox@xmax, muni_bbox@ymax)
      
    } else {
      arg2 <- sprintf("--bounding-box left=%s bottom=%s right=%s top=%s",
                      muni_bbox@xmin, muni_bbox@ymin, muni_bbox@xmax, muni_bbox@ymax)
      }
    
    arg3 <- sprintf("--write-pbf %s", muni_pbf)
    system2(command = osmosis_path, args = c(arg1, arg2, arg3))
    
    # Marcação de tempo - fim
    end_time <- Sys.time()
    message('\narq   : ', muni_pbf, 
            '\ninicio: ', start_time, 
            '\nfim   : ', end_time, 
            '\ntotal : ', round(end_time - start_time, 1),' min \n')
  } else {
    message('Arquivo para a cidade ', muni, " já existe, pulando...\n")
  }
}


# Demora de 2 a 3 minutos por cidade para rodar
ano = 2019
purrr::walk(munis_list$munis_metro[ano_metro == ano]$abrev_muni, extrai_malha_viaria, ano_base = ano, ano_arq_pbf = ano_pbf)


