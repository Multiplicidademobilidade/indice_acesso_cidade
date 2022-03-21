#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.1 Criação de graphs no OpenTripPlanner

# Programa OpenTripPlanner (otp-2.1.0-shaded.jar) deve ser baixado manualmente do link:
# https://repo1.maven.org/maven2/org/opentripplanner/otp/1.5.0/
# https://repo1.maven.org/maven2/org/opentripplanner/otp/2.1.0/
# e copiado para a pasta "../../indice-mobilidade_dados/00_Originais/OTP"

# carregar bibliotecas
source("fun/setup.R")
library('opentripplanner') # depende de install.packages('RcppSimdJson')
library('mapview')


sigla_muni <- 'nat'; ano <- 2019; ano_pbf <- 2021

# Estrutura de pastas
files_folder <- "../../indice-mobilidade_dados"
# subfolder00  <- sprintf("%s/00_Originais", files_folder)
subfolder10  <- sprintf("%s/10_topografia", files_folder, sigla_muni)
subfolder11 <- sprintf("%s/11_malha_viaria/%s", files_folder, ano_pbf)
subfolder15  <- sprintf("%s/15_otp", files_folder)
subfolder15A <- sprintf("%s/15_otp/01_graphs/%s/%s", files_folder, ano, sigla_muni)
dir.create(subfolder15A, recursive = TRUE, showWarnings = FALSE)

# otp-1.5.0-shaded.jar
# path_data <- subfolder15A
 

# Caso ainda não exista, baixar Open Trip Planner (OTP1) e guardar na pasta subfolder00
otp_file <- sprintf('%s/otp-2.1.0-shaded.jar', subfolder15)
if (!file.exists(otp_file)) {
  path_otp <- otp_dl_jar(subfolder15, version = '2.1.0', cache = FALSE)
} else {
  path_otp <- otp_file
}

# # Caso ainda não exista, baixar Open Trip Planner (OTP1) e guardar na pasta subfolder00
# otp_file <- sprintf('%s/otp-1.5.0-shaded.jar', subfolder15)
# if (!file.exists(otp_file)) {
#   path_otp <- otp_dl_jar(subfolder15, version = '2.1.0', cache = FALSE)
# } else {
#   path_otp <- otp_file
# }

# Criar pasta de demonstração com a Isle of Wight
otp_dl_demo(subfolder15)
log1 <- otp_build_graph(otp = path_otp, dir = subfolder15, memory = 10240) 
log2 <- otp_setup(otp = path_otp, dir = subfolder15)


# Criar estrutura de pastas temporárias para OTP
tmp_otp1 <- sprintf("%s/tmp", subfolder15)
tmp_otp2 <- sprintf("%s/graphs/default", tmp_otp1)
dir.create(tmp_otp2, recursive = TRUE, showWarnings = FALSE)

# Criar cópia do arquivo .pbf na nova pasta temporária
file.copy(sprintf('%s/%s/%s_%s.osm.pbf', subfolder11, sigla_muni, sigla_muni, ano), tmp_otp2)

# Criar cópia do arquivo de topografia .tif na nova pasta temporária
file.copy(sprintf('%s/%s/topografia_%s.tif', subfolder10, sigla_muni, sigla_muni), tmp_otp2)

# Criar graph para a cidade
log1 <- otp_build_graph(otp = path_otp, dir = tmp_otp1, memory = 10240) 

# Carregar o OTP e o graph para a cidade
log2 <- otp_setup(otp = path_otp, dir = tmp_otp1)

# router_config <- otp_make_config("router")
# router_config$routingDefaults$walkSpeed <- 0.7222222 # 0.722 mps = 2.6 km/h
# router_config$routingDefaults$bikeSpeed <- 3.3333333 # 3.333 mps = 12 km/h
# router_config$routingDefaults$maxWalkDistance <- 1000

# otp_validate_config(router_config)
# otp_write_config(router_config,
#                  dir = subfolder15,
#                  router = "default")


otpcon <- otp_connect(otp_version = '2.1')
# O otp_connect não está conseguindo pegar a versão do OTP, informar manualmente
otpcon$otp_version <- '2.1'

# otpcon <- otp_connect(otp_version = '1.5')
# # O otp_connect não está conseguindo pegar a versão do OTP, informar manualmente
# otpcon$otp_version <- '1.5'


route <- otp_plan(otpcon, 
                  mode = 'BICYCLE',
                  # ncores = 8,
                  routeOptions = routingOptions,
                  get_geometry = TRUE,
                  # fromPlace = c(-1.17502, 50.64590), 
                  # toPlace = c(-1.15339, 50.72266)),
                  fromPlace = c(-35.245341, -5.733231), 
                  toPlace = c(-35.2555406, -5.8254397))
route$distance / 1000
route$distance / route$duration * 3.6
mapview(route['geometry'])

route <- otp_plan(otpcon, 
                  mode = 'WALK',
                  # ncores = 8,
                  routeOptions = routingOptions,
                  get_geometry = TRUE,
                  # fromPlace = c(-1.17502, 50.64590), 
                  # toPlace = c(-1.15339, 50.72266)),
                  fromPlace = c(-35.25513, -5.74752), 
                  #fromPlace = c(-35.1903967, -5.8701859),
                  toPlace = c(-35.21770, -5.82642))

route$distance / 1000
route$distance / route$duration * 3.6
mapview(route['geometry'])

# Descrição dos resultados do Itinerary
# http://dev.opentripplanner.org/apidoc/1.4.0/json_Itinerary.html
route$duration


# Definir velocidades para bicicleta e a pé
routingOptions <- otp_routing_options()
routingOptions$walkSpeed <- 0.75
# routingOptions$bikeSpeed <- 3.3333333
routingOptions$bikeSpeed <- 3.5
# routingOptions$maxWalkDistance <- 1000
routingOptions <- otp_validate_routing_options(routingOptions)


# Parar o OTP, que está rodando em Java - Avisõ: vai parar todos os programas 
# que estiverem rodando em Java, segundo a documentação
# https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html
otp_stop()


-35.245341, -5.733231,
-35.2555406, -5.8254397,
-35.274004,12.67, -5.8044573

# FUNCAO PARA CONSTRUIR GRAPH (SO RODAR UMA VEZ!) -------------------------
# graph.obj é salvo na pasta './otp/graphs/cidade

construir_graph_muni <- function(sigla_muni, ano, 
                                 otp = "../../otp/programs/otp-1.5.0-shaded.jar",
                                 memory = 12000) {
  
  if (file.exists(sprintf("../../otp/graphs/%s/%s/Graph.obj", ano, sigla_muni))) {
    file.remove(sprintf("../../otp/graphs/%s/%s/Graph.obj", ano, sigla_muni))
  }
  
  dir <-  "../../otp"
  router <-  sprintf("%s/%s", ano, sigla_muni)
  
  message(paste0("Criando Graph da cidade ",sigla_muni, " para o ano ", ano, "\n"))
  
  text <- paste0("java -Xmx", memory, "M -jar \"", otp, "\" --build \"", 
                 dir, "/graphs/", router, "\"")
  
  options(max.print = 10000000)
  
  a <- system(text, intern = TRUE)
  
  sink(file = sprintf("../../otp/graphs/%s/%s/graph_log_%s_%s.txt", ano, sigla_muni, sigla_muni, ano))
  print(a)
  sink()
  
  options(max.print = 1000)
}


# aplicar funcao ------------------------------------------------------------------------------
lapply(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, construir_graph_muni, ano = 2019)


