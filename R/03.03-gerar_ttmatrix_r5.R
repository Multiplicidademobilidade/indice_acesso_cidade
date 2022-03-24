# Calcula a matriz de tempo de viagem com o R5

# Carregar bibliotecas
# options(java.parameters = "-Xmx64G")
options(java.parameters = '-Xmx14G')
source('fun/setup.R')


calculate_ttmatrix <- function(sigla_muni, ano, res = '08') {
  
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder15A <- sprintf("%s/15_r5r/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  subfolder15B <- sprintf("%s/15_r5r/02_points/%s", files_folder, ano)
  subfolder15C <- sprintf("%s/15_r5r/03_output_ttmatrix/%s", files_folder, ano)
  dir.create(subfolder15C, recursive = TRUE, showWarnings = FALSE)
  
  # r5 setup
  setup <- setup_r5(data_path = subfolder15A)
  
  # Pontos
  points <- fread(sprintf("%s/points_%s_%s_%s.csv", subfolder15B, sigla_muni, res, ano))
  colnames(points) <- c("id", "lon", "lat")
  
  # Selecionar data
  date <- "2019-10-23"
  
  max_walk_dist <- 1000   # meters
  max_trip_duration <- 180 # minutes
  max_trip_duration_walk <- 30 # minutes
  max_trip_duration_bike <- 30 # minutes
  max_trip_duration_car  <- 60 # minutes
  departure_pico <- paste0(date, " 07:00:00")
  departure_fpico <- paste0(date, " 14:00:00")
  departure_datetime_pico <- as.POSIXct(departure_pico, format = "%Y-%m-%d %H:%M:%S")
  departure_datetime_fpico <- as.POSIXct(departure_fpico, format = "%Y-%m-%d %H:%M:%S")
  
  
  # Calcular para modo carro somente se resolução for abaixo de 8
  if (strtoi(res, base = 10L) < 8) {
    ttm_car <- travel_time_matrix(r5r_core = setup,
                                  origins = points,
                                  destinations = points,
                                  mode = "CAR",
                                  departure_datetime = departure_datetime_pico,
                                  max_trip_duration = max_trip_duration_car)
    
    ttm_car[, mode := "car_r5r"]
    ttm_car[, pico := 1]
  }
  
  ttm_walk <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 destinations = points,
                                 mode = "WALK",
                                 departure_datetime = departure_datetime_pico,
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration_walk,
                                 walk_speed = 2.6) # com base em tabulações da OD 2007
  
  ttm_walk[, mode := "walk"]
  ttm_walk[, pico := 1]
  
  ttm_bike <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 departure_datetime = departure_datetime_pico,
                                 destinations = points,
                                 mode = "BICYCLE",
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration_bike,
                                 # mantendo os padrões da função mas deixando
                                 # os valores explícitos aqui
                                 bike_speed = 12, 
                                 max_lts = 2)
  
  ttm_bike[, mode := "bike"]
  ttm_bike[, pico := 1]
  
  # Juntar matrizes
  if (strtoi(res, base = 10L) < 8) {
    ttm <- rbind(ttm_walk, ttm_bike, ttm_car)
  } else {
    ttm <- rbind(ttm_walk, ttm_bike)
  }
  
  # Renomear columns
  ttm <- ttm %>% rename(origin = fromId, destination = toId) %>% setDT()
  
  # Identificar columns
  ttm[, city := sigla_muni]
  ttm[, ano := ano]
  
  table(ttm$mode, useNA = 'always')
  table(ttm$pico, useNA = 'always')
  
  # Salvar ttmatrix
  fwrite(ttm, sprintf("%s/ttmatrix_%s_%s_%s_r5.csv", subfolder15C, sigla_muni, res, ano))
  
  
}


# Rodar função
walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni,
     calculate_ttmatrix, ano = 2019, res = '08')
