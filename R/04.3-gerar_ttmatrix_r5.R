# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem com o R5


# carregar bibliotecas
# options(java.parameters = "-Xmx64G")
options(java.parameters = '-Xmx14G')
library(r5r)
source('fun/setup.R')
# source("fun/selecionar_data_gtfs.R")


# sigla_muni <- 'bho'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'poa'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'for'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'for'; ano <- 2017
# sigla_muni <- 'spo'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'spo'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'rio'; ano <- 2019; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'rio'; ano <- 2017; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'rio'; ano <- 2018; modo <- c("WALK", "TRANSIT")
# sigla_muni <- 'rio-novo'; ano <- 2019; modo <- c("WALK", "TRANSIT")


calculate_ttmatrix <- function(sigla_muni, ano, res = '08') {
  
  # sigla_muni <- "oco"; ano <- 2019
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder15A <- sprintf("%s/15_otp/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  subfolder15B <- sprintf("%s/15_otp/02_points/%s", files_folder, ano)
  subfolder15C <- sprintf("%s/15_otp/03_output_ttmatrix/%s", files_folder, ano)
  dir.create(subfolder15C, recursive = TRUE, showWarnings = FALSE)
  
  # r5 setup
  setup <- setup_r5(data_path = subfolder15A)
  
  # points
  points <- fread(sprintf("%s/points_%s_%s_%s.csv", subfolder15B, sigla_muni, res, ano))
  colnames(points) <- c("id", "lon", "lat")
  
  # select date
  if (sigla_muni == "spo" & ano == 2017) {
    date <- "2018-05-01"
  } else {
    # date <- selecionar_data_gtfs(sigla_muni, ano)
    date <- "2019-10-23" # quarta-feira
  }
  
  max_walk_dist <- 1000   # meters
  max_trip_duration <- 180 # minutes
  max_trip_duration_walk <- 30 # minutes
  max_trip_duration_bike <- 30 # minutes
  departure_pico <- paste0(date, " 07:00:00")
  departure_fpico <- paste0(date, " 14:00:00")
  departure_datetime_pico <- as.POSIXct(departure_pico, format = "%Y-%m-%d %H:%M:%S")
  departure_datetime_fpico <- as.POSIXct(departure_fpico, format = "%Y-%m-%d %H:%M:%S")
  
  
  # 3.1) calculate a travel time matrix
  # ttm_pico <- travel_time_matrix(r5r_core = setup,
  #                                origins = points,
  #                                destinations = points,
  #                                mode = c("WALK", "TRANSIT"),
  #                                departure_datetime = departure_datetime_pico,
  #                                time_window = 120,
  #                                max_walk_dist = max_walk_dist,
  #                                max_trip_duration = max_trip_duration)
  # 
  # ttm_pico[, mode := "transit"]
  # ttm_pico[, pico := 1]
  # 
  # ttm_fpico <- travel_time_matrix(r5r_core = setup,
  #                                 origins = points,
  #                                 destinations = points,
  #                                 mode = c("WALK", "TRANSIT"),
  #                                 departure_datetime = departure_datetime_fpico,
  #                                 time_window = 120,
  #                                 max_walk_dist = max_walk_dist,
  #                                 max_trip_duration = max_trip_duration)
  # 
  # ttm_fpico[, mode := "transit"]
  # ttm_fpico[, pico := 0]
  
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
  
  # juntar matrizes
  # ttm <- rbind(ttm_pico, ttm_fpico, ttm_walk, ttm_bike)
  ttm <- rbind(ttm_walk, ttm_bike)
  
  # rename columns
  ttm <- ttm %>% rename(origin = fromId, destination = toId) %>% setDT()
  
  # identify columns
  ttm[, city := sigla_muni]
  ttm[, ano := ano]
  
  table(ttm$mode, useNA = 'always')
  table(ttm$pico, useNA = 'always')
  
  # save ttmatrix/
  fwrite(ttm, sprintf("%s/ttmatrix_%s_%s_%s_r5.csv", subfolder15C, sigla_muni, res, ano))
  
  
}


# apply function
# walk(munis_list$munis_metro[ano_metro == 2017]$abrev_muni,
#      calculate_ttmatrix, ano = 2017)
# 
# walk(munis_list$munis_metro[ano_metro == 2018]$abrev_muni,
#      calculate_ttmatrix, ano = 2018)

walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni,
     calculate_ttmatrix, ano = 2019, res = '08')
