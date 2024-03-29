# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.3 Calcula matriz de tempo de viagem ideal com o R5 - nesta versão do
###### script, velocidades de bicicleta e a pé são maiores do que as anteriores


# carregar bibliotecas
# options(java.parameters = "-Xmx64G")
options(java.parameters = '-Xmx14G')
source('fun/setup.R')


calculate_ttmatrix <- function(sigla_muni, ano, res = '8') {
  
  # sigla_muni <- "nat"; ano <- 2019; res <- '7'
  
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  
  subfolder15A <- sprintf("%s/15_r5r/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  subfolder15B <- sprintf("%s/15_r5r/02_points/%s", files_folder, ano)
  subfolder15D <- sprintf("%s/15_r5r/04_output_ttmatrix_ideal/%s", files_folder, ano)
  dir.create(subfolder15D, recursive = TRUE, showWarnings = FALSE)
  
  # r5 setup
  setup <- setup_r5(data_path = subfolder15A)
  
  # points
  points <- fread(sprintf("%s/points_%s_%s_%s.csv", subfolder15B, sigla_muni, res, ano))
  colnames(points) <- c("id", "lon", "lat")
  
  # select date
  date <- "2019-10-23"
  
  max_walk_dist <- 1000   # meters
  # max_trip_duration <- 180 # minutes
  max_trip_duration_walk <- 30 # minutes
  max_trip_duration_bike <- 30 # minutes
  max_trip_duration_car  <- 60 # minutes
  departure_pico <- paste0(date, " 07:00:00")
  # departure_fpico <- paste0(date, " 14:00:00")
  departure_datetime_pico <- as.POSIXct(departure_pico, format = "%Y-%m-%d %H:%M:%S")
  # departure_datetime_fpico <- as.POSIXct(departure_fpico, format = "%Y-%m-%d %H:%M:%S")
  
  
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
  
  
  
  
  
  # # Calcular para modo carro somente se resolução for abaixo de 8
  # if (strtoi(res, base = 10L) < 8){
  #   # Criar faixas de horário para o cálculo de viagens de automóvel
  #   car_times <- c(paste0(date, " 06:00:00"), paste0(date, " 06:30:00"),
  #                  paste0(date, " 07:00:00"), paste0(date, " 07:30:00"), 
  #                  paste0(date, " 08:00:00"), paste0(date, " 01:00:00"))
  #   # car_times <- c(paste0(date, " 07:00:00"), paste0(date, " 14:30:00"), paste0(date, " 01:00:00"))
  #   
  #   # Calcular matriz de tempos de viagem para um horário específico de saída
  #   calculate_car_times <- function(car_datetime){
  #     # Transformar horários no formato POSIXct
  #     start_date <- as.POSIXct(car_datetime, format = "%Y-%m-%d %H:%M:%S")
  #     print(start_date)
  #     
  #     # Calcular tempos pelo r5r
  #     tmp_car_matrix <- travel_time_matrix(r5r_core = setup,
  #                                          origins = points,
  #                                          destinations = points,
  #                                          mode = "CAR",
  #                                          departure_datetime = start_date,
  #                                          max_trip_duration = 60,
  #                                          verbose = FALSE) 
  #     
  #     r5r::stop_r5()
  #     
  #     return(tmp_car_matrix)
  #   }
  #   
  #   
  #   # Criar dataframe temporário para guardar resultados
  #   tmp_df <- data.frame(fromId = character(),
  #                        toId = character(),
  #                        travel_time = numeric(),
  #                        stringsAsFactors = FALSE)
  #   
  #   # Calcular para todas as faixas de tempo, atualizar resultados em tmp_df
  #   for (car_time in car_times){
  #     boo <- calculate_car_times(car_time)
  #     tmp_df <- tmp_df %>% rbind(boo)
  #   }
  # }
    
    
    
    
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
                                 walk_speed = 3.6) # cenário ideal, default do r5r
  
  ttm_walk[, mode := "walk"]
  ttm_walk[, pico := 1]
  
  ttm_bike <- travel_time_matrix(r5r_core = setup,
                                 origins = points,
                                 departure_datetime = departure_datetime_pico,
                                 destinations = points,
                                 mode = "BICYCLE",
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration_bike,
                                 bike_speed = 15, # cenário ideal para bicicletas
                                 max_lts = 2)
  
  ttm_bike[, mode := "bike"]
  ttm_bike[, pico := 1]
  
  # juntar matrizes
  # ttm <- rbind(ttm_pico, ttm_fpico, ttm_walk, ttm_bike)
  if (strtoi(res, base = 10L) < 8) {
    ttm <- rbind(ttm_walk, ttm_bike, ttm_car)
  } else {
    ttm <- rbind(ttm_walk, ttm_bike)
  }
  
  # rename columns
  ttm <- ttm %>% rename(origin = fromId, destination = toId) %>% setDT()
  
  # identify columns
  ttm[, city := sigla_muni]
  ttm[, ano := ano]
  
  table(ttm$mode, useNA = 'always')
  table(ttm$pico, useNA = 'always')
  
  # save ttmatrix/
  fwrite(ttm, sprintf("%s/ttmatrix_ideal_%s_%s_%s_r5.csv", subfolder15D, sigla_muni, res, ano))
  
  
}


# rodar função
walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni,
     calculate_ttmatrix, ano = 2019, res = '08')
