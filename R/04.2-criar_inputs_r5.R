#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.3.2 Criar inputs do OpenTripPlanner
# 1. pontos de origem e destino
# 2. Scripts em Python

# carregar bibliotecas
# options(java.parameters = '-Xmx50G')
options(java.parameters = '-Xmx14G')
source('fun/setup.R')



### 1) Funcao para gerar pontos de origem e destino -----------------------------------

gerar_pontos_OTP_muni <- function(sigla_muni, ano) {
  
  # sigla_muni <- "oco"; ano <- 2019
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder15A <- sprintf("%s/15_otp/01_graphs/%s/%s", files_folder, ano, sigla_muni)
  subfolder15B <- sprintf("%s/15_otp/02_points/%s", files_folder, ano)
  dir.create(subfolder15B, recursive = TRUE, showWarnings = FALSE)
  
  # status message
  message("Trabalhando na cidade ", sigla_muni, " no ano ", ano, "\n")
  
  # lista grades relativas ao municipio - um arquivo para res. 8 e outro pra 9
  grades_muni <- paste0(subfolder14, "/",
                        list.files(paste0(subfolder14), pattern = sigla_muni))
  
  # gera r5r_core usado pra fazer o snap dos pontos
  r5r_core <- r5r::setup_r5(subfolder15A, verbose = FALSE)
  
  # gera os pontos a serem utilizados para cada resolucao
  gerar_por_resolucao <- function(endereco_grade) {
    # muni_res <- dir_muni[2]
    # endereco_grade <- grades_muni[2]
    # endereco_grade <- grades_muni
    
    # cria variavel dummy pra identificar se o hexagono tem populacao/oports
    grade <- data.table::setDT(readRDS(endereco_grade))
    grade[
      , 
      tem_pop_oport := 
        pop_total > 0 | 
        renda_total > 0 |
        empregos_total > 0 | 
        saude_total > 0 | 
        edu_total > 0
    ]
    
    # identifica resolucao utilizada
    # res <- str_extract(endereco_grade, "\\d{2}(?=_)")
    res <- '08'
    
    # gera centroides e faz snap
    # suprime warnings de calculo de centroides com lat long
    suppressWarnings(
      centroides <- sf::st_centroid(sf::st_as_sf(grade)) %>% rename(id = id_hex)
    )
    snaps <- r5r::find_snap(r5r_core, centroides)
    
    # traz a informacao se o hexagono tem populacao/oportunidades e filtra os
    # pontos a serem utilizados com base nisso
    # mantem apenas os pontos cujo snap foi aceitavel (<= 452 metros, caiu no 
    # maximo em um dos seus vizinhos imediatos) ou cujo snap foi ruim ou
    # inexistente, mas que possuem alguma populacao/oportunidade
    data.table::setnames(snaps, "point_id", "id_hex")
    snaps[grade, on = "id_hex", tem_pop_oport := i.tem_pop_oport]
    snaps[, distance := as.numeric(distance)]
    antes <- nrow(snaps)
    snaps <- snaps[distance <= 452 | tem_pop_oport]
    depois <- nrow(snaps)
    
    # mantem apenas as colunas de id, lat e lon, e renomeia lat e lon pra Y e X
    snaps <- snaps[, .(id_hex, X = lon, Y = lat)]
    
    # salva resultado
    arquivo_resultado <- sprintf('%s/points_%s_%s_%s.csv', subfolder15B, sigla_muni, res, ano)
    data.table::fwrite(snaps, arquivo_resultado)
    
    resumo <- data.frame(sigla_muni = sigla_muni,
                         ano = ano,
                         res = res,
                         points_r5 = antes,
                         points_out = depois)
  }
  
  resumo <- lapply(grades_muni, gerar_por_resolucao) %>% rbindlist()
  
  # r5r::stop_r5(r5r_core)
  
  return(resumo)
  
}

# Rodar a função
ano = 2019
# a <- lapply(munis_list$munis_metro[ano_metro == 2017]$abrev_muni, gerar_pontos_OTP_muni, ano = 2017)
# b <- lapply(munis_list$munis_metro[ano_metro == 2018]$abrev_muni, gerar_pontos_OTP_muni, ano = 2018)
c <- lapply(munis_list$munis_metro[ano_metro == ano]$abrev_muni, gerar_pontos_OTP_muni, ano = ano)

# Guardar estatísticas resumidas
# go <- rbindlist(c(a, b, c))
hex_res = "08"
go <- rbindlist(c(c))
go <- go[res == hex_res]
go <- go %>%
  mutate(points_perc = points_out/points_r5) %>%
  rename(points_total = points_r5,
         points_r5 = points_out)

# googlesheets4::write_sheet(data = go,
#                            ss = "https://docs.google.com/spreadsheets/d/11pIp1Ioiua7NWDp41DKXMMWI_3JvmzonhNMoNpvR1aE/edit#gid=215944254",
#                            sheet = "points_snap")
files_folder <- "../../indice-mobilidade_dados"
subfolder15 <- sprintf("%s/15_otp/", files_folder)
write_delim(go, sprintf("%s/15_otp/02_points/resumo_pontos_%s_%s.csv", files_folder, hex_res, ano), delim = ';')




