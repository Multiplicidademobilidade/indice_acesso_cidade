# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade - Resolução 08

# Calcula o acesso a oportunidades para os hexágonos de resolução 08. Considera
# no cálculo *somente os modos ativos*. Para resolução 07, que considera também
# os modos motorizados, usar o script anterior.

# carregar bibliotecas
source('fun/setup.R')


#### 1. CALCULAR ACESSIBILIDADE --------------------------------------------------------------

# sigla_muni <- "spo"; ano <- 2019; res <- '08'

calcular_acess_muni <- function(sigla_muni, ano) {
  
  # Forçar resolução para 08. Para resolução 07, usar script anterior; para 
  # outras, adaptar o script.
  res <- '08'
  
  # status message
  message('Trabalhando na cidade ', sigla_muni, ' no ano ', ano,  '\n')
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder15D <- sprintf("%s/15_otp/04_output_ttmatrix_ideal/%s", files_folder, ano)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/%s", files_folder, ano)
  # dir.create(subfolder17, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  # traz os tempos dos modos ativos em minutos, calculados pelo r5r
  ttmatrix <- readr::read_delim(sprintf('%s/ttmatrix_ideal_%s_%s_%s_r5.csv',
                                        subfolder15D, sigla_muni, res, ano))
  
  # o limite de tempo a ser observado para o cálculo das acessibilidades é
  # de 60 min - descartar todas as linhas com travel_time maior do que isso.
  # Para modos ativos, o limite será 30 min, mas este filtro será feito adiante
  ttmatrix <- ttmatrix %>% filter(travel_time <= 60)

  
  
  # 2) Agregar dados de uso do solo à ttmatrix --------------------------
  
  # Pegar arquivo com os hexagonos com as atividades (oportunidades)
  dir_hex <- sprintf("%s/hex_agregado_%s_%s_%s.rds", subfolder14, sigla_muni, res, ano)
  hexagonos_sf <- readr::read_rds(dir_hex) 
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- 
    hexagonos_sf %>% 
    as.data.frame() %>% 
    dplyr::select(id_hex, 
                  # variaveis de populacao - total e sexo
                  matches("pop_"), 
                  # variaveis de populacao - cor
                  cor_branca, cor_amarela, cor_indigena, cor_negra, 
                  # variaveis de populacao - idade
                  matches("idade_"),
                  # variaveis de renda
                  renda_total, renda_capita, quintil, decil)
  
  # Filtrar apenas colunas com info de uso do solo no destino
  hex_dest <- 
    hexagonos_sf %>% 
    as.data.frame() %>% 
    dplyr::select(id_hex, 
                  # variaveis de emprego
                  empregos_total, # empregos_baixa, empregos_media, empregos_alta,  
                  # variaveis de saúde
                  matches("saude_"), 
                  # variaveis de educação
                  matches("edu_"), 
                  matches("mat_"), 
                  # variaveis cras
                  cras_total)
    
  
  
  # Merge dados de origem na matrix de tempo de viagem
  ttmatrix <- ttmatrix %>% left_join(hex_orig, by = c("origin" = "id_hex"))
  
  # Merge dados de destino na matrix de tempo de viagem
  ttmatrix <- ttmatrix %>% left_join(hex_dest, by = c("destination" = "id_hex"))
  
  # Transformar em data.table para cálculos de acessibilidade
  ttmatrix <- ttmatrix %>% setDT()
  
  # Dicionario de variaveis:
  # Acessibilidade:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
  # - TMI = Acessibilidade de Tempo Mínimo à Oportunidade
  # Atividades:
  # - PT ~ "pop_total"
  # - PB ~ "cor_branca"
  # - PA ~ "cor_amarela"
  # - PI ~ "cor_indigena"
  # - PN ~ "cor_negra"
  # - TT ~ "empregos_total"
  # - TQ ~ "empregos_match_quintil" # não usado
  # - TD ~ "empregos_match_decil" # não usado
  # - ST ~ "saude_total"
  # - SB ~ "saude_baixa"
  # - SM ~ "saude_media"
  # - SA ~ "saude_alta"
  # - ET ~ "edu_total"
  # - EI ~ "edu_infantil"
  # - EF ~ "edu_fundamental"
  # - EM ~ "edu_medio"
  # - EI ~ "edu_infantil"
  # - CT ~ "cras_total"
  
  
  # 3) Calcular acessibilidade cumulativa ativa ----------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cma <- "CMA"
  atividade_cma <- c(
    # emprego
    "TT", # "TB", "TM", "TA",
    # saude
    "ST", "SB", "SM", "SA", 
    # educacao - escolas
    "ET", "EI", "EF", "EM",
    # educacao - matriculas
    "MT", "MI", "MF", "MM", 
    # cras
    "CT")
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  # Grid resulta em:
  #  acess_sigla- atividade_sigla - tt_sigla - tt_tp - tt_ativo - junto_tp - junto_ativo - atividade_nome
  #  CMA          TT                1          30      15         CMATT30    CMATT15       empregos_total
  grid_cma <- 
    expand.grid(acess_cma, atividade_cma, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold  para cada um dos modos
    mutate(tt_ativo = case_when(
      # Linhas para tt_ativo com tempos maiores do que 30 serão retiradas depois
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "TT" ~ "empregos_total",
                                      # atividade_sigla == "TQ" ~ "empregos_match_quintil",
                                      # atividade_sigla == "TD" ~ "empregos_match_decil",
                                      atividade_sigla == "ST" ~ "saude_total",
                                      atividade_sigla == "SB" ~ "saude_baixa",
                                      atividade_sigla == "SM" ~ "saude_media",
                                      atividade_sigla == "SA" ~ "saude_alta",
                                      atividade_sigla == "ET" ~ "edu_total",
                                      atividade_sigla == "EF" ~ "edu_fundamental",
                                      atividade_sigla == "EM" ~ "edu_medio",
                                      atividade_sigla == "EI" ~ "edu_infantil",
                                      atividade_sigla == "MT" ~ "mat_total",
                                      atividade_sigla == "MF" ~ "mat_fundamental",
                                      atividade_sigla == "MM" ~ "mat_medio",
                                      atividade_sigla == "MI" ~ "mat_infantil",
                                      atividade_sigla == "CT" ~ "cras_total"))
  
  
  # gerar o codigo para calcular as oportunidades
  # para ativo
  # "CMATT15 = (sum(empregos_total[which(travel_time <= 15)], na.rm = T))"
  codigo_cma_ativo <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cma$junto_ativo, 
            grid_cma$atividade_nome, 
            grid_cma$tt_ativo
    )
  )
  
  # dar nomes às variaveis
  to_make_cma_ativo <- setNames(codigo_cma_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_ativo))
  
  
  # aplicar a acessibilidade para os modos da cidade
  # para modos ativos
  acess_cma <- ttmatrix[mode %in% c("bike", "walk"), 
                        lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                        , by = .(city, mode, origin, pico, quintil, decil)]
  
  
  # Retirar todas as colunas com tempos 45 e 60 para os modos ativos
  acess_cma <- acess_cma %>% dplyr::select(-matches('[46]'))
  
  
  
  # 4) Calcular acessibilidade cumulativa passiva --------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cmp <- "CMP"
  atividade_cmp <- c("PT", "PM", "PW",
                     "PB", "PA", "PI", "PN", 
                     "I1", "I2", "I3", "I4", "I5", "I6", "I7")
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  # acess_sigla atividade_sigla tt_sigla tt_tp tt_ativo junto_tp junto_ativo atividade_nome
  # CMP         PT              1        30    15       CMPPT30  CMPPT15     pop_total
  grid_cmp <- expand.grid(acess_cmp, atividade_cmp, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold para cada um dos modos
    mutate(tt_ativo = case_when(
      # Linhas para tt_ativo com tempos maiores do que 30 serão retiradas depois
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_ativo = paste0(acess_sigla, atividade_sigla, tt_ativo)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "PT" ~ "pop_total",
                                      atividade_sigla == "PM" ~ "pop_homens",
                                      atividade_sigla == "PW" ~ "pop_mulheres",
                                      atividade_sigla == "PB" ~ "cor_branca",
                                      atividade_sigla == "PA" ~ "cor_amarela",
                                      atividade_sigla == "PI" ~ "cor_indigena",
                                      atividade_sigla == "PN" ~ "cor_negra",
                                      atividade_sigla == "I1" ~ "idade_0a5", 
                                      atividade_sigla == "I2" ~ "idade_6a14", 
                                      atividade_sigla == "I3" ~ "idade_15a18", 
                                      atividade_sigla == "I4" ~ "idade_19a24",    
                                      atividade_sigla == "I5" ~ "idade_25a39", 
                                      atividade_sigla == "I6" ~ "idade_40a69", 
                                      atividade_sigla == "I7" ~ "idade_70"))
  
  # gerar o codigo para calcular as oportunidades
  # para ativo
  # "CMPPT15 = (sum(pop_total[which(travel_time <= 15)], na.rm = T))"
  codigo_cmp_ativo <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cmp$junto_ativo, 
            grid_cmp$atividade_nome, 
            grid_cmp$tt_ativo
    )
  )
  
  
  # gerar os nomes das variaveis
  to_make_cmp_ativo <- setNames(codigo_cmp_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_ativo))
  
  
  # aplicar a acessibilidade para os modos da cidade
  # para modos ativos
  acess_cmp <- ttmatrix[mode %in% c("bike", "walk"), 
                        lapply(to_make_cmp_ativo, function(x) eval(parse(text = x)))
                        , by = .(city, mode, destination, pico)]
  
  
  # Retirar todas as colunas com tempos 45 e 60 para os modos ativos
  acess_cmp <- acess_cmp %>% dplyr::select(-matches('[46]'))
  
  
  
  # 7) Juntar os arquivos de acess ------------------------------------------------
  
  # Juntar as acessibilidades (left_join)
  acess <- merge(acess_cma, acess_cmp,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 # como o cmp eh calculado para os destinos, o join eh para destination
                 by.y = c("city", "mode", "destination", "pico"))
  
  
  
  # Transformar para sf
  acess_sf <- 
    merge(acess, setDT(hexagonos_sf)[, .(id_hex, geometry)],
          by.x = "origin",
          by.y = "id_hex",
          all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  
  # identificar o ano
  acess_sf <- acess_sf %>% mutate(ano = ano)
  
  # 8) Salvar output --------------------------------------
  
  path_out <- sprintf("%s/acess_ideal_%s_%s_modos_ativos_%s", subfolder17, res, sigla_muni, ano)
  write_rds(acess_sf, sprintf('%s.rds', path_out))
  # write_delim(acess_sf, sprintf('%s.csv', path_out), delim = ';')
  st_write(acess_sf, sprintf('%s.gpkg', path_out), driver = 'GPKG', append = FALSE)
  
  
  # # Checar se acessibilidade mudou frente ao arquivo anterior gerado
  # acess_nao_ideal <- read_rds(sprintf('%s/acess_%s_%s_modos_ativos_%s.rds', subfolder17, res, sigla_muni, ano))
  # acess_nao_ideal <- acess_nao_ideal %>% dplyr::select(origin, city, mode, matches('CMATT')) %>% st_drop_geometry()
  # 
  # acess_ideal <- acess_sf %>% dplyr::select(origin, city, mode, matches('CMATT')) %>% st_drop_geometry()
  # 
  # comparativo <- acess_nao_ideal %>% left_join(acess_ideal, by = c('origin', 'city', 'mode'))
  # comparativo %>% filter(mode == 'walk') %>% sample_n(20)
  # comparativo %>% filter(mode == 'bike') %>% sample_n(20)
  # comparativo %>% filter(mode == 'bike' & CMATT15.x != CMATT15.y) %>% sample_n(20)
  
}

# 2. APLICAR PARA TODAS AS CIDADES --------------------------------------------------------------
# Parallel processing using future.apply
# if (future::supportsMulticore()) {
#   future::plan(future::multicore)
# } else {
#   future::plan(future::multisession)
# }
# furrr::future_walk('nat', calcular_acess_muni, ano = 2019)

walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, calcular_acess_muni, ano = 2019)


