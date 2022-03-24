# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade - resolução 07 - ÔNIBUS

# Calcula o acesso a oportunidades para os hexágonos de resolução 07. Considera
# no cálculo somente o modo ônibus. Para resolução 08, usar script seguinte.

# carregar bibliotecas
source('fun/setup.R')


#### 1. CALCULAR ACESSIBILIDADE ÔNIBUS --------------------------------------------------------

# sigla_muni <- "nat"; ano <- 2019; res <- '07'

calcular_acess_muni <- function(sigla_muni, ano) {
  
  # Forçar resolução para 07. Para resolução 08, usar script seguinte; para 
  # outras resoluções, adaptar os scripts.
  res <- '07'
  
  # status message
  message('Trabalhando na cidade ', sigla_muni, ' no ano ', ano,  '\n')
  
  # Estrutura de pastas
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder15C <- sprintf("%s/15_r5r/03_output_ttmatrix/%s", files_folder, ano)
  subfolder16B  <- sprintf("%s/16_ttmatrix_motorizados/%s/01_onibus", files_folder, ano)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/%s", files_folder, ano)
  dir.create(subfolder17, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  
  # traz os tempos do modo ônibus em segundos, calculados pelo gmaps - versão
  # para quando os tempos resultantes estão guardados no dataframe
  # ttmatrix_onibus <-
  #   readr::read_rds(sprintf('%s/matriztp_%s_%s_%s.rds',
  #                           subfolder16B, sigla_muni, res, ano)) %>%
  #   # Padronizar nomes das colunas e ajeitar dados para rbind()
  #   dplyr::mutate(origin = id_hex,
  #                 destination = hex_dest,
  #                 travel_time = Time / 60,
  #                 mode = 'transit',
  #                 pico = 1,
  #                 city = sigla_muni,
  #                 ano = ano) %>%
  #   dplyr::select(-c(origem, destino, id_hex, pop_total, hex_dest, distancia,
  #                    oportunidades, Time, Distance, Status, ID))
  
  # traz os tempos do modo ônibus em segundos, calculados pelo gmaps - versão
  # para quando o que está guardado no dataframe é uma marcação de faixa de tempo
  ttmatrix_onibus <- 
    readr::read_rds(sprintf('%s/matriztp_%s_%s_%s.rds',
                            subfolder16B, sigla_muni, res, ano)) %>% 
    # Criar uma coluna de travel_time, que na prática para este caso 
    # significaria o tempo máximo - rotas não encontradas vaõ ficar como NA
    mutate(travel_time = case_when(T15 == '1' ~ paste0('15'),
                                   T30 == '1' ~ paste0('30'),
                                   T45 == '1' ~ paste0('45'),
                                   T60 == '1' ~ paste0('60'),
                                   T60_== '1' ~ paste0('99')),
           travel_time = as.numeric(travel_time),
           # Criar colunas de padronização da base frente aos demais modos
           mode = 'transit',
           pico = 1,
           city = sigla_muni,
           ano = ano) %>% 
    dplyr::select(-c(origem, destino, pop_total, oportunidades, distancia, 
                     status, T15, T30, T45, T60, T60_)) %>% 
    rename(origin = id_hex,
           destination = hex_dest)


  # O limite de tempo a ser observado para o cálculo das acessibilidades é
  # de 60 min - descartar todas as linhas com travel_time maior do que isso
  ttmatrix <- ttmatrix_onibus %>% filter(travel_time <= 60)



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
  #  acess_sigla- atividade_sigla - tt_sigla - tt_tp - junto_tp - atividade_nome
  #  CMA          TT                1          30      CMATT30    empregos_total
  grid_cma <- 
    expand.grid(acess_cma, atividade_cma, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold  para cada um dos modos
    mutate(tt_tp = case_when(
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60,
      # tt_sigla == 1 ~ 30,
      # tt_sigla == 2 ~ 60,
      # tt_sigla == 3 ~ 90,
      # tt_sigla == 4 ~ 120
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
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
  # para tp
  # "CMATT30 = (sum(empregos_total[which(travel_time <= 30)], na.rm = T))"
  codigo_cma_tp <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cma$junto_tp, 
            grid_cma$atividade_nome, 
            grid_cma$tt_tp
    )
  )
  
  # dar nomes às variaveis
  to_make_cma_tp <-    setNames(codigo_cma_tp,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_tp))
  
  
  # aplicar a acessibilidade para os modos da cidade
  
  # para transporte publico e carro
  acess_cma <- ttmatrix[mode %in% c("transit"),
                        lapply(to_make_cma_tp, function(x) eval(parse(text = x)))
                        , by = .(city, mode, origin, pico, quintil, decil)]
  
  
  
  
  
  # 4) Calcular acessibilidade cumulativa passiva --------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cmp <- "CMP"
  atividade_cmp <- c("PT", "PM", "PW",
                     "PB", "PA", "PI", "PN", 
                     "I1", "I2", "I3", "I4", "I5", "I6", "I7")
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  # acess_sigla atividade_sigla tt_sigla tt_tp junto_tp atividade_nome
  # CMP         PT              1        30    CMPPT30  pop_total
  grid_cmp <- expand.grid(acess_cmp, atividade_cmp, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold para cada um dos modos
    mutate(tt_tp = case_when(
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60,
      # tt_sigla == 1 ~ 30,
      # tt_sigla == 2 ~ 60,
      # tt_sigla == 3 ~ 90,
      # tt_sigla == 4 ~ 120
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
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
  # para tp 
  # "CMPPT30 = (sum(pop_total[which(travel_time <= 30)], na.rm = T))"
  codigo_cmp_tp <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cmp$junto_tp, 
            grid_cmp$atividade_nome, 
            grid_cmp$tt_tp
    )
  )
  
  
  # gerar os nomes das variaveis
  to_make_cmp_tp <- setNames(codigo_cmp_tp, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_tp))
  
  
  # aplicar a acessibilidade para os modos da cidade
  
  # para transporte publico e carro
  acess_cmp <- ttmatrix[mode %in% c("transit"),
                        lapply(to_make_cmp_tp, function(x) eval(parse(text = x)))
                        , by = .(city, mode, destination, pico)]
  
  
    # 7) Juntar os arquivos de acess ------------------------------------------------
  
  # Juntar os tres (left_join)
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
  
  path_out <- sprintf("%s/acess_%s_%s_onibus_%s", subfolder17, res, sigla_muni, ano)
  write_rds(acess_sf, sprintf('%s.rds', path_out))
  # write_delim(acess_sf, sprintf('%s.csv', path_out), delim = ';')
  st_write(acess_sf, sprintf('%s.gpkg', path_out), driver = 'GPKG', append = FALSE)
  
  # gc colletc
  # gc(TRUE)
  
  
}

# 2. APLICAR PARA TODAS AS CIDADES --------------------------------------------------------------

# Nem todos os municípios possuem dados de viagens de ônibus - vamos puxar todos
# e retirar os que não possuem
munic_sem_dados_onibus <- c('jpa', 'tsa', 'vta')
munis <- munis_list$munis_metro[ano_metro == 2019]
munis <- munis %>% filter(abrev_muni %nin% munic_sem_dados_onibus)

walk(munis$abrev_muni, calcular_acess_muni, ano = 2019)
