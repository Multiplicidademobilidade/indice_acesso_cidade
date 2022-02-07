# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.4.1 Calcular acessibilidade - resolução 07

# Calcula o acesso a oportunidades para os hexágonos de resolução 07. Considera
# no cálculo tanto os modos ativos quanto motorizados. Para resolução 08, usar
# o script seguinte.

# carregar bibliotecas
source('fun/setup.R')


#### 1. CALCULAR ACESSIBILIDADE --------------------------------------------------------------

# sigla_muni <- "nat"; ano <- 2019; res <- '07'


calcular_acess_muni <- function(sigla_muni, ano) {
  
  # Forçar resolução para 07. Para resolução 08, usar script seguinte; para 
  # outras resoluções, adaptar os scripts.
  res <- '07'
  
  # status message
  message('Trabalhando na cidade ', sigla_muni, ' no ano ', ano,  '\n')
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  subfolder15C <- sprintf("%s/15_otp/03_output_ttmatrix/%s", files_folder, ano)
  subfolder16B  <- sprintf("%s/16_ttmatrix_motorizados/%s/01_onibus", files_folder, ano)
  subfolder16C  <- sprintf("%s/16_ttmatrix_motorizados/%s/02_automovel", files_folder, ano)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/%s", files_folder, ano)
  dir.create(subfolder17, recursive = TRUE, showWarnings = FALSE)
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  # traz os tempos dos modos ativos em minutos, calculados pelo r5r - o arquivo
  # inclui os tempos para o modo carro calculados pelo 5r5. Estes tempos de
  # automóvel serão usados caso haja tempos NA na base fornecida pela 99 
  # ttmatrix_ativos <- readr::read_rds(sprintf('%s/ttmatrix_fix_%s_%s_%s.rds',
  #                                            subfolder15D, sigla_muni, res, ano))
  ttmatrix_ativos <- readr::read_delim(sprintf('%s/ttmatrix_%s_%s_%s_r5.csv',
                                             subfolder15C, sigla_muni, res, ano))
  
  # Separar matrizes de modos ativos e de automóvel
  ttmatrix_carro_r5r <- ttmatrix_ativos %>% filter(mode == 'car_r5r')
  ttmatrix_ativos    <- ttmatrix_ativos %>% filter(mode != 'car_r5r')
  
  
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
    
  
  
  # traz os tempos médios do modo carro, calculados pela 99 a partir da média 
  # de tempo das viagens iniciadas às 6h, 7h e 8h 
  ttmatrix_carro <- 
    readr::read_delim(sprintf('%s/ttmatrix_car_%s_%s_%s.csv',
                              subfolder16C, sigla_muni, res, ano), delim = ';') %>% 
    # Padronizar nomes das colunas e ajeitar dados para rbind()
    mutate(origin = id_origem,
           destination = id_destino,
           travel_time_99 = dur_minutes,
           mode = 'car',
           pico = 1,
           city = sigla_muni,
           ano = ano) %>% 
    dplyr::select(-c(id_origem, id_destino, dur_minutes)) %>% 
    # Criar coluna de id para comparar com ttmatrix_carro_r5r
    mutate(id = str_c(origin, destination, sep = '-'))
    
  # Ajustar ttmatrix_carro_r5r
  ttmatrix_carro_r5r <- 
    ttmatrix_carro_r5r %>% 
    # Criar coluna de id em ttmatrix_carro_r5r para comparar com ttmatrix_carro
    mutate(id = str_c(origin, destination, sep = '-')) %>% 
    # Filtrar somente linhas que também estão em ttmatrix_carro
    filter(.$id %in% ttmatrix_carro$id) %>% 
    # Simplificar dataframe para join
    dplyr::select(id, travel_time_r5r = travel_time)
  
  # Juntar tempos calculados pela 99 e pelo r5r
  ttmatrix_carro <- ttmatrix_carro %>% left_join(ttmatrix_carro_r5r, by = 'id') %>% dplyr::select(-id)
  
  
  
  
  # Apagar
  # ttmatrix_carro_r5r %>% group_by(id) %>% tally() %>% filter(n>1)
  # ttmatrix_carro <- ttmatrix_carro %>% mutate(travel_time_99 = case_when(origin!='87818a592ffffff' & destination != '878199965ffffff' ~ travel_time_99))
  
  # Checar diferença entre os tempos calculados
  # ttmatrix_carro %>% mutate(dif = abs(travel_time_r5r - travel_time_99))
  
  # Criar coluna de travel_time: onde tem tempo calculado pela 99, é este tempo;
  # onde não tem, o tempo é o calculado pelo r5r
  ttmatrix_carro <- 
    ttmatrix_carro %>% 
    mutate(travel_time = case_when(is.na(travel_time_99) ~ travel_time_r5r,
                                   TRUE ~ travel_time_99),
           .after = 'destination')
  
  # Simplificar dataframe
  ttmatrix_carro <- ttmatrix_carro %>% dplyr::select(-c(travel_time_r5r, travel_time_99))
  
  
  
  
  
  # juntar todas as matrizes de tempo
  ttmatrix <- ttmatrix_ativos %>% rbind(ttmatrix_onibus) %>% rbind(ttmatrix_carro)
  
  
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
    mutate(tt_ativo = case_when(
      # Linhas para tt_ativo com tempos maiores do que 30 serão retiradas depois
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
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
  # para tp
  # "CMATT30 = (sum(empregos_total[which(travel_time <= 30)], na.rm = T))"
  codigo_cma_tp <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cma$junto_tp, 
            grid_cma$atividade_nome, 
            grid_cma$tt_tp
    )
  )
  
  
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
  to_make_cma_tp <-    setNames(codigo_cma_tp,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_tp))
  to_make_cma_ativo <- setNames(codigo_cma_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_ativo))
  
  
  # aplicar a acessibilidade para os modos da cidade
  
  # para transporte publico e carro
  acess_cma_tp <- ttmatrix[mode %in% c("transit", "car"),
                           lapply(to_make_cma_tp, function(x) eval(parse(text = x)))
                           , by=.(city, mode, origin, pico, quintil, decil)]
  
  # para modos ativos
  acess_cma_ativo <- ttmatrix[mode %in% c("bike", "walk"), 
                              lapply(to_make_cma_ativo, function(x) eval(parse(text = x)))
                              , by=.(city, mode, origin, pico, quintil, decil)]
  
  
  # Transformar todos os valores das colunas 45 e 60 para NA nos modos ativos
  acess_cma_ativo <- acess_cma_ativo %>% mutate(across(matches('[46]'), ~replace(., is.numeric(.), NA)))
  
  # juntar os cma
  acess_cma <- rbind(acess_cma_tp, acess_cma_ativo, fill = TRUE)
  
  
  
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
    mutate(tt_ativo = case_when(
      # Linhas para tt_ativo com tempos maiores do que 30 serão retiradas depois
      tt_sigla == 1 ~ 15,
      tt_sigla == 2 ~ 30,
      tt_sigla == 3 ~ 45,
      tt_sigla == 4 ~ 60
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
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
  # para tp 
  # "CMPPT30 = (sum(pop_total[which(travel_time <= 30)], na.rm = T))"
  codigo_cmp_tp <- c(
    
    sprintf("%s = (sum(%s[which(travel_time <= %s)], na.rm = T))", 
            grid_cmp$junto_tp, 
            grid_cmp$atividade_nome, 
            grid_cmp$tt_tp
    )
  )
  
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
  to_make_cmp_tp <- setNames(codigo_cmp_tp, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_tp))
  to_make_cmp_ativo <- setNames(codigo_cmp_ativo, sub('^([[:alnum:]]*) =.*', '\\1', codigo_cmp_ativo))
  
  
  # aplicar a acessibilidade para os modos da cidade
  
  # para transporte publico e carro
  acess_cmp_tp <- ttmatrix[mode %in% c("transit", "car"),
                           lapply(to_make_cmp_tp, function(x) eval(parse(text = x)))
                           , by=.(city, mode, destination, pico)]
  
  # para modos ativos
  acess_cmp_ativo <- ttmatrix[mode %in% c("bike", "walk"), 
                              lapply(to_make_cmp_ativo, function(x) eval(parse(text = x)))
                              , by=.(city, mode, destination, pico)]
  
  
  # Transformar todos os valores das colunas 45 e 60 para NA nos modos ativos
  acess_cmp_ativo <- acess_cmp_ativo %>% mutate(across(matches('[46]'), ~replace(., is.numeric(.), NA)))
  
  # juntar os cmp
  acess_cmp <- rbind(acess_cmp_tp, acess_cmp_ativo, fill = TRUE)  
  
  
  
  # # 5) Calcular acessibilidade tempo minimo ---------------
  # # (aqui eh feito junto para os dois modos)
  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # 
  # acess_tmi <- "TMI"
  # atividade_tmi <- c("ST", "SB", "SM", "SA", "ET", "EI", "EF", "EM", "CT")
  # 
  # grid_tmi <- 
  #   expand.grid(acess_tmi, atividade_tmi, stringsAsFactors = FALSE) %>%
  #   rename(acess_sigla = Var1, atividade_sigla = Var2) %>%
  #   mutate(junto = paste0(acess_sigla, atividade_sigla)) %>%
  #   mutate(atividade_nome = case_when(atividade_sigla == "ST" ~ "saude_total",
  #                                     atividade_sigla == "SB" ~ "saude_baixa",
  #                                     atividade_sigla == "SM" ~ "saude_media",
  #                                     atividade_sigla == "SA" ~ "saude_alta",
  #                                     atividade_sigla == "ET" ~ "edu_total",
  #                                     atividade_sigla == "EI" ~ "edu_infantil",
  #                                     atividade_sigla == "EF" ~ "edu_fundamental",
  #                                     atividade_sigla == "EM" ~ "edu_medio",
  #                                     atividade_sigla == "EI" ~ "edu_infantil",
  #                                     atividade_sigla == "CT" ~ "cras_total"))
  # 
  # 
  # # gerar o codigo para calcular as oportunidades
  # # "TMIST = min(travel_time[which(saude_total >= 1)])"
  # codigo_tmi <- sprintf("%s = min(travel_time[which(%s >= 1)])", 
  #                       grid_tmi$junto, 
  #                       grid_tmi$atividade_nome)
  # 
  # 
  # # gerar os nomes das variaveis
  # to_make_tmi <- setNames(codigo_tmi, sub('^([[:alnum:]]*) =.*', '\\1', codigo_tmi))
  # 
  # 
  # # calcular acessibilidade
  # acess_tmi <- ttmatrix[, lapply(to_make_tmi, function(x) eval(parse(text = x)))
  #                       , by=.(city, mode, origin, pico)]
  # 
  # 
  # # hexagonos_sf <- st_sf(hexagonos_sf)
  # # 
  # # ggplot() + geom_sf(data=hexagonos_sf ,fill='gray') +
  # #  geom_sf(data=subset(hexagonos_sf, saude_media>0) , aes(fill=saude_media))
  # # 
  # # ggplot() + geom_sf(data=hexagonos_sf ,fill='gray') +
  # #   geom_sf(data=subset(hexagonos_sf, saude_baixa>0) , aes(fill=saude_baixa))
  # # 
  # # 
  
  # 7) Juntar os arquivos de acess ------------------------------------------------
  
  # Juntar os tres (left_join)
  acess <- merge(acess_cma, acess_cmp,
                 all.x = TRUE,
                 by.x = c("city", "mode", "origin", "pico"),
                 # como o cmp eh calculado para os destinos, o join eh para destination
                 by.y = c("city", "mode", "destination", "pico"))
  
  # acess <- merge(acess, acess_tmi,
  #                all.x = TRUE,
  #                by.x = c("city", "mode", "origin", "pico"),
  #                by.y = c("city", "mode", "origin", "pico"))
  
  
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
  
  path_out <- sprintf("%s/acess_%s_%s_%s", subfolder17, sigla_muni, res, ano)
  write_rds(acess_sf, sprintf('%s.rds', path_out))
  write_delim(acess_sf, sprintf('%s.csv', path_out), delim = ';')
  st_write(acess_sf, sprintf('%s.gpkg', path_out), driver = 'GPKG', append = FALSE)
  
  # gc colletc
  # gc(TRUE)
  
  
}

# 2. APLICAR PARA TODAS AS CIDADES --------------------------------------------------------------
# Parallel processing using future.apply
if (future::supportsMulticore()) {
  future::plan(future::multicore)
} else {
  future::plan(future::multisession)
}

furrr::future_walk(munis_list$munis_metro[ano_metro == 2019]$abrev_muni, calcular_acess_muni, ano = 2019)
furrr::future_walk('nat', calcular_acess_muni, ano = 2019)