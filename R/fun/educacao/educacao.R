
#' A funcao 'educacao_filter':
#' 1) Lê os dados do censo escolar (que foram baixadas do site do INEP, base de escolas) e faz filtros selecionando

# Processo manual para baixar os dados (ver observações dentro da função):
# 1) Baixar os Microdados do Censo Escolar da Educacação Básica para os anos 
# desse link: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
# 2) Dezipar o arquivo completamente
# 3) Copiar o arquivo de ANEXOS/dicionario, ESCOLAS, e as MATRICULAS de todas as regioes
# 4) Copiar para a pasta do ano no indice-mobilidade_dados/05_censo_escolar/[ano]


# Aviso sobre encoding - dados para 2019 possuíam os seguintes encodings, 
# detectados com uchardet *.CSV
# ESCOLAS.CSV: IBM852
# MATRICULA_CO.CSV: ASCII
# MATRICULA_NORDESTE.CSV: ASCII
# MATRICULA_NORTE.CSV: ASCII
# MATRICULA_SUDESTE.CSV: ASCII
# MATRICULA_SUL.CSV: ASCII

# Para rodar a função 'educacao_juntar_geocode_inep' é preciso baixar os dados
# das escolas (com georreferenciamento) no site do INEP:
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/inep-data/catalogo-de-escolas
# O endereço vai redirecionar para outro, em https://inepdata.inep.gov.br/analytics/saw.dll...etc,
# onde é possível baixar as bases das escolas. Se baixar as escolas por região,
# salve e renomeie cada arquivo no padrão de 'INEP_ESCOLAS_CO.csv' e 
# 'INEP_ESCOLAS_NE.csv' etc para rodar o script. Guarde os arquivos todos na
# mesma pasta do ano em indice-mobilidade_dados/05_censo_escolar/[ano], junto
# com os demais baixados no passo anterior.



# Criar pasta geral para todos os arquivos e demais pastas - este trecho está
# fora da função devido ao processo manual de baixar e copiar os arquivos
criar_pastas_censo_educacao <- function(ano){
  files_folder <- "../../indice-mobilidade_dados"
  subfolder5 <- sprintf("%s/05_censo_escolar/%s", files_folder, ano)
  dir.create(sprintf("%s", subfolder5), recursive = TRUE, showWarnings = FALSE)
}


educacao_filter <- function(ano, download = FALSE) {
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder5 <- sprintf("%s/05_censo_escolar/%s", files_folder, ano)
  # a <- fread("../../data-raw/censo_escolar/2017/MATRICULA_CO.CSV", nrow = 10)
  
  # 1) Abrir e juntar dados de matriculas
  matriculas <- 
    lapply(list.files(sprintf("%s", subfolder5), pattern = "MATRICULA", full.names = TRUE),
           fread, select = c("CO_ENTIDADE", "TP_DEPENDENCIA", "TP_ETAPA_ENSINO", 
                             "IN_REGULAR", "IN_PROFISSIONALIZANTE")) %>%
    rbindlist()
  
  # selecionar somente matriculas regulares
  matriculas <- matriculas[IN_REGULAR == 1 | IN_PROFISSIONALIZANTE == 1]
  # selecionar somente matriculas em escolas publicas
  matriculas <- matriculas[TP_DEPENDENCIA %in% c(1, 2, 3)]
  
  # count(matriculas, TP_ETAPA_ENSINO)
  
  # categorias a serem escolhidas e re-categorizadas
  # checar arquivo Dicionário de Dados da Educação Básica 2017.excel na mesma pasta
  matriculas[,
             mat_tipo := fcase(
               TP_ETAPA_ENSINO == 1 , "mat_infantil"   , # - Educação Infantil - Creche
               TP_ETAPA_ENSINO == 2 , "mat_infantil"   , # - Educação Infantil - Pré-escola
               TP_ETAPA_ENSINO == 4 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 1ª Série
               TP_ETAPA_ENSINO == 5 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 2ª Série
               TP_ETAPA_ENSINO == 6 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 3ª Série
               TP_ETAPA_ENSINO == 7 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 4ª Série
               TP_ETAPA_ENSINO == 8 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 5ª Série
               TP_ETAPA_ENSINO == 9 , "mat_fundamental", # - Ensino Fundamental de 8 anos - 6ª Série
               TP_ETAPA_ENSINO == 10, "mat_fundamental", # - Ensino Fundamental de 8 anos - 7ª Série
               TP_ETAPA_ENSINO == 11, "mat_fundamental", # - Ensino Fundamental de 8 anos - 8ª Série
               TP_ETAPA_ENSINO == 14, "mat_fundamental", # - Ensino Fundamental de 9 anos - 1º Ano
               TP_ETAPA_ENSINO == 15, "mat_fundamental", # - Ensino Fundamental de 9 anos - 2º Ano
               TP_ETAPA_ENSINO == 16, "mat_fundamental", # - Ensino Fundamental de 9 anos - 3º Ano
               TP_ETAPA_ENSINO == 17, "mat_fundamental", # - Ensino Fundamental de 9 anos - 4º Ano
               TP_ETAPA_ENSINO == 18, "mat_fundamental", # - Ensino Fundamental de 9 anos - 5º Ano
               TP_ETAPA_ENSINO == 19, "mat_fundamental", # - Ensino Fundamental de 9 anos - 6º Ano
               TP_ETAPA_ENSINO == 20, "mat_fundamental", # - Ensino Fundamental de 9 anos - 7º Ano
               TP_ETAPA_ENSINO == 21, "mat_fundamental", # - Ensino Fundamental de 9 anos - 8º Ano
               TP_ETAPA_ENSINO == 41, "mat_fundamental", # - Ensino Fundamental de 9 anos - 9º Ano
               TP_ETAPA_ENSINO == 25, "mat_medio"      , # - Ensino Médio - 1ª Série
               TP_ETAPA_ENSINO == 26, "mat_medio"      , # - Ensino Médio - 2ª Série
               TP_ETAPA_ENSINO == 27, "mat_medio"      , # - Ensino Médio - 3ª Série
               TP_ETAPA_ENSINO == 28, "mat_medio"      , # - Ensino Médio - 4ª Série
               TP_ETAPA_ENSINO == 29, "mat_medio"      , # - Ensino Médio - Não Seriada
               TP_ETAPA_ENSINO == 30, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 1ª Série
               TP_ETAPA_ENSINO == 31, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 2ª Série
               TP_ETAPA_ENSINO == 32, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 3ª Série
               TP_ETAPA_ENSINO == 33, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) 4ª Série
               TP_ETAPA_ENSINO == 34, "mat_medio"      , # - Curso Técnico Integrado (Ensino Médio Integrado) Não Seriada
               TP_ETAPA_ENSINO == 35, "mat_medio"      , # - Ensino Médio - Normal/Magistério 1ª Série
               TP_ETAPA_ENSINO == 36, "mat_medio"      , # - Ensino Médio - Normal/Magistério 2ª Série
               TP_ETAPA_ENSINO == 37, "mat_medio"      , # - Ensino Médio - Normal/Magistério 3ª Série
               TP_ETAPA_ENSINO == 38, "mat_medio"      , # - Ensino Médio - Normal/Magistério 4ª Série
               TP_ETAPA_ENSINO == 39, "mat_medio"      , # - Curso Técnico - Concomitante
               TP_ETAPA_ENSINO == 40, "mat_medio"      , # - Curso Técnico - Subsequente
               TP_ETAPA_ENSINO == 68, NA_character_    , # - Curso FIC Concomitante
               TP_ETAPA_ENSINO == 65, NA_character_    , # - EJA - Ensino Fundamental - Projovem Urbano
               TP_ETAPA_ENSINO == 67, NA_character_    , # - Curso FIC integrado na modalidade EJA  - Nível Médio
               TP_ETAPA_ENSINO == 69, NA_character_    , # - EJA - Ensino Fundamental -  Anos iniciais
               TP_ETAPA_ENSINO == 70, NA_character_    , # - EJA - Ensino Fundamental -  Anos finais
               TP_ETAPA_ENSINO == 71, NA_character_    , # - EJA - Ensino Médio
               TP_ETAPA_ENSINO == 72, NA_character_    , # - EJA - Ensino Fundamental  - Anos iniciais e Anos finais4
               TP_ETAPA_ENSINO == 73, NA_character_    , # - Curso FIC integrado na modalidade EJA - Nível Fundamental (EJA integrada à Educação Profissional de Nível Fundamental)
               TP_ETAPA_ENSINO == 74, NA_character_    # - Curso Técnico Integrado na Modalidade EJA (EJA integrada à Educação Profissional de Nível Médio)
             )]
  
  # table(matriculas$mat_tipo, useNA = "always")
  
  # tirar NAs
  matriculas <- matriculas[!is.na(mat_tipo)]
  # agrupar por escola e tipo de matricula
  matriculas_group <- matriculas[, .(.N), by = .(CO_ENTIDADE, mat_tipo)]
  
  # transformar para formato largo
  matriculas_group_wide <- tidyr::pivot_wider(matriculas_group,
                                              names_from = mat_tipo,
                                              values_from = N,
                                              values_fill = 0)
  
  
  # 2) Ler escolas do ano do censo escolar 
  
  # essa funcao esta desativada porque os dados estao sendo baixados manualmente
  # deu problema na funcao de zip_list que sempre dava crash no R
  # if (download) {
  #   
  #   # os dados do censo escolar sao baixados daqui https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
  #   # os dados tambem podem ser baixados automaticamente para cada ano (arquivo .zip)
  #   # definir url
  #   url <- ifelse(ano == 2017, sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano),
  #                 sprintf("https://download.inep.gov.br/microdados/microdados_educacao_basica_%s.zip", ano))
  #   curl::curl_download(url,
  #                       destfile = sprintf("../../data-raw/censo_escolar/%s/censo-escolar_%s.zip", ano, ano),
  #                       quiet = FALSE)
  #   
  #   # extrar os arquivos dentro de cada zip
  #   zip_list <- zip_list(sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano))
  #   
  #   # so vamos extrair dos arquivos: a base de escolas e a base com a qtde de matriculas
  #   zip::unzip(zipfile = sprintf("https://download.inep.gov.br/microdados/micro_censo_escolar_%s.zip", ano), 
  #              files = "",
  #              exdir = sprintf("../../data-raw/censo_escolar/%s", ano))
  # }
  
  # colunas de interesse: 
  colunas <- c(c("CO_ENTIDADE", "NO_ENTIDADE", "CO_MUNICIPIO",
                 # "IN_COMUM_CRECHE", "IN_COMUM_PRE", 
                 # "IN_COMUM_FUND_AI", "IN_COMUM_FUND_AF", 
                 # "IN_COMUM_MEDIO_MEDIO", "IN_COMUM_MEDIO_NORMAL",
                 # "IN_ESP_EXCLUSIVA_CRECHE", "IN_ESP_EXCLUSIVA_PRE", 
                 # "IN_COMUM_MEDIO_INTEGRADO", "IN_PROFISSIONALIZANTE",
                 # "IN_ESP_EXCLUSIVA_FUND_AI", "IN_ESP_EXCLUSIVA_FUND_AF",
                 # "IN_ESP_EXCLUSIVA_MEDIO_MEDIO", "IN_ESP_EXCLUSIVA_MEDIO_INTEGR",
                 # "IN_ESP_EXCLUSIVA_MEDIO_NORMAL","IN_COMUM_EJA_MEDIO","IN_COMUM_EJA_PROF",
                 # "IN_ESP_EXCLUSIVA_EJA_MEDIO","IN_ESP_EXCLUSIVA_EJA_PROF","IN_COMUM_PROF",
                 # "IN_ESP_EXCLUSIVA_PROF","IN_COMUM_EJA_FUND","IN_ESP_EXCLUSIVA_EJA_FUND",
                 "IN_LOCAL_FUNC_UNID_PRISIONAL", "IN_LOCAL_FUNC_PRISIONAL_SOCIO", # escolas prisionais
                 "IN_REGULAR", "IN_PROFISSIONALIZANTE", "IN_EJA",
                 "TP_DEPENDENCIA", "TP_SITUACAO_FUNCIONAMENTO"), 
               ifelse(ano == 2017, "NU_FUNCIONARIOS", "QT_FUNCIONARIOS"))
  
  escolas <- fread(sprintf("%s/ESCOLAS.CSV", subfolder5), select = colunas)
  # rename funcionarios variable
  if (ano != 2019) {
    colnames(escolas)[ncol(escolas)] <- "NU_FUNCIONARIOS"
  }
  
  # format columns
  escolas <- janitor::clean_names(escolas)
  
  # filter municipalities
  muni_list <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist()
  escolas_munis <- escolas[co_municipio %in% muni_list]
  rm(escolas)
  
  # only public
  escolas_munis <- escolas_munis[tp_dependencia %in% c(1, 2, 3)]
  
  # only active
  escolas_munis <- escolas_munis[tp_situacao_funcionamento == 1]
  
  # selecionar somente escola com ensino regular
  escolas_munis <- escolas_munis[in_regular == 1 | in_profissionalizante == 1]
  
  # Identifica codigo das escolas priosionais
  escolas_prisionais <- subset(escolas_munis, in_local_func_unid_prisional ==1 | in_local_func_prisional_socio ==1)$co_entidade
  
  # remove escolas prisionais
  escolas_fim <- subset(escolas_munis, co_entidade %nin% escolas_prisionais)
  escolas_fim$in_local_func_unid_prisional <- NULL
  escolas_fim$in_local_func_prisional_socio <- NULL
  
  
  # 3) trazer matriculas
  
  # usando inner_join para manter apenas escolas com matriculas que nao sejam EJA
  escolas_fim_mat <- inner_join(escolas_fim,
                                matriculas_group_wide,
                                by = c("co_entidade" = "CO_ENTIDADE"),
                                sort = FALSE)
  
  # Checar se há NAs nas colunas de matrículas
  sum(is.na(escolas_fim_mat$mat_infatil))
  sum(is.na(escolas_fim_mat$mat_fundamental))
  sum(is.na(escolas_fim_mat$mat_medio))
  
  
  # 4) Selecionar variaveis e salvar ---------------
  # after 2019, variable 'nu_funcionarios' was descontinued
  if (ano %in% c(2017, 2018)) {
    escolas_fim_mat <- 
      escolas_fim_mat %>%
      mutate(ano = ano) %>%
      dplyr::select(co_entidade, ano, code_muni = co_municipio, no_entidade, 
                    mat_infantil, mat_fundamental, mat_medio, nu_funcionarios)
  } else {
    escolas_fim_mat <- 
      escolas_fim_mat %>%
      mutate(ano = ano) %>%
      dplyr::select(co_entidade, ano,  code_muni = co_municipio, no_entidade, 
                    mat_infantil, mat_fundamental, mat_medio)
  }
  
  message("Total de matriculas nivel mat_infantil: ", sum(escolas_fim_mat$mat_infantil, na.rm = TRUE))
  message("Total de matriculas nivel mat_fundamental: ", sum(escolas_fim_mat$mat_fundamental, na.rm = TRUE))
  message("Total de matriculas nivel mat_medio: ", sum(escolas_fim_mat$mat_medio, na.rm = TRUE))
  
  # 4) salvar ---------------------------
  write_rds(escolas_fim_mat, sprintf("%s/educacao_%s_filter.rds", subfolder5, ano), compress = 'gz')
}


educacao_juntar_geocode_inep <- function(ano_base) {
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder5 <- sprintf("%s/05_censo_escolar/%s", files_folder, ano)
  # a <- fread("../../data-raw/censo_escolar/2017/MATRICULA_CO.CSV", nrow = 10)
  
  # 1) Abrir todos os dados das escolas
  escolas_coords <- 
    lapply(list.files(sprintf("%s", subfolder5), pattern = "INEP_ESCOLAS", full.names = TRUE),
           fread, sep = ';', colClasses = list(character = 1:19)) %>%
    rbindlist()
  
  escolas_coords <- janitor::clean_names(escolas_coords)
  
  escolas_coords <-
    escolas_coords %>% 
    dplyr::select(co_entidade = codigo_inep, escola, endereco, uf, municipio, latitude, longitude) %>%
    mutate(co_entidade = as.numeric(co_entidade),
           latitude = as.double(latitude),
           longitude = as.double(longitude))
  
  # 2) Abrir arquivo com matrículas por escola e juntá-lo ao das coordenadas
  # censo_escolar <- lapply(sprintf("%s/%s/educacao_%s_filter.rds", subfolder5, c(2017, 2018, 2019), c(2017, 2018, 2019)), read_rds)
  censo_escolar <- read_rds(sprintf("%s/educacao_%s_filter.rds", subfolder5, ano_base))
  
  escolas_coords_out <- 
    censo_escolar %>% 
    left_join(escolas_coords, by = 'co_entidade') %>% 
    # Os nomes das escolas em 'no_entidade' estão com problema de encoding - atualizar
    # a partir dos nomes em 'escola', exceto para os casos em que a 'escola' for
    # NA, ou seja, para as poucas escolas que existem na base 'censo_escolar'
    # mas não na base escolas_coords (em 2019 são apenas 8)
    mutate(no_entidade = case_when(!is.na(escola) ~ escola,
                                   TRUE ~ no_entidade)) %>% 
    # Descartar a coluna 'escola'
    dplyr::select(-escola)
  
  
  write_rds(escolas_coords_out, sprintf("%s/educacao_%s_filter_geocoded.rds", subfolder5, ano), compress = 'gz')
}


# Faz o geocode das escolas que não possuem latlong no arquivo que vem do INEP
educacao_geocode_all <- function(ano) { 
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder5 <- sprintf("%s/05_censo_escolar/%s", files_folder, ano)
  
  # Checar se arquivo resultante já existe. Se sim, avisar e pular processamento
  out_file <- sprintf("educacao_%s_filter_geocoded_gmaps.rds", ano)
  
  if (out_file %nin% list.files(subfolder5)){
    
    # Arquivo com as escolas e os geocodes vindos do INEP
    open_file <- sprintf('%s/educacao_%s_filter_geocoded.rds', subfolder5, ano)
    educacao_geocode_raw <- read_rds(open_file) 
    
    # Filtrar somente os que não possuem dados de latlong
    educacao_to_geocode <- 
      educacao_geocode_raw %>% 
      filter(is.na(latitude)) %>% 
      # Criar coluna para geocode
      mutate(end_geocode = sprintf('%s, %s, %s', no_entidade, municipio, uf))
    
    # Fazer o geocode usando o ggmap e a coluna 'end_geocode'
    my_api <- data.table::fread("../../api.txt", header = FALSE)
    register_google(key = my_api$V1)
    geocode_ggmap <- geocode(location = educacao_to_geocode$end_geocode, output = "more", source = "google")
    
    # Simplificar o dataframe com resultados
    geocode_ggmap <- geocode_ggmap %>% dplyr::select(latitude = lat, longitude = lon)
    
    # Juntar dados das escolas com os dados de latlong
    educacao_geocodada <- 
      educacao_to_geocode %>% 
      dplyr::select(-c(latitude, longitude, end_geocode)) %>% 
      cbind(geocode_ggmap)
    
    # Juntar os dois dataframes, de escolas que já tinham geocode com as novas
    educacao_geocodada_final <- 
      educacao_geocode_raw %>% 
      filter(!is.na(latitude)) %>%
      rbind(educacao_geocodada) %>% 
      arrange(co_entidade)
    
    # Salvar arquivo resultante
    write_rds(educacao_geocodada_final, sprintf("%s/%s", subfolder5, out_file), compress = 'gz')
    # write_delim(geocode_ggmap, sprintf("%s/educacao_%s_filter_geocoded_gmaps.csv", subfolder5, ano), delim = ';')
    # write_delim(educacao_geocodada_final, sprintf("%s/educacao_%s_filter_geocoded_gmaps2.csv", subfolder5, ano), delim = ';')
    
  } else {
    message('Arquivo para o ano  ', ano, " já existe, pulando...\n")
  }
}

#' #' A funcao 'educacao_geocode' faz o geocode de escolas com coordenadas problematicas e tem como output
#' #' a base final do ano ja com as coordenadas corrigidas e pronta
#' #' Etapas:
#' #' 1) Abre as escolas do ano que foram filtradas na etapa anterior
#' #' 2) Separa somente as escolas para geocode que nao forem geocoded no ano anterior (isso so serve a partir de 2018)
#' #' 3) Identifica escolas com lat/long problematicos a partir dos quatro criterios estabelecidos
#' #' 4) Faz geocode das escolas problematicas usando google maps. Se a opcao 'run_gmaps = FALSE', vai fazer uso
#' #' dos dados que ja foram rodados no gmaps antes
#' #' 5) Recodifica escolas que estao fora da cidade: algumas escolas persistem em ficar fora da cidade, o q pode indicar
#' #' que esses estabs estao com a localizacao correta so que foram registrados numa cidade da RM
#' #' 6) Traz as coordenadas corrigidas para a base do novo ano
#' #' 7) Traz as coordenadas do ano anterior para a base do novo ano
#' 
#' educacao_geocode_ipea <- function(ano1, run_gmaps = FALSE) {
#'   # 1) Trazer as coordenadas da escolas fornecidas pelo INEP e geocodififadas pelo streetmap 
#'   escolas_coords <- fread("../../data/geocode/educacao/educacao_raw_geocoded.csv",
#'                           encoding = "UTF-8")
#'   
#'   
#'   # abrir base com as escolas do censo escolar
#'   # censo_escolar <- lapply(sprintf("%s/%s/educacao_%s_filter.rds", subfolder5, c(2017, 2018, 2019), c(2017, 2018, 2019)), read_rds)
#'   censo_escolar <- lapply(sprintf("%s/%s/educacao_%s_filter.rds", subfolder5, 2019, 2019), read_rds)
#'   censo_escolar_estabs <- censo_escolar %>% rbindlist(fill = TRUE) %>% dplyr::select(co_entidade)
#'   
#'   # filtrar somente as escolas dos nossos municipios
#'   escolas_coords <- escolas_coords %>% 
#'     filter(co_entidade %in% censo_escolar_estabs$co_entidade) %>%
#'     select(co_entidade, cidade, uf, endereco, lon_inep, lat_inep,
#'            matched_address, Status, Addr_type, Score, lon, lat) %>%
#'     setDT()
#'   
#'   escolas_coords$cidade <- iconv(escolas_coords$cidade, from = "UTF-8", to = "ASCII//TRANSLIT") 
#'   
#'   
#'   # identify engine
#'   escolas_coords[, geocode_engine := "streetmap"]
#'   
#'   
#'   # 3.1) Selecionar estabs com baixa precisao
#'   escolas_coords[, gmaps := fifelse(Status %in% c("T", "U"), TRUE,
#'                                     fifelse(Addr_type == "PointAddress", FALSE,
#'                                             fifelse(cidade %like% "Brasilia" & Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 75, FALSE,
#'                                                     fifelse(Addr_type %in% c("StreetAddress", "StreetAddressExt", "StreetName") & Score >= 90, FALSE, TRUE))))]
#'   
#'   estabs_problema <- escolas_coords[gmaps == TRUE]
#'   
#'   message("Total of estabs to go to gmaps: ", unique(estabs_problema$co_entidade) %>% length())
#'   
#'   
#'   # 3.2) Listar esses enderecos com problema
#'   enderecos <- estabs_problema %>% mutate(fim = endereco) %>% pull(fim)
#'   
#'   # 3.3) Registrar Google API Key
#'   my_api <- data.table::fread("../../data-raw/google_key.txt", header = TRUE)
#'   
#'   
#'   
#'   if (run_gmaps) {
#'     
#'     message("Running gmaps, this may take a while")
#'     
#'     register_google(key = my_api$key[1]) # rafa
#'     coordenadas_google <- lapply(X=enderecos, ggmap::geocode, output = "all")
#'     
#'     # identify list names as id_estab
#'     names(coordenadas_google) <- estabs_problema$co_entidade
#'     
#'     # save
#'     write_rds(coordenadas_google, 
#'               "../../data/acesso_oport/educacao/geocode/educacao_geocode_%s_output_google.rds")
#'     
#'   } else coordenadas_google <- read_rds("../../data/acesso_oport/educacao/geocode/educacao_geocode_%s_output_google.rds")
#'   
#'   
#'   # function to create data.frame from gmaps output
#'   create_dt <- function(x) {
#'     
#'     precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
#'                                x[["results"]][[1]][["address_components"]], 
#'                                NA)
#'     
#'     # check length from precision depth
#'     precision_depth <- ifelse(is.na(precision_depth0), NA,
#'                               ifelse(length(precision_depth0[[1]]$types) > 0,
#'                                      precision_depth0[[1]]$types[[1]], 
#'                                      NA))
#'     
#'     a <- data.table(
#'       matched_address = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
#'       # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
#'       Addr_type = precision_depth,
#'       lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
#'       lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
#'     )
#'     
#'   }
#'   
#'   # 3.5) Rodar funcao que transforma todos os estabs georef em data.table
#'   estabs_problema_geocoded <- lapply(coordenadas_google, create_dt)
#'   
#'   # 3.6) Rbind as data.table
#'   estabs_problema_geocoded_dt <- rbindlist(estabs_problema_geocoded, idcol = "co_entidade",
#'                                            use.names = TRUE)
#'   
#'   
#'   # make sure we only using the correct subset
#'   estabs_problema_geocoded_dt <- estabs_problema_geocoded_dt[co_entidade %in% estabs_problema$co_entidade]
#'   
#'   
#'   # 3.9) Identificar o tipo de problema
#'   estabs_problema_geocoded_dt[, geocode_engine := 'gmaps']
#'   
#'   # 3.10) Identificar qualidade quando o endereco nao foi encontrado
#'   estabs_problema_geocoded_dt[is.na(lon), ':='(Addr_type = "address_not_found")]
#'   
#'   
#'   # 10) Substituir as coordenadas problematicas que estao na base do streetmap ----
#'   # pelas novas coordenadas que foram corrigidas pelo gmaps
#'   
#'   # 10.1) Identificar geocode engine (essa info vai acabar sendo substituida quando
#'   # necessario)
#'   # table(rais_geocoded_filter$Addr_type, useNA = 'always')
#'   # table(rais_geocoded_filter$geocode_engine, useNA = 'always')
#'   # table(rais_geocoded_filter$type_year_input, useNA = 'always')
#'   
#'   # 10.2) Fazer a substituicao
#'   escolas_coords[, co_entidade := as.character(co_entidade)]
#'   escolas_coords[estabs_problema_geocoded_dt, on = "co_entidade",
#'                  c("Addr_type", "matched_address",  "lon", "lat", "geocode_engine") :=
#'                    list(i.Addr_type, i.matched_address, i.lon, i.lat, i.geocode_engine)]
#'   
#'   # table(escolas_coords$geocode_engine, useNA = "always")
#'   
#'   
#'   
#'   # censo <- censo_escolar[[1]]
#'   
#'   # substituir para cada ano
#'   atualizar_censoescolar <- function(censo) {
#'     
#'     
#'     censo[, co_entidade := as.character(co_entidade)]
#'     
#'     censo[escolas_coords, on = "co_entidade",
#'           c("cidade", "uf", "endereco", "lon_inep", "lat_inep", 
#'             "matched_address", "Addr_type", "Score", "lon", "lat", "geocode_engine") :=
#'             list(i.cidade, i.uf, i.endereco, i.lon_inep, i.lat_inep, 
#'                  i.matched_address, i.Addr_type, i.Score, i.lon, i.lat, i.geocode_engine)]
#'     
#'     # identificar ano
#'     ano1 <- unique(censo$ano)
#'     
#'     
#'     # 8) Salvar
#'     write_rds(censo,
#'               sprintf("../../data/acesso_oport/educacao/%s/educacao_%s_filter_geocoded_gmaps.rds", ano1, ano1), compress = 'gz')
#'     
#'     
#'     # ui <- censo %>% filter(Addr_type == "street_number")
#'     
#'     # ui %>%
#'     #   slice(800) %>%
#'     #   to_spatial() %>%
#'     #   mapview()
#'     
#'   }
#'   
#'   
#'   walk(censo_escolar,atualizar_censoescolar)
#'   
#'   
#' }




