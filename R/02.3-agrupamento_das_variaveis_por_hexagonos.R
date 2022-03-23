# Agrega informacoes demograficas e de uso do solo nos hexágonos

# Carregar bibliotecas
sf::sf_use_s2(FALSE)
source('fun/setup.R')

#' A funcao `agrupar_variaveis_hex` agrega as variáveis de emprego, educação, 
#' saúde e demográficas das grades estísticas para os hexágonos de cada cidade

agrupar_variaveis_hex <- function(ano, munis = "all", res = '08') {
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder5 <- sprintf("%s/05_censo_escolar/%s", files_folder, ano)
  subfolder6 <- sprintf("%s/06_cnes_saude/%s", files_folder, ano)
  subfolder7 <- sprintf("%s/07_rais_empregos/%s", files_folder, ano)
  subfolder8 <- sprintf("%s/08_cras_assist_social/%s", files_folder, ano)
  subfolder12 <- sprintf("%s/12_hex_municipios/%s", files_folder, ano)
  subfolder13 <- sprintf("%s/13_grade_municipio_com_renda_cor/%s", files_folder, ano)
  subfolder14 <- sprintf("%s/14_hex_agregados/%s", files_folder, ano)
  dir.create(subfolder14, recursive = TRUE, showWarnings = FALSE)
  
  
  # 1) Abrir arquivos com as oportunidades -------------------------------------
  
  # 1.1) Saúde
  cnes_data <- readr::read_rds(sprintf("%s/saude_%s_filter_geocode_revisto_gmaps_duas_etapas.rds", subfolder6, ano)) 
  
  # selecionar colunas
  cnes_data <- cnes_data %>% dplyr::select(cnes = CO_CNES, 
                                           code_muni = IBGE,
                                           health_low, health_med, health_high,
                                           lon = NU_LONGITUDE, 
                                           lat = NU_LATITUDE)
  
  # remover linhas sem latlong
  cnes_data <- cnes_data %>% filter(!is.na(lat))
  
  
  # 1.2) Escolas
  escolas <- read_rds(sprintf("%s/educacao_%s_filter_geocoded_gmaps.rds", subfolder5, ano))
  
  # selecionar colunas
  escolas <- escolas %>% dplyr::select(co_entidade, code_muni,
                                       mat_infantil, mat_fundamental, mat_medio,
                                       lon = longitude, lat = latitude)
  
  # remover linhas sem latlong
  escolas <- escolas %>% filter(!is.na(lat))
  
  
  # 1.3) Empregos
  empregos <- readr::read_rds(sprintf("%s/rais_%s.rds", subfolder7, ano))
  
  # selecionar colunas
  empregos <- empregos %>% dplyr::select(code_muni = codemun, 
                                         # id_estab, 
                                         # baixo, 
                                         # medio, 
                                         # alto, 
                                         vinc_ativos = qtd_vinculos_ativos, # Nova coluna
                                         lon, 
                                         lat)
  
  # remover linhas sem latlong
  empregos <- empregos %>% filter(!is.na(lat))
  
  
  
  # 1.3) CRAS
  # cras <- read_rds(sprintf("../../data/acesso_oport/cras/cras_%s_geocoded.rds", ano))
  cras <- read_rds(sprintf("%s/cras_%s_geocoded.rds", subfolder8, ano))
  
  # selecionar colunas
  cras <- cras %>% dplyr::select(code_cras, code_muni, lon, lat)
  
  # remover linhas sem latlong
  cras <- cras %>% filter(!is.na(lat))
  
  
  #' A funcao `agrupar_variaveis` agrupa as variáveis de uso do solo determinadas 
  #' acima nos hexágonos de cada um dos municípios. Tambám traz as informações 
  #' demográficas da grade do IBGE
  
  agrupar_variaveis <- function(sigla_muni) { 
    
    # status message
    message('Trabalhando na cidade ', sigla_muni, '\n')
    
    # Resoluções disponíveis
    # res <- c("08", "09")
    # res <- c("08")
    
    # Pegar endereco das grades e hexágonos em todas resoluções
    grade_file <- sprintf("%s/grade_renda_cor_%s_%s.rds", subfolder13, sigla_muni, ano)
    
    # Gerar centroide da grade de cada município
    centroide_pop <- 
      readr::read_rds(grade_file) %>%
      dplyr::select(id_grade, pop_total, pop_mulheres, pop_homens,
                    renda,
                    cor_branca, cor_amarela, cor_indigena, cor_negra,
                    idade_0a5,
                    idade_6a14,
                    idade_15a18,
                    idade_19a24,
                    idade_25a39,
                    idade_40a69,
                    idade_70   ) %>% 
      st_centroid()
    
    # Qual o codigo do municipio em questao?
    cod_mun_ok <- 
      munis_list$munis_metro[abrev_muni == sigla_muni & ano_metro == ano]$code_muni %>% 
      unlist()
    
    # Filtrar somente as atividades referentes a cada municipio e transforma em sf
    # para rais 2017
    empregos_filtrado <- 
      empregos %>% 
      filter(str_starts(code_muni, str_sub(cod_mun_ok, start = 1, end = 6))) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # escolas
    escolas_filtrado <- 
      escolas %>% 
      filter(code_muni == cod_mun_ok) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # saude
    cnes_filtrado <- 
      cnes_data %>% 
      filter(str_starts(code_muni, str_sub(cod_mun_ok, start = 1, end = 6))) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # cras
    cras_filtrado <- 
      cras %>% 
      filter(code_muni == cod_mun_ok) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    
    
    # FUNCAO PARA REALIZAR PARA TODAS AS RESOLUCOES
    
    por_resolucao <- function(muni_res) {
      # muni_res <- '09'
      # muni_res <- '08'
      
      # endereco do hexagono na resolucao
      hexf <- sprintf("%s/hex_%s_%s_%s.rds", subfolder12, sigla_muni, muni_res, ano)
      
      # Ler arquivo de hexagono  
      hex_muni <- readr::read_rds(hexf)
      
      # Agrupar populacao, cor e renda
      # join espacial 
      hex_pop <- hex_muni %>% st_join(centroide_pop)
      
      
      ### a. substituir CENTROIDE pela proporcao de intersecao
      ### b. arredondamento %>%
      ###  mutate_at(vars(matches("pop|renda|moradores|cor|idade")), round)
      
      # Summarize
      hex_pop <- setDT(hex_pop)[, .(cor_branca   = as.numeric(sum(round(cor_branca,0), na.rm = TRUE)),
                                    cor_amarela  = as.numeric(sum(round(cor_amarela,0), na.rm = TRUE)),
                                    cor_indigena = as.numeric(sum(round(cor_indigena,0), na.rm = TRUE)),
                                    cor_negra    = as.numeric(sum(round(cor_negra,0), na.rm = TRUE)),
                                    # age variables
                                    idade_0a5   = as.numeric(sum(idade_0a5   , na.rm = TRUE)),
                                    idade_6a14 =  as.numeric(sum(idade_6a14 , na.rm = TRUE)),
                                    idade_15a18 = as.numeric(sum(idade_15a18 , na.rm = TRUE)),
                                    idade_19a24 = as.numeric(sum(idade_19a24 , na.rm = TRUE)),
                                    idade_25a39 = as.numeric(sum(idade_25a39 , na.rm = TRUE)),
                                    idade_40a69 = as.numeric(sum(idade_40a69 , na.rm = TRUE)),
                                    idade_70 =    as.numeric(sum(idade_70 , na.rm = TRUE)),
                                    # total pop and income 
                                    pop_total    = as.numeric(sum(round(pop_total,0), na.rm = TRUE)),
                                    pop_homens   = as.numeric(sum(round(pop_homens,0), na.rm = TRUE)),
                                    pop_mulheres = as.numeric(sum(round(pop_mulheres,0), na.rm = TRUE)),
                                    renda_total  = as.numeric(sum(renda, na.rm = TRUE))), 
                                by = id_hex ]
      
      # Calcular quintil e decil de renda
      # calcula renda per capita de cada hexagono
      hex_pop[, renda_capita := renda_total / pop_total]
      
      # calcular quintis ponderados pela populacao
      deciles  <- Hmisc::wtd.quantile(hex_pop$renda_capita, weights = hex_pop$pop_total, 
                                      probs = c( seq(0 , 1 , 0.1) ), 
                                      type = c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                      normwt = FALSE, na.rm = T)
      
      quintiles  <- Hmisc::wtd.quantile(hex_pop$renda_capita, weights = hex_pop$pop_total, 
                                        probs = c( seq(0 , 1 , 0.2) ), 
                                        type = c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                        normwt = FALSE, na.rm = T)
      
      # classificar cada hexagono em cada quintil de renda
      hex_pop[, quintil := findInterval(renda_capita , quintiles[ -length(quintiles) ] ) ]
      hex_pop[, decil := findInterval(renda_capita , deciles[ -length(deciles) ] ) ]
      
      # check if pop size in each decile are roughly equal
      hex_pop[, .(po_in_quintil = sum(pop_total, na.rm = T)), by = quintil]
      hex_pop[, .(po_in_decile  = sum(pop_total, na.rm = T)), by = decil]
      
      # # remove NAs
      # hex_pop <- hex_pop[ !is.na(decil)]
      # hex_pop <- hex_pop[ !is.na(quintil)]
      
      # Agrupar empregos
      # join espacial 
      hex_rais <- hex_muni %>% st_join(empregos_filtrado) %>% setDT()
      
      # Summarize
      hex_rais <- hex_rais[, .(# empregos_baixa = sum(baixo, na.rm = TRUE),
                               # empregos_media = sum(medio, na.rm = TRUE),
                               # empregos_alta  = sum(alto, na.rm = TRUE),
                               # empregos_total = sum(alto, medio, baixo, na.rm = TRUE)
                               empregos_total = sum(vinc_ativos, na.rm = TRUE)), 
                           by = id_hex ]
      
      
      # agrupar saúde
      # join espacial 
      cnes_filtrado <- sf::st_transform(cnes_filtrado, sf::st_crs(hex_muni)) # mesma projecao geografica
      hex_saude <- hex_muni %>% st_join(cnes_filtrado) %>% setDT()
      
      hex_saude[, saude_total := ifelse( is.na(cnes), 0, 1) ]
      
      # Summarize
      hex_saude <- hex_saude[, .(saude_total = sum(saude_total, na.rm = T),
                                 saude_baixa = sum(health_low, na.rm = T),
                                 saude_media = sum(health_med, na.rm = T),
                                 saude_alta  = sum(health_high, na.rm = T)),
                             by = id_hex ]
      
      
      
      
      # agrupar educacao
      # join espacial 
      hex_escolas <- hex_muni %>% st_join(escolas_filtrado) %>% setDT()
      hex_escolas <- hex_escolas[!is.na(co_entidade)]
      
      # Summarize
      hex_escolas[, ':='(edu_infantil    = fifelse(mat_infantil    == 0, 0, 1),
                         edu_fundamental = fifelse(mat_fundamental == 0, 0, 1),
                         edu_medio       = fifelse(mat_medio == 0, 0, 1))]
      
      hex_escolas <- hex_escolas[, .(edu_infantil      = sum(edu_infantil, na.rm = TRUE),
                                     edu_fundamental = sum(edu_fundamental, na.rm = TRUE),
                                     edu_medio       = sum(edu_medio, na.rm = TRUE),
                                     edu_total       = .N,
                                     mat_infantil    = sum(mat_infantil, na.rm = T),
                                     mat_fundamental = sum(mat_fundamental, na.rm = T),
                                     mat_medio       = sum(mat_medio, na.rm = T),
                                     mat_total       = sum(mat_infantil, mat_fundamental, mat_medio, na.rm = TRUE)),
                                 by = id_hex]
      
      
      # summary(hex_escolas$edu_total)
      
      # agrupar cras
      hex_cras <- hex_muni %>% st_join(cras_filtrado) %>% setDT()
      hex_cras[, cras_total := ifelse( is.na(code_cras), 0, 1) ]
      
      # summarise
      hex_cras <- hex_cras[, .(cras_total = sum(cras_total, na.rm = TRUE)),
                           by = id_hex]
      
      # Junta todos os dados agrupados por hexagonos
      hex_muni_fim <- left_join(hex_muni, hex_pop) %>%
        left_join(hex_rais) %>%
        left_join(hex_saude) %>%
        left_join(hex_escolas) %>%
        left_join(hex_cras)
      
      # substitui NAs por zeros
      hex_muni_fim[is.na(hex_muni_fim)] <- 0
      
      # adiciona sigla do municipio
      hex_muni_fim$muni <- sigla_muni
      
      
      # Salva grade de hexagonos com todas informacoes de uso do soloe
      dir_output <- sprintf("%s/hex_agregado_%s_%s_%s.rds", subfolder14, sigla_muni, muni_res, ano)
      readr::write_rds(hex_muni_fim, dir_output)
      
    }
    
    # Aplicar funcao para cada resolucao
    walk(res, por_resolucao)
    
  }
  
  # Aplica funcao para cada municipio
  if (munis == "all") {
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)
  
  walk(x, agrupar_variaveis)
  
  
}



# Aplicar função
# agrupar_variaveis_hex(ano = 2019, munis = 'vta')
agrupar_variaveis_hex(ano = 2019, munis = 'all', res = '08') # res pode ser uma lista: c('07', '08')




#### Salvar shapes  ------------
salvar_shapes_agregados <- function(res, ano="2019"){ # especifica resolucao e ano
  load_folder <- sprintf("../../indice-mobilidade_dados/14_hex_agregados/%s", ano)
  save_folder <- sprintf("%s/shapes", load_folder)
  
  # Cria pasta se nao existir
  if ("shapes" %nin% list.dirs(load_folder, recursive = FALSE, full.names = FALSE)) {
    dir.create(save_folder)
  }
  
  # Procura os arquivos que atendem o especificado
  files_list <- list.files(load_folder, pattern = sprintf("\\w{3}_%s", res)) 
  
  for (arquivo in files_list) {
    shape_name <- str_extract(arquivo, sprintf("\\w{3}_%s", res)) # le o nome
    aux <- readRDS(sprintf("%s/%s", load_folder, arquivo)) # le o arquivo
    st_write(aux, sprintf("%s/hex_agregado_%s_%s.gpkg", save_folder, shape_name, ano), driver = "GPKG", append = FALSE) # salva
    rm(aux)
  }
}

# Aplicar função
salvar_shapes_agregados(res = "08")