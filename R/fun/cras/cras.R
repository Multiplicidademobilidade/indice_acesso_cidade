#> Esse script faz Download, Limpeza dos dados brutos e geolocalização dos CRAS

#> Após o download dos dados brutos, a função cras_geocode tem 6 etapas:

#. 1.) Leitura dos Dados Originais
#. 2.) Filtra os CRAS localizados nas cidades do projeto, renomeia e recodifica variáveis
#. 3.) Corrige coordenadas defeituosas: baseado no script fun/saude.R, ajusta os dados brutos para o nº
#      correto de caracteres, posição da vírgula etc, e identifica dois tipos de erros: (i) coordenadas 
#      com menos de 2 dígitos após a vírgula; (ii) CRAS diferentes na mesma lat/lon
#. 4.) Gera dois outputs: uma base intermediária com os endereços e ok e outra com os erros identificados em 3.)
#      para geolocalização
#. 5.) Geocode dos endereços com problema: faz o geocode via Google Maps;
#. 6.) Salva dois arquivos


cras_geocode <- function(ano, raw_data_folder, out_folder, run_gmaps = FALSE) {
  # 1) Ler os dados da base do SUAS baixado do site do MDS ---------------------------------
  
  if (ano == '2019') {
    # Abrir arquivo considerando encoding
    cras <- read_delim(sprintf('%s/CRAS/Censo_SUAS_2019_dados_gerais_RH_CRAS_divulgaÆo.csv', raw_data_folder),
                       delim = ';',
                       locale = locale(encoding = 'iso_8859-1'),
                       col_select = c("NU_IDENTIFICADOR","q0_1","q0_2", "q0_3","q0_4","q0_6","q0_8","q0_9","q0_10", "q0_11",
                                      "q0_12","q0_15", 'Latitude', 'Longitude', 'q39'))
    
    
  } else if (ano == '2018') {
    cras <- data.table::fread(sprintf('%s/1.CRAS/Censo_SUAS_2018_CRAS_Dados_Gerais_divulgacao.csv', raw_data_folder),
                              select = c("NU_IDENTIFICADOR","ident_0_1","ident_0_2","ident_0_3","ident_0_4","ident_0_6",
                                          "ident_0_8","ident_0_9","ident_0_10", "ident_0_11",
                                          "ident_0_12","ident_0_15", 'Latitude', 'Longitude', 'q_40'))
    
  }  else if (ano == '2017') {
    cras <- data.table::fread(sprintf('%s/Censo_SUAS_2017_CRAS/Censo SUAS 2017_CRAS_divulgacao_Base de dados.csv', raw_data_folder),
                              select = c("NºIDENTIFICADOR","ident.1.Nome","ident.2.TPLog","ident.3.Endereço","ident.4.Núm",
                                         "ident.6.Bairro","ident.8.CEP","IBGE7","ident.10.UF", "ident.11.Email",
                                         "ident.12.Tel","ident.15.DTImp", 'Latitude', 'Longitude', 'q35'))
  }
  
  # 2) Renomeia colunas, filtra cidades e recodifica variáveis ---------------------------------
  
  # renomeia colunas
  data.table::setnames(cras,
                       old = names(cras),
                       new = c("code_cras","name_suas","tp_log","logradouro","numero","bairro","cep","code_muni", 'code_uf','email',
                               "telefone","open_date",'lat_suas','lon_suas', 'cad_unico'))
  
  # filtra somente cras nos municipios do projeto
  code_munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist()
  cras <- data.table::setDT(cras, key = 'code_cras')[code_muni %in% code_munis]
  
  # traz info dos municipios para dados dos CRAS
  munis_info <- 
    as.data.frame(munis_list$munis_df[,.(name_muni,code_muni,abrev_estado)]) %>% 
    mutate(code_muni = as.character(code_muni))
  cras <- data.table::merge.data.table(cras,
                                       munis_info,
                                       all.x = TRUE,
                                       by = 'code_muni')
  # head(cras, 3)
  
  
  # Recodifica tipo de logradouro e indicador de Cadastramento no CADUnico
  
  if (ano == '2019') {
    cras[, tp_log := fcase(
      tp_log == 3, "Avenida", tp_log == 32, "Rua", tp_log == 35, "Travessa", tp_log == 12, "Estrada",
      tp_log == 2, "Área", tp_log == 1, "Alameda", tp_log == 40, "Via", tp_log == 31, "Rodovia",
      tp_log == 8, "Conjunto", tp_log == 27, "Praça", tp_log == 28, "Quadra", tp_log == 38, "Travessa", 
      tp_log == 12, "Estrada", tp_log == 21, "Loteamento", tp_log == 20, "Largo")]
    
    cras[, cad_unico := ifelse(cad_unico == 0, 'Não', 'Sim')]
  } else if (ano != '2019') {
    # se faz cadastro do cadUnico
    cras[, cad_unico := stringr::str_sub(cad_unico,1,3)]
  }
  

  # Recodifica endereços sem número
  cras[, numero := abs(as.numeric(numero))]
  cras[, numero := as.character(numero)]
  cras[, numero := gsub('^0','S/N', numero)]
  
  # Cria coluna com endereço completo
  cras[, endereco := paste(paste(tp_log, logradouro, sep = ' '), numero, sep = ', ')]
  # table(cras$endereco)
  
  # Endereco em maiusculo, email em minusculo
  cras[, endereco := stringr::str_to_upper(endereco)]
  cras[, bairro := stringr::str_to_upper(bairro)]
  
  # Remove tipo de logradouro duplicado
  cras[, endereco := gsub('RUA RUA','RUA', endereco)]
  cras[, endereco := gsub('AVENIDA AVENIDA','AVENIDA', endereco)]
  cras[, endereco := gsub('QUADRA QUADRA','QUADRA', endereco)]
  cras[, endereco := gsub('ÁREA ÁREA','ÁREA', endereco)]
  

  
  # 3) Corrige coordenadas defeituosas ------------------------------------------------------------------
  
  munis <- 
    purrr::map_dfr(as.integer(code_munis), geobr::read_municipality, year = ano) %>% 
    as_tibble() %>% 
    st_sf() %>%
    st_centroid() %>% 
    # transformar para lon-lat %>%
    sfc_as_cols()  %>%
    # quantos digitos as latitudes tem antes da virgula?
    mutate(lat_digits = sub("^-?(\\d+)[[:punct:]]{1}\\d+$", "\\1", lat)) %>%
    mutate(lat_digits = nchar(lat_digits)) %>%
    # municipio so tem 6 digitos
    # mutate(code_muni = substr(code_muni, 1,6)) %>%
    
    # transformar coluna code_muni em character
    mutate(code_muni = as.character(code_muni)) %>% 
    # selecionar so as colunas necessarias
    dplyr::select(code_muni, lat_digits)
  
  
  # criar dataframe com as coordenadas ajeitadas
  cras_fixed <- cras %>%
    # Seleciona Colunas 
    dplyr::select(code_muni, code_cras, lon_suas, lat_suas) %>% 
    # Left join com número de dígitos da latitude
    left_join(munis, by = 'code_muni') %>% # código revisto
    # left_join(munis %>% mutate(code_muni = as.integer(code_muni)), by = 'code_muni') %>% # original IPEA
    # primeiro, tirar tudo que for ponto ou virgula
    mutate(lon = gsub("(\\.|,)", "", lon_suas),
           lat = gsub("(\\.|,)", "", lat_suas)) %>%
    # tirar sinal de negativo
    mutate(lon = gsub("-", "", lon),
           lat = gsub("-", "", lat)) %>%
    # o ponto na longitude vai ser sempre depois do segundo numerico, e vai ser sempre negativo
    mutate(lon = sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lon)) %>%
    # o ponto na latitude vai depender do nchar
    mutate(lat = ifelse(lat_digits == 1, sub("(^\\d{1})(\\d+)", "-\\1\\.\\2", lat),
                        sub("(^\\d{2})(\\d+)", "-\\1\\.\\2", lat))) %>%
    # delete E+16 from coords
    mutate(lon = str_replace(lon, "E\\+\\d{2}", ""),
           lat = str_replace(lat, "E\\+\\d{2}", "")) %>%
    # delete undefined
    mutate(lon = ifelse(lon == "undefined", NA, lon),
           lat = ifelse(lat == "undefined", NA, lat)) %>%
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
  
  # Encontra coordenadas problemáticas 
  # Número de Dígitos após o ponto (menos de 3 digitos apos pontos)
  setDT(cras_fixed)
  cras_fixed[, ndigitos_lat := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lat))]
  cras_fixed[, ndigitos_lon := nchar(sub("(-\\d+)\\.(\\d+)", "\\2", lon))]
  cras_fixed[, ndigitos := pmin(ndigitos_lon, ndigitos_lat) , by = code_cras ]
  
  # Número de CRAS no mesmo ponto
  cras_fixed <- cras_fixed %>%
    group_by(lat, lon) %>%
    mutate(cras_rep = n()) %>%
    ungroup()
  
  # Indicador se geocode está ok - check == 0 significa que precisa rever geocode
  setDT(cras_fixed)[, check := ifelse(ndigitos <= 2 | cras_rep > 1, 0, 1)]
  table(cras_fixed$check)
  
  
  # 4) Merge com dataframe original, output intermediário e input geocode -------------------------------------------
  cras <- merge.data.table(cras, 
                           cras_fixed[,.(code_cras,lon,lat,check)],
                           all.x = TRUE,
                           by = 'code_cras')
  

  # 5) Geocode CRAS defeituosos --------------------------------------------
  
  # Quais linhas estão ok?
  intermediate <- cras[check == 1, .(code_cras,name_suas,email,telefone,code_muni,endereco,bairro,cep,name_muni,abrev_estado,lon,lat)]
  # Quais linhas precisam de revisão (check == 0)?
  # Estas linhas vão ser criadas sem as colunas de latlong, que virão do gmaps
  cras_wrong <- cras[check == 0, .(code_cras,name_suas,email,telefone,code_muni,endereco,bairro,cep,name_muni,abrev_estado)]
  
  
  # Criar coluna de endereço para geocode
  cras_wrong <- 
    cras_wrong %>% 
    mutate(end_geocode = sprintf('%s - %s, %s - %s, CEP %s', endereco, bairro, name_muni, abrev_estado, cep))
  
  # registrar Google API Key
  my_api <- data.table::fread("../../api.txt", header = FALSE)
  register_google(key = my_api$V1)
  
  if (run_gmaps) {
    message("Rodando gmaps, pode demorar um pouco...\n")
    
    # Fazer o geocode usando o ggmap e a coluna 'end_geocode'
    geocode_ggmap <- geocode(location = cras_wrong$end_geocode, output = "more", source = "google")
      
    # Simplificar geocode_ggmap para juntar ao cras_wrong
    geocode_ggmap <- geocode_ggmap %>% dplyr::select(c(lat, lon))
    
    # Juntar novo georreferenciamento ao cras_wrong
    cras_wrong <- cras_wrong %>% dplyr::select(-end_geocode) %>% cbind(geocode_ggmap)
     
    # Juntar tudo em um único dataframe de saída 
    cras_ano <- intermediate %>% rbind(cras_wrong)
      
  } else {
      
    # Arquivo final
    message("\nAviso:")
    message("O arquivo final exportado EXCLUI as linhas que estavam com problema no georreferenciamento.")
    message("Considere rodar o script novamente com a opção run_gmaps = TRUE.\n")
    cras_ano <- intermediate
    
    }
    
  
  # 6) Salvar arquivo final  --------------------------------------------
  write_rds(cras_ano, sprintf("%s/%s/cras_%s_geocoded.rds", out_folder, ano, ano))
    
    
  }