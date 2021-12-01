
# Este script foi dispensado do fluxo de trabalho pois os dados provenientes 
# da RAIS foram georreferenciados externamente
# 
# Nos scripts originais, o IPEA teve acesso a uma base fechada da RAIS. Para
# este processo, a base pública da RAIS foi georreferenciada tendo como base
# o CEP dos estabelecimentos. Este script vai se limitar a criar uma estrutura
# de pastas similar às usadas no projeto e juntar os arquivos originais,
# renomeando o resultado para também estar de acordo com a nomenclatura
# esperada nos scripts posteriores.

# carregar bibliotecas
source('fun/setup.R')

# 1. Criar estrutura de pastas para os dados georreferenciados da RAIS
ano = 2019
files_folder <- "../../indice-mobilidade_dados"
subfolder7 <- sprintf("%s/07_rais_empregos/%s", files_folder, ano)
dir.create(subfolder7, recursive = TRUE, showWarnings = FALSE)


# 2. Abrir os arquivos georreferenciados pelo CEP
rais_files_folder <- sprintf("%s/00_Originais/RAIS2019", files_folder)
lote1 <- read_sf(sprintf('%s/Lote1/Lote1.gpkg', rais_files_folder))
lote2 <- read_sf(sprintf('%s/Lote2/Lote2.gpkg', rais_files_folder)) %>% rename(Cod_Proc = Codigo_Processamento)

# Descrição da coluna Codigo_Processamento
# 
# 1 - Dentro do Município e aderente ao DNE
# 2 - Dentro do Município sem aderência ao DNE
# 3 - Para registros inconsistentes, foi executada uma segunda geocodificação a 
# partir do endereço relacionado ao CEP no DNE. Esses registros foram inseridos 
# na base se o novo geocoder atendiam às validações do slide anterior.
# 4 - Sem geocodificação. Muitos desses registros não têm aderência ao DNE. Também 
# foram encontrados muitos registros em rodovia.

# Juntar os arquivos em um só e renomear colunas
rais <- 
  rbind(lote1, lote2) %>% 
  # Novos nomes de colunas de acordo, sempre que possível, com o script original
  # do IPEA
  dplyr::select(codemun             = Município,
                clas_cnae20         = F0_Subclasse, # Subclasse de Atividade Econômica, segundo classificação CNAE - versão 2.0
                subsetor_ibge       = IBGE_Subsetor, # Subsetor IBGE 80 do estabelecimento 
                nat_jur             = Natureza_Jurídica,
                ind_simples         = Ind_Simples, # Indicador de optante pelo Simples
                qtd_vinculos_ativos = Qtd_Vínculos_Ativos, # Quantidade de vínculos ativos
                cep_estab           = CEP_Estab,
                comp_cep            = Comp_Cep,
                cod_proc            = Cod_Proc, # Código do geoprocessamento dos CEPS - ver descrição acima
                lon                 = Long,
                lat                 = Lat,
                shape               = Shape) # Geometria, igual às colunas de latlong


# Não será preciso filtrar pelos municípios do projeto pois o georreferenciamento
# pelo CEP já considerou apenas as cidades selecionadas

# Manter somente registros com dados de latlong
rais <- rais %>% filter(cod_proc != '4')

# Salvar em disco
write_rds(rais, sprintf("%s/rais_%s.rds", subfolder7, ano), compress = 'gz')


# ---------------------------------------
# SCRIPT ORIGINAL IPEA
# ---------------------------------------
# Tarefas deste script:
# Aplicacao de filtros para selecionar o ano de interesse
# Georreferenciamento usando o software Galileo
# Verificacao da qualidade do georreferenciamento

# Tarefas do script auxiliar (.R/fun/empregos/empregos.R)
# Aplica filtros com relacao a natureza juridica
# Aplica filtros e classifica de acordo com o nivel de escolaridade (baixo, medio, alto)
# Tratamento de outliers (CNAE e numero de vinculos)
# Incorporacao dos dados georreferenciados 
# Incorporacao dos dados de escolas

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ###### 0.1.6 Limpeza e Geocode dados da RAIS
# 
# # carregar bibliotecas
# source('./R/fun/setup.R')
# 
# # carregar funcoes
# purrr::walk(dir('./R/fun/empregos', full.names = TRUE), source)
# 
# # Aplicar funcoes para o ano de 2017 -------------
# 
# # 0) Save raw data with columns we use in the municipalities of the project
# rais_filter_raw_data(2017)
# 
# # 0.1) Filter raw workers data
# rais_filter_pessoas(2017)
# 
# # 1) Categorizar trabalhadores por grau de instrucao
# rais_categorize_inst(2017)
# 
# # 2) Tratar os outliers 
# rais_treat_outliers(2017)
# 
# # 3) Realizar e trazer geocode dos estabelecimentos
# # realizar geocode
# # rais_clean_estabs_raw(2017)
# # rais_export_data_to_galileo(2017)
# 
# ## Run Galileo to geocode the data before moving on to the next functions
# 
# # rais_check_new_estabs(2017)
# # rais_gmaps_geocode(2017, run_gmaps = FALSE)
# # trazer geocode
# rais_bring_geocode(2017)
# 
# 
# # 5) Selecionar somente as obsservacoes com boa qualidade de geocode
# source("R/fun/filter_geocode.R")
# geocode_filter(ano = 2017, "rais")
# 
# 
# 
# # Aplicar funcoes para o ano de 2018 -------------
# 
# # 0) Save raw data with columns we use
# rais_filter_raw_data(2018)
# 
# # 0.1) Filter raw trabalhadores data
# rais_filter_pessoas(2018)
# 
# # 1) Categorizar trabalhadores por grau de instrucao
# rais_categorize_inst(2018)
# 
# # 2) Tratar os outliers 
# rais_treat_outliers(2018)
# 
# # 3) Realizar e trazer geocode dos estabelecimentos
# # realizar geocode
# rais_clean_estabs_raw(2018)
# rais_export_data_to_galileo(2018)
# 
# ## Run Galileo to geocode the data before moving on to the next functions
# 
# rais_check_new_estabs(2018)
# rais_gmaps_geocode(2018, run_gmaps = FALSE)
# # trazer geocode
# rais_bring_geocode(2018)
# 
# # 5) Selecionar somente as obsservacoes com boa qualidade de geocode
# source("R/fun/filter_geocode.R")
# geocode_filter(ano = 2018, "rais")
# 
# 
# 
# 
# # Aplicar funcoes para o ano de 2019 -------------
# 
# # 0) Save raw data with columns we use
# rais_filter_raw_data(2019)
# 
# # 0.1) Filter raw trabalhadores data
# rais_filter_pessoas(2019)
# 
# # 1) Categorizar trabalhadores por grau de instrucao
# rais_categorize_inst(2019)
# 
# # 2) Tratar os outliers 
# rais_treat_outliers(2019)
# 
# # 3) Realizar e trazer geocode dos estabelecimentos
# # realizar geocode
# rais_clean_estabs_raw(2019)
# rais_export_data_to_galileo(2019)
# 
# ## Run Galileo to geocode the data before moving on to the next functions
# 
# rais_check_new_estabs(2019)
# rais_gmaps_geocode(2019, run_gmaps = FALSE)
# # trazer geocode
# rais_bring_geocode(2019)
# 
# 
# # 5) Selecionar somente as obsservacoes com boa qualidade de geocode
# source("R/fun/filter_geocode.R")
# geocode_filter(ano = 2019, "rais")
# 
