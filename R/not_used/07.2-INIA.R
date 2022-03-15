library(tidyverse)

# ----- PARTE 1: CALCULO DO INIA -----

# Indicadores

# 1. Integracao com TP
# Comp_1_1: Componente integracao realizada com tp - % de sim
# 1.2 Preferencia sobre integracao = concordo + concordo totalmente (% de concordancia)

# 2. Novas viagens
# 2.1 Nova viagem realizada (% de 'nao faria a viagem')
# 2.2 Opiniao sobre novas viagens - 4.1 (% de concordancia)

files_folder <- "../../indice-mobilidade_dados"
# dados <- read_csv(sprintf('%s/dados_99/base_clientes.csv', files_folder))
dados <- read_csv(sprintf('%s/00_Originais/PesquisaClientes99/pesquisa_clientes_99_2021-2022_tratado.csv', files_folder))


dados_2_1 <- filter(dados, county_name=='nat')%>%
  count(name='modo',modo_outro=='nao_faria_viagem')

# 1.1
dados_1_1 <- dados %>% group_by(county_name, integ_app_e_transp_col)%>%
  summarise(n=n())%>% 
  mutate(percent_1_1 = n/sum(n))

comp_1_1 <- dplyr::filter(dados_1_1, integ_app_e_transp_col=='sim')%>%
  dplyr::select(county_name, percent_1_1)


# 1.2
dados_1_2 <- dados %>% group_by(county_name, concord_int_app_transp_col)%>%
  summarise(n=n())%>% 
  mutate(percent_1_2 = n/sum(n))

comp_1_2 <- dplyr::filter(dados_1_2, concord_int_app_transp_col=='concorda' | concord_int_app_transp_col=='concorda_total')%>%
  group_by(county_name)%>%
  mutate(soma_1_2=sum(n))%>%
  mutate(percent_1_2=sum(percent_1_2))%>%
  dplyr::filter(concord_int_app_transp_col=='concorda')%>%
  dplyr::select(county_name, percent_1_2)

INIA <- left_join(comp_1_1, comp_1_2, by='county_name')

# 2.1
dados_2_1 <- dados %>% group_by(county_name, modo_outro)%>%
  summarise(n=n()) %>% 
  mutate(percent_2_1 = n/sum(n))

comp_2_1 <- dplyr::filter(dados_2_1, modo_outro=='nao_faria_viagem')%>%
  dplyr::select(county_name, percent_2_1)

INIA <- left_join(INIA, comp_2_1, by='county_name')

# 2.2

dados_2_2 <- dados %>% group_by(county_name, concord_app_mais_cidade)%>%
  summarise(n=n())%>% 
  mutate(percent_2_2 = n/sum(n))

comp_2_2 <- dplyr::filter(dados_2_2, concord_app_mais_cidade=='concorda' | concord_app_mais_cidade=='concorda_total')%>%
  group_by(county_name)%>%
  mutate(soma_2_2=sum(n))%>%
  mutate(percent_2_2=sum(percent_2_2))%>%
  dplyr::filter(concord_app_mais_cidade=='concorda')%>%
  dplyr::select(county_name, percent_2_2)

INIA <- left_join(INIA, comp_2_2, by='county_name')

rm(comp_1_1, comp_1_2, comp_2_1, comp_2_2, dados_1_1, dados_1_2, dados_2_1, dados_2_2)
# rm(INIA)

# Normalizando
INIA$percent_1_1<- (INIA$percent_1_1 - min(INIA$percent_1_1))/(max(INIA$percent_1_1)-min(INIA$percent_1_1))
INIA$percent_2_1 <- (INIA$percent_2_1 - min(INIA$percent_2_1))/(max(INIA$percent_2_1)-min(INIA$percent_2_1))
INIA$percent_1_2 <- (INIA$percent_1_2 - min(INIA$percent_1_2))/(max(INIA$percent_1_2)-min(INIA$percent_1_2))
INIA$percent_2_2 <- (INIA$percent_2_2 - min(INIA$percent_2_2))/(max(INIA$percent_2_2)-min(INIA$percent_2_2))
INIA$ind_1 <- ((5*INIA$percent_1_1) + (5*INIA$percent_1_2))/10
INIA$ind_2 <- ((5*INIA$percent_2_1) + (5*INIA$percent_2_2))/10

INIA$INIA <- ((7*INIA$ind_1) + (3*INIA$ind_2))/10

#INIA$ind_1 <- ((5*INIA$percent_1_1) + (5*INIA$percent_1_2))/10
#INIA$ind_2 <- ((5*INIA$percent_2_1) + (5*INIA$percent_2_2))/10

#INIA$INIA <- ((7*INIA$ind_1) + (3*INIA$ind_2))/10


# ----- PARTE 2: INCORPORACAO NO RANKING DE MOBILIDADE -----

dados_iaod <- read_csv2(sprintf('%s/19_indices/2019/IAOD_2019.csv', files_folder))%>%
  rename('IAOD' = 'iaod')%>%
  dplyr::select(c('muni', 'IAOD')) %>% 
  mutate(IAOD = as.double(IAOD))

ranking <- left_join(INIA, dados_iaod, by=c('county_name'='muni'))%>%
  dplyr::select(c('county_name', 'INIA', 'IAOD'))


ranking$ranking <- ((8*ranking$IAOD)+(2*ranking$INIA))/10

# ranking$ranking_n <- ((8*ranking$IAOD)+(2*ranking$INIA_n))/10

ranking <- ranking%>%
  dplyr::select(-c('INIA', 'ranking'))

save_folder <- sprintf('%s/19_indice_mobilidade/2019', files_folder)
write_csv2(ranking, sprintf('%s/ranking_final_2019_2.csv', save_folder))