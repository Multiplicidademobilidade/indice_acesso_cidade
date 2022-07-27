# Este script calcula o "Índice de integração do automóvel de aplicativo com 
# transporte público - IATP" e, a partir dele e do IAOD calculado anteriormente, calcula
# o "Índice de Acesso à Cidade" (IAC).

# Carregar bibliotecas
library('tidyverse')


# Estrutura de pastas
ano = 2019
files_folder <- "../../indice_acesso_cidade_dados"
subfolder00  <- sprintf("%s/00_Originais/PesquisaClientes99", files_folder)
subfolder18  <- sprintf('%s/18_indices/%s', files_folder, ano)


# ----- PARTE 1: CÁLCULO DO IATP -----

# Indicadores
# 1. Integraçãoo com TP
# 1.1 integra_tp: Componente integração realizada com tp - % de sim
# 1.2 Preferência sobre integração = concordo + concordo totalmente (% de concordância)

# 2. Novas viagens
# 2.1 Nova viagem realizada (% de 'não faria a viagem')
# 2.2 Opinião sobre novas viagens - 4.1 (% de concordância)


# Abrir resultados da pesquisa da 99 com clientes e isolar colunas de interesse
arquivo_pesquisa <- sprintf('%s/pesquisa_clientes_99_2021-2022_tratado.csv', subfolder00)
dados_pesquisa <- 
  read_delim(arquivo_pesquisa, delim = ',', col_types = cols(.default = 'c')) %>% 
  dplyr::select(county_name, integ_app_e_transp_col, concord_int_app_transp_col, 
                modo_outro, concord_app_mais_cidade) %>% 
  rename(muni = county_name)


# Indicador - Quantas pessoas integram viagens de aplicativo com transporte público?
integra_tp <- 
  dados_pesquisa %>% 
  # Calcular quantas pessoas por resposta
  group_by(muni, integ_app_e_transp_col) %>%
  tally() %>% 
  # Calcular percentual por cidade
  mutate(perc_integra_tp = n / sum(n)) %>% 
  # Selecionar somente quem disse que sim
  filter(integ_app_e_transp_col == 'sim') %>% 
  # Isolar colunas de interesse
  dplyr::select(muni, perc_integra_tp)


# Indicador - Quantas pessoas preferem combinar a viagem no carro compartilhado com o tp?
combina_app_tp <- 
  dados_pesquisa %>%
  # Calcular quantas pessoas por resposta
  group_by(muni, concord_int_app_transp_col) %>%
  tally() %>% 
  # Calcular percentual por cidade
  mutate(perc_combina_app_tp = n/sum(n)) %>%
  # Selecionar somente quem disse que concorda com a afirmação
  filter(concord_int_app_transp_col == 'concorda' | concord_int_app_transp_col == 'concorda_total') %>%
  # Reagrupar por muniípios, para somar percentuais
  group_by(muni) %>%
  mutate(perc_combina_app_tp = sum(perc_combina_app_tp)) %>%
  # Como colunas de percentuais entre 'concorda' e 'concorda_total' têm os mesmos
  # valores agora, pegar só a primeira
  filter(concord_int_app_transp_col == 'concorda') %>%
  # Isolar colunas de interesse
  dplyr::select(muni, perc_combina_app_tp)


# Juntar todos os percentuais em um único dataframe
percentuais_pesquisa <- integra_tp %>% left_join(combina_app_tp, by = 'muni')

# Limpar espaço de trabalho
rm(integra_tp, combina_app_tp)


# Normalizar resultados percentuais - os resultados normalizados são os indicadores
# que vão gerar o IATP
percentuais_pesquisa <- 
  percentuais_pesquisa %>% 
  mutate(i_int_tp     = (perc_integra_tp - min(.$perc_integra_tp))         / (max(.$perc_integra_tp) - min(.$perc_integra_tp)),
         i_op_int_tp = (perc_combina_app_tp - min(.$perc_combina_app_tp))  / (max(.$perc_combina_app_tp) - min(.$perc_combina_app_tp)))


# Criar o Índice de integração do automóvel de aplicativo com transporte público (IATP),
# em que cada indicador corresponde a 50% do total da composição do índice
indice_iatp <- 
  percentuais_pesquisa %>% 
  mutate(iatp = ((i_int_tp * 5) + (i_op_int_tp * 5)) / 10)


# Guardar resultados do IATP
out_file1 <- sprintf('%s/IATP_%s.csv', subfolder18, ano)
write_delim(indice_iatp, out_file1, delim = ';')



# ----- PARTE 2: CÁLCULO DO "Índice de Acesso à Cidade" (IAC) -----

# Carregar dados_calculados do Indicador de Acesso às Oportunidades em prol da
# Redução de Desigualdades (IAOD)
arquivo_iaod <- sprintf('%s/IAOD_%s.csv', subfolder18, ano)
indice_iaod <- read_delim(arquivo_iaod, delim = ';')

# arquivo_iatp <- sprintf('%s/IATP_%s.csv', subfolder18, ano)
# indice_iatp <- read_delim(arquivo_iatp, delim = ';')


# Juntar dados de ambos os índices em um único dataframe
indices <- 
  indice_iaod %>% 
  # Isolar colunas de interesse
  dplyr::select(c('muni', 'iaod')) %>% 
  # Juntar com índice IATP
  left_join(subset(indice_iatp, select = c('muni', 'iatp')), by = 'muni')


# Calcular o Índice de Acesso à Cidade (IAC): o IAOD corresponde a 80% da 
# composição do índice, enquanto o IATP corresponde a 20%
indices <- indices %>% mutate(iac = ((iaod * 8) + (iatp * 2)) / 10)

# Guardar resultados do cálculo do IAC
out_file2 <- sprintf('%s/IAC_%s.csv', subfolder18, ano)
write_delim(indices, out_file2, delim = ';')
