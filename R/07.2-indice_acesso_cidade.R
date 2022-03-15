# Este script calcula o "Índice de integração e novas viagens do automóvel por
# aplicativo - INIA" e, a partir dele e do IAOD calculado anteriormente, calcula
# o "Índice de Acesso à Cidade" (IAC).

# Carregar bibliotecas
library('tidyverse')


# Estrutura de pastas
ano = 2019
files_folder <- "../../indice-mobilidade_dados"
subfolder00  <- sprintf("%s/00_Originais/PesquisaClientes99", files_folder)
subfolder19  <- sprintf('%s/19_indices/%s', files_folder, ano)


# ----- PARTE 1: CÁLCULO DO INIA -----

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


# Quantas pessoas integram viagens de aplicativo com transporte público?
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


# Quantas pessoas preferem combinar a viagem no carro compartilhado com o tp?
combina_app_tp <- 
  dados_pesquisa %>%
  # Calcular quantas pessoas por resposta
  group_by(muni, concord_int_app_transp_col) %>%
  tally() %>% 
  # Calcular percentual por cidade
  mutate(perc_combina_app_tp = n/sum(n)) %>% 
  # Selecionar somente quem disse que concorda
  filter(concord_int_app_transp_col == 'concorda') %>%
  # Isolar colunas de interesse
  dplyr::select(muni, perc_combina_app_tp)


# Quantas pessoas não teriam feito a viagem não fosse o veículo por aplicativo?
nao_teriam_vj <- 
  dados_pesquisa %>%
  # Calcular quantas pessoas por resposta
  group_by(muni, modo_outro) %>%
  tally() %>% 
  # Calcular percentual por cidade
  mutate(perc_nao_teriam_vj = n / sum(n)) %>% 
  # Selecionar somente quem disse que não teria feito a viagem
  filter(modo_outro == 'nao_faria_viagem') %>%
  # Isolar colunas de interesse
  dplyr::select(muni, perc_nao_teriam_vj)


# Quantas pessoas concordam que usar carros compartilhados permite fazer mais coisas na cidade?
app_mais_viagens <- 
  dados_pesquisa %>%
  # Calcular quantas pessoas por resposta
  group_by(muni, concord_app_mais_cidade) %>%
  tally() %>% 
  # Calcular percentual por cidade
  mutate(perc_app_mais_vj = n / sum(n)) %>% 
  # Selecionar somente quem disse que concorda com a afirmação
  filter(concord_app_mais_cidade == 'concorda' | concord_app_mais_cidade == 'concorda_total') %>%
  # Reagrupar por muniípios, para somar percentuais
  group_by(muni) %>%
  mutate(perc_app_mais_vj = sum(perc_app_mais_vj)) %>%
  # Como colunas de percentuais entre 'concorda' e 'concorda_total' têm os mesmos
  # valores agora, pegar só a primeira
  filter(concord_app_mais_cidade == 'concorda') %>%
  # Isolar colunas de interesse
  dplyr::select(muni, perc_app_mais_vj)


# Juntar todos os percentuais em um único dataframe
percentuais_pesquisa <- 
  integra_tp %>% 
  left_join(combina_app_tp,   by = 'muni') %>% 
  left_join(nao_teriam_vj,    by = 'muni') %>% 
  left_join(app_mais_viagens, by = 'muni')

# Limpar espaço de trabalho
rm(integra_tp, combina_app_tp, nao_teriam_vj, app_mais_viagens)


# Normalizar resultados percentuais - os resultados normalizados são os componentes
# que vão gerar o (a) indicador de integração com o transporte público e (b) indicador
# de novas viagens
percentuais_pesquisa <- 
  percentuais_pesquisa %>% 
  # Os dois primeiros componentes vão formar o indicador de integração com o transporte público
  mutate(c_int_tp     = (perc_integra_tp - min(.$perc_integra_tp))         / (max(.$perc_integra_tp) - min(.$perc_integra_tp)),
         c_prefint_tp = (perc_combina_app_tp - min(.$perc_combina_app_tp)) / (max(.$perc_combina_app_tp) - min(.$perc_combina_app_tp)),
         # Os dois segundos vão formar o indicador de novas viagens
         c_nova_vg    = (perc_nao_teriam_vj - min(.$perc_nao_teriam_vj))   / (max(.$perc_nao_teriam_vj) - min(.$perc_nao_teriam_vj)),
         c_op_nova_vg = (perc_app_mais_vj - min(.$perc_app_mais_vj))       / (max(.$perc_app_mais_vj) - min(.$perc_app_mais_vj)))


# Criar o Índice de integração e novas viagens do automóvel por aplicativo (INIA),
# em que cada componente corresponde a 50% do total da composição dos indicadores...
indice_inia <- 
  percentuais_pesquisa %>% 
  # ... de integração com o transporte público
  mutate(itp = ((c_int_tp * 5) + (c_prefint_tp * 5)) / 10,
         # ... de novas viagens
         inv = ((c_nova_vg * 5) + (c_op_nova_vg * 5)) / 10) %>% 
  # Por sua vez, o indicador de integração com o transporte público corresponde
  # a 70% da composição do INIA, enquanto o indicador de novas viagens corresponde
  # a 30% do total
  mutate(inia = ((itp * 7) + (inv * 3)) / 10)


# Guardar resultados do INIA
out_file1 <- sprintf('%s/INIA_%s.csv', subfolder19, ano)
write_delim(indice_inia, out_file1, delim = ';')



# ----- PARTE 2: CÁLCULO DO "Índice de Acesso à Cidade" (IAC) -----

# Carregar dados_calculados do Indicador de Acesso às Oportunidades em prol da
# Redução de Desigualdades (IAOD)
arquivo_iaod <- sprintf('%s/IAOD_%s.csv', subfolder19, ano)
indice_iaod <- read_delim(arquivo_iaod, delim = ';')


# Juntar dados de ambos os índices em um único dataframe
indices <- 
  indice_iaod %>% 
  # Isolar colunas de interesse
  dplyr::select(c('muni', 'iaod')) %>% 
  # Juntar com índice INIA
  left_join(subset(indice_inia, select = c('muni', 'inia')), by = 'muni')


# Calcular o Índice de Acesso à Cidade (IAC): o IAOD corresponde a 80% da 
# composição do índice, enquanto o INIA corresponde a 20%
indices <- indices %>% mutate(iac = ((iaod * 8) + (inia * 2)) / 10)


# Guardar resultados do cálculo do IAC
out_file2 <- sprintf('%s/IAC_%s.csv', subfolder19, ano)
write_delim(indices, out_file2, delim = ';')
