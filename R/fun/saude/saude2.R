
#' A funcao 'saude_filter':
#' 1) Lê os dados da base do CNES e faz filtros selecionando as colunas de interesse
#' 2) Faz filtros selecionando os municipios de interesse, hospitais publicos, 
#' faz refactor dos niveis de atendimento e retira hospitais indesejados

# # Links para baixar as bases de dados
# 
# Bases de dados completas por mês - o mês base usado para 2019 foi 202001
# http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp
# 
# Dicionário de dados
# http://cnes.datasus.gov.br/pages/downloads/documentacao.jsp
# 
# Não usados mas segue o link de referência:
# Tabela simplificada dos estabelecimentos, deve ser baixada por estado e por mês
# http://cnes.datasus.gov.br/pages/estabelecimentos/extracao.jsp
# 
# Arquivo wiki explicando a navegação do site
# https://wiki.saude.gov.br/cnes/index.php/Portal_CNES


# ------------------------------------------------
# Passo a passo
# ------------------------------------------------

# 1. Criar as pastas de trabalho para os dados de saúde
ano = 2019 # Atualizar conforme o caso
files_folder <- "../../indice-mobilidade_dados"
subfolder7 <- sprintf("%s/07_cnes_saude", files_folder)
subfolder7A <- sprintf("%s/%s", subfolder7, ano)
dir.create(sprintf("%s", subfolder7A), recursive = TRUE, showWarnings = FALSE)


# 2. Baixar a base completa do CNES para o mês base no link e descompactá-la
# http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp


# 3. Checar o encoding dos arquivos com uchardet *.csv. Abaixo segue a lista
# dos arquivos que serão usados e respectivos encodings para o mês base baixado.
# Os arquivos estão na ordem em que serão abertos:

# tbEstabelecimento202001.csv: UTF-8
# rlEstabSubTipo202001.csv: UTF-8
# tbTipoUnidade202001.csv: ASCII
# rlEstabProgFundo202001.csv: UTF-8
# tbGestao202001.csv: ASCII
# rlEstabAtendPrestConv202001.csv: UTF-8


# 4. Informar a pasta onde estão os dados descompactados e iniciar processo de
# junção das bases
cnes_files_folder <- sprintf("%s/00_Originais/CNES202001/BASE_DE_DADOS_CNES_202001", files_folder)


# ------------------------------------------------
# tbEstabelecimento
# ------------------------------------------------
# A coluna TP_PFPJ indica se é pessoa física (1) ou jurídica (3)
# As colunas NU_LATITUDE e NU_LONGITUDE têm os dados de latlong (há NAs)
# A coluna CO_UNIDADE é composta dos seis primeiros dígitos do código do 
# município (221100) e dos sete dígitos do CNES (9285350) - 2211009285350
tb_estab <- read_delim(sprintf('%s/tbEstabelecimento202001.csv', cnes_files_folder), 
                       delim = ';', 
                       col_types = cols(.default = "c"),
                       locale = locale(encoding = 'utf-8'))


# Adicionar coluna IBGE, que seria equivalente aos seis primeiros dígitos do 
# código dos municípios em munis_list
cnes_estab <- 
  tb_estab %>% 
  mutate(IBGE = str_sub(.$CO_UNIDADE, start = 1, end = 6), .after = 'CO_UNIDADE')

# Remover bases temporárias
rm(tb_estab)


# ------------------------------------------------
# tipo_unidade = rlEstabSubTipo + tbTipoUnidade
# ------------------------------------------------
# rlEstabProgFundo é uma ponte para juntar tbTipoUnidade à base principal
rlEstabSubTipo <- read_delim(sprintf('%s/rlEstabSubTipo202001.csv', cnes_files_folder),
                             delim = ';', 
                             col_types = cols(.default = "c"),
                             locale = locale(encoding = 'utf-8'))

# Selecionar colunas de interesse
rlEstabSubTipo <- rlEstabSubTipo %>% dplyr::select(CO_UNIDADE, CO_TIPO_UNIDADE)


# tbTipoUnidade
tbTipoUnidade <- read_delim(sprintf('%s/tbTipoUnidade202001.csv', cnes_files_folder),
                            delim = ';', 
                            col_types = cols(.default = "c"),
                            locale = locale(encoding = 'ascii'))

# Juntar as duas bases
tipo_unidade <- left_join(rlEstabSubTipo, tbTipoUnidade, by = 'CO_TIPO_UNIDADE')

# Juntar ao dataframe principal
cnes_estab <- cnes_estab %>% left_join(tipo_unidade, by = 'CO_UNIDADE')

# Remover bases temporárias
rm(tipo_unidade, rlEstabSubTipo, tbTipoUnidade)


# ------------------------------------------------
# gestao = rlEstabProgFundo + tbGestao
# ------------------------------------------------
# rlEstabProgFundo é uma ponte para juntar tbGestao à base principal
rlEstabProgFundo <- read_delim(sprintf('%s/rlEstabProgFundo202001.csv', cnes_files_folder),
                               delim = ';', 
                               col_types = cols(.default = "c"),
                               locale = locale(encoding = 'utf-8'))

# Selecionar colunas de interesse - não vamos precisar de TP_ESTADUAL_MUNICIPAL,
# que é redundante com a coluna TP_GESTAO do arquivo tbEstabelecimento
rlEstabProgFundo <- rlEstabProgFundo %>% dplyr::select(CO_UNIDADE, CO_ATIVIDADE)


# tbGestao
tbGestao <- read_delim(sprintf('%s/tbGestao202001.csv', cnes_files_folder),
                       delim = ';', 
                       col_types = cols(.default = "c"),
                       locale = locale(encoding = 'ascii'))

# CO_GESTAO - DS_GESTAO - TP_PROG
# 1 - ATENCAO BASICA - 1
# 2 - MEDIA COMPLEXIDADE - 1
# 3 - INTERNACAO - 2
# 4 - ALTA COMPLEXIDADE - 1
# 5 - MEDIA COMPLEXIDADE - 2
# 6 - ALTA COMPLEXIDADE - 2
# 0 - NAO SE APLICA - 0

# TP_PROG é o domínio do Tipo de Atendimento:
# 0 - Não se aplica
# 1 - Ambulatorial
# 2 - Hospitalar

# Juntar os dois dataframes
gestao <- 
  rlEstabProgFundo %>% 
  left_join(tbGestao, by = c("CO_ATIVIDADE" = "CO_GESTAO")) %>% 
  # Reordenar colunas - não vamos precisar da descrição DS_GESTAO, 
  # que é redundante com a coluna CO_GESTAO/CO_ATIVIDADE. Vamos manter
  # o nome CO_GESTAO, mudado só para o join, para que essa coluna não
  # seja confundida com a CO_ATIVIDADE do arquivo tbEstabelecimento, 
  # que se refere a atividades de ensino
  dplyr::select(CO_UNIDADE, CO_GESTAO = CO_ATIVIDADE, TP_PROG)

# # Algumas unidades possuem mais de um tipo de gestão
# gestao_mais_um_counidade <- gestao %>% group_by(CO_UNIDADE) %>% tally() %>% filter(n > 1) %>% arrange(-n)
# dim(gestao_mais_um_counidade)
# head(gestao_mais_um_counidade)

# # Alguns possuem mais de um TP_ESTADUAL_MUNICIPAL, CO_ATIVIDADE, DS_GESTAO e TP_PROG
# gestao %>% filter(CO_UNIDADE == '4113702578506')

# # Esta mesma unidade é uma só linha em cnes_estab
# cnes_estab %>% filter(CO_UNIDADE == '4113702578506')

# Criar colunas de valores boolean para as variáveis CO_GESTAO e TP_PROG - isso
# vai possibilitar um join() que mantenha cada CNES em uma só linha, sem repetir
# conforme os tipos de atendimento mudam
gestao <- 
  gestao %>% 
  # Primeiro, criar dummies das variáveis CO_GESTAO e TP_PROG
  dummy_cols(select_columns = c('CO_GESTAO', 'TP_PROG'), remove_selected_columns = TRUE) %>% 
  # O problema é que pode haver várias linhas do mesmo CO_UNIDADE, que ficaram
  # como dummies separadas - queremos integrar tudo em uma linha só, como
  # um boolean. Para isso, vamos somar as colunas com group_by()...
  group_by(CO_UNIDADE) %>% 
  summarize(across(everything(), sum)) %>% 
  # ... e o que for maior que zero vai se tornar 1
  mutate(across(where(is.numeric), ~ case_when(. > 0 ~ 1, TRUE ~ 0)))


# Juntar ao dataframe principal
cnes_estab <- cnes_estab %>% left_join(gestao, by = 'CO_UNIDADE')

# Remover bases temporárias
rm(gestao, rlEstabProgFundo, tbGestao)


# # 75 estabelecimentos em cnes_estab não possuíam dados sobre gestão
# no_gestao <- cnes_estab %>% filter(is.na(CO_GESTAO_00)) 
# nrow(no_gestao)

# # Alguns desses estabelecimentos estão em municípios do projeto? Aparentemente não
# munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist() %>% substring(., 1, 6)
# no_gestao %>% select(IBGE) %>% distinct() %>% filter(.$IBGE %in% munis)
# no_gestao %>% select(CO_MUNICIPIO_GESTOR) %>% distinct() %>% filter(.$CO_MUNICIPIO_GESTOR %in% munis)


# ------------------------------------------------
# convenio = rlEstabAtendPrestConv
# ------------------------------------------------
# rlEstabProgFundo
convenio <- read_delim(sprintf('%s/rlEstabAtendPrestConv202001.csv', cnes_files_folder),
                       delim = ';', 
                       col_types = cols(.default = "c"),
                       locale = locale(encoding = 'utf-8'))

# Selecionar colunas de interesse
convenio <- convenio %>% dplyr::select(CO_UNIDADE, CO_CONVENIO, CO_ATENDIMENTO_PRESTADO)

# CO_CONVENIO - DS_CONVENIO
# 1 - SUS
# 2 - PARTICULAR
# 3 - PLANO / SEGURO PROPRIO
# 4 - PLANO / SEGURO TERCEIRO
# 5 - PLANO DE SAUDE PUBLICO
# 6 - PLANO DE SAUDE PRIVADO
# 7 - GRATUIDADE


# Criar colunas boolean para as variáveis CO_CONVENIO e CO_ATENDIMENTO_PRESTADO - 
# isso vai possibilitar um join() que mantenha cada CNES em uma só linha, sem 
# repetir conforme os tipos de atendimento mudam
convenio <- 
  convenio %>% 
  # Primeiro, criar dummies das variáveis CO_CONVENIO e CO_ATENDIMENTO_PRESTADO
  dummy_cols(select_columns = c('CO_CONVENIO', 'CO_ATENDIMENTO_PRESTADO'), remove_selected_columns = TRUE) %>% 
  # O problema é que pode haver várias linhas do mesmo CO_UNIDADE, que ficaram
  # como dummies separadas - queremos integrar tudo em uma linha só, como
  # um boolean. Para isso, vamos somar as colunas com group_by()...
  group_by(CO_UNIDADE) %>% 
  summarize(across(everything(), sum)) %>% 
  # ... e o que for maior que zero vai se tornar 1
  mutate(across(where(is.numeric), ~ case_when(. > 0 ~ 1, TRUE ~ 0)))


# # Checando uma das CO_UNIDADE que possuía 23 entradas
# convenio %>% filter(CO_UNIDADE == '3113702178982')

# Juntar ao dataframe principal
cnes_estab <- cnes_estab %>% left_join(convenio, by = 'CO_UNIDADE')

# Remover bases temporárias
rm(convenio)


# # 77 estabelecimentos não possuem dados sobre convenio
# no_convenio <- cnes_estab %>% filter(is.na(CO_CONVENIO_01)) 
# nrow(no_convenio)
# 
# # A maior parte é igual à que não tinha dados de gestão
# this <- no_gestao$CO_UNIDADE
# that <- no_convenio$CO_UNIDADE
# this %in% that

# # Alguns desses estabelecimentos estão em municípios do projeto? Aparentemente não
# munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist() %>% substring(., 1, 6)
# no_convenio %>% select(IBGE) %>% distinct() %>% filter(.$IBGE %in% munis)
# no_convenio %>% select(CO_MUNICIPIO_GESTOR) %>% distinct() %>% filter(.$CO_MUNICIPIO_GESTOR %in% munis)


# ------------------------------------------------
# Criar dummies relacionadas ao tipo de atendimento
# ------------------------------------------------
# No código original do IPEA:
# complex_baix - se refere aos booleans CO_GESTAO_01
# complex_medi - se refere aos booleans CO_GESTAO_02, CO_GESTAO_05
# complex_alta - se refere aos booleans CO_GESTAO_03, CO_GESTAO_04, CO_GESTAO_06
# Para CO_GESTAO_03 foi atribuída somente a complexidade média

# _ambu - se refere aos booleans TP_PROG_01
# _hosp - se refere aos booleans TP_PROG_02

# _est - se refere à coluna TP_GESTAO == 'E'
# _mun - se refere à coluna TP_GESTAO == 'M'
# Gestão dupla (TP_GESTAO == 'D') atribuída a ambos
# Complexidade baixa sempre terá TP_PROG igual a Ambulatorial ou Não Se Aplica
health_low_filter  <- quo(CO_GESTAO_01 == 1 & (TP_PROG_1 == 1 | TP_PROG_2 == 1) & TP_GESTAO != 'S')
health_med_filter  <- quo((CO_GESTAO_02 == 1 | CO_GESTAO_03 == 1 | CO_GESTAO_05 == 1) & (TP_PROG_1 == 1 | TP_PROG_2 == 1) & TP_GESTAO != 'S')
health_high_filter <- quo((CO_GESTAO_04 == 1 | CO_GESTAO_06 == 1) & (TP_PROG_1 == 1 | TP_PROG_2 == 1) & TP_GESTAO != 'S')

cnes_estab <- 
  cnes_estab %>% 
  # Criar colunas demarcando quais são os atendimentos de baixa, média e alta complexidade
  mutate(health_low  = ifelse(!!health_low_filter,  1, 0),
         health_med  = ifelse(!!health_med_filter,  1, 0),
         health_high = ifelse(!!health_high_filter, 1, 0))


# ------------------------------------------------
# Aplicar filtros finais IPEA
# ------------------------------------------------
# Códigos de municípios que fazem parte do projeto (primeiros seis dígitos somente)
munis <- munis_list$munis_metro[ano_metro == ano]$code_muni %>% unlist() %>% substring(., 1, 6)

# Filtros de remoção vindos do IPEA
to_remove1 <- '   |VETERINARI|CENTRO DE ESTUDOS|PSIQUIAT|PRESIDIO|PENAL|JUDICIARIO|PENITENCIARIA|PENITENCIARIO|PRISIONAL|FUNDACAO CASA|CASA DE CUSTODIA|CASA DE CUST|SEDIT|DETENCAO|PROVISORIA|SANATORIO|POLICIA| PADI|DE REGULACAO|VIGILANCIA|SAMU |ACADEMIA|DEPEND QUIMICO|REEDUCACAO SOCIAL|CAPS|CENTRO DE ATENCAO PSICOSSOCIAL|DISTRIB DE ORGAOS|MILITAR|CADEIA PUBLICA|DOMICILIAR|ARTES MARCIAIS|UBS IPAT|UBS CDPM II'
to_remove2 <- 'TELESSAUDE|UNIDADE MOVEL|DOMICILIAR|PSICOSSOCIAL|FARMACIA|DE ORGAOS|CENTRAL DE REGULACAO DO ACESSO'

cnes_estab <- 
  cnes_estab %>% 
  # Filter 0: healthcare nao aplica (pq nao tem servicos de alta/baixa complexidade, e.g. academias de saude, secretarias de saude etc)
  # Gestão deve ser municipal, estadual ou dupla, mas não "sem gestão"
  filter(TP_GESTAO != 'S') %>% 
  # Deve haver necessariamente um tipo de atendimento, não pode ser "não se aplica"
  filter(CO_GESTAO_01 == 1 | CO_GESTAO_02 == 1 | CO_GESTAO_03 == 1 |
         CO_GESTAO_04 == 1 | CO_GESTAO_05 == 1 | CO_GESTAO_06 == 1) %>% 
  # Filter 1: healthcare facilities operating with the public health system
  # Convênio deve atender SUS
  filter(CO_CONVENIO_01 == 1) %>% 
  # Filter 2: Pessoa juridica
  # TP_PFPJ deve ser de pessoa jurídica (3)
  filter(TP_PFPJ == '3') %>% 
  # Filter 3: Only municipalities in the project
  filter(.$IBGE %in% munis) %>% 
  # Filter 4: Only atendimento hospitalar ou ambulatorial
  filter(TP_PROG_1 == 1 | TP_PROG_2 == 1) %>% 
  # Filter 5. Remove special categories of facilities 
  # 5.1 Delete prison hospitals, research centers, police hospitals etc
  filter(!.$NO_FANTASIA %like% to_remove1) %>% 
  # 5.2 Delete Home care, tele saude, unidades moveis de saude
  filter(!.$DS_TIPO_UNIDADE %like% to_remove2) %>% 
  # Converter latitude e longitude para double
  mutate(NU_LATITUDE = as.double(NU_LATITUDE),
         NU_LONGITUDE = as.double(NU_LONGITUDE))


# ------------------------------------------------
# Salvar arquivo final
# ------------------------------------------------
write_rds(cnes_estab, sprintf("%s/saude_%s_filter_geocoded.rds", subfolder7A, ano), compress = 'gz')
