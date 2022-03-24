# Carregar dados socioecônomicos dos setores censitários

# Carregar bibliotecas
source('fun/setup.R')

### 1. Carregar micro dados dos setores censitários --------------------------------------------------

# Fazer download dos dados censitários em
# https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html
# Censos > Censo Demográfico 2010 > Resultados do Universo > Agregados por Setores Censitários
# 
# Descompactar todos os arquivos .zip. Os arquivos foram salvos na pasta
# "../../indice_acesso_cidade_dados/00_Originais/Censo2010. Vamos precisar das planilhas:
# 1. Basico
# 2. Domicilio 02
# 3. DomicilioRenda
# 4. Entorno04
# 5. Pessoa03 e
# 6. Pessoa13
# 
# Checar codificação dos arquivos com uchardet *.csv ou guess_encoding()
#
# Todos os arquivos Basico*.csv são ISO-8859-1, exceto:
# Basico_SP2.csv: UTF-8
# Basico_TO.csv: MAC-CENTRALEUROPE
# Todos os arquivos Entorno04*.csv são ASCII, exceto:
# Entorno04_ES.csv: ISO-8859-1
# Entorno04_SP1.csv: ISO-8859-1
# Entorno04_TO.csv: ISO-8859-1
# Todos os arquivos Domicilio*.csv são ASCII;
# Todos os arquivos Pessoa*.csv são ASCII.

# Tratar manualmente as exceções. Somente Basico_TO.csv que não tem como
# ser tratado e será aberto com problemas. A sorte é que as colunas que
# usaremos não possuem acentuação e estão isentas dos problemas de codificação
# this_TO <- c(NA, rep('NULL', 2), NA, rep('NULL', 7), NA, rep('NULL', 21))


# Descrição das variáveis
# Dom2_V002 # Moradores em domicílios particulares permanentes
# DomRen_V003 # Total do rendimento nominal mensal dos domicílios particulares permanentes 

# PessoaRenda
# V001 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de até ½ salário mínimo
# V002 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de ½ a 1 salário mínimo
# V003 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 1 a 2 salários mínimos
# V004 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 2 a 3 salários mínimos
# V005 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 3 a 5 salários mínimos
# V006 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 5 a 10 salários mínimos
# V007 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 10 a 15 salários mínimos
# V008 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 15 a 20 salários mínimos
# V009 Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 20 salários mínimos
# V010 Pessoas de 10 anos ou mais de idade sem rendimento nominal mensal 

# Raca/cor
# Pess3_V002 # Pessoas Residentes e cor ou raça - branca
# Pess3_V003 # Pessoas Residentes e cor ou raça - preta
# Pess3_V004 # Pessoas Residentes e cor ou raça - amarela
# Pess3_V005 # Pessoas Residentes e cor ou raça - parda
# Pess3_V006 # Pessoas Residentes e cor ou raça - indígena 

# Idade
# 0 a 5   - Pess13_V023:Pess13_V039
# 6 a 14  - Pess13_V040:Pess13_V048
# 15 a 18 - Pess13_V049:Pess13_V052
# 19 a 24 - Pess13_V053:Pess13_V058
# 25 a 39 - Pess13_V059:Pess13_V073
# 40 a 69 - Pess13_V074:Pess13_V0103
# 70+     - Pess13_V0104:Pess13_V134


# Pasta geral para todos os arquivos e demais pastas
files_folder <- "../../indice_acesso_cidade_dados"
censo_original_files <- sprintf("%s/00_Originais/Censo2010", files_folder)

# Lista todos os arquivos de uma pasta com o mesmo padrão regex, não recursivo
list_common_files <- function(files_path, files_pattern){
  return(list.files(path = files_path, pattern = files_pattern, 
                    recursive = FALSE, full.names = TRUE))
}

# Abre arquivos com a mesma estrutura e os junta-em um único dataframe
merge_common_files <- function(open_files, files_encoding){
  out_df <- 
    lapply(X = open_files,
           FUN = read_delim, delim = ';', locale = locale(encoding = files_encoding), 
           col_types = cols(.default = 'c')) %>% 
    rbindlist(fill = TRUE)
  
  return(out_df)
}

# Abrir arquivos do Censo - Basico
income_files <- list_common_files(censo_original_files, '^Basico_(.+).csv')
df_basico <- merge_common_files(income_files, 'iso_8859-1')
# TO tem uma codificação indefinida e vai dar erro com os acentos. Por sorte,
# não precisaremos das colunas que vão ficar com o encoding zoado
# dim = 310105 x 35; zero NAs
# Isolar colunas de interesse
df_basico <- df_basico %>% dplyr::select(Cod_UF, Cod_municipio, Cod_setor)


# Abrir arquivos do Censo - DomicilioRenda
income_files <- list_common_files(censo_original_files, '^DomicilioRenda_(.+).csv')
df_dom_renda <- merge_common_files(income_files, 'ascii')
# dim = 310120    134; zero NAs
# Isolar colunas de interesse e renomear para ficar de acordo com IPEA original
df_dom_renda <- df_dom_renda %>% dplyr::select(Cod_setor, V003) %>% rename(DomRend_V003 = V003)


# Abrir arquivos do Censo - Domicilio02
income_files <- list_common_files(censo_original_files, '^Domicilio02_(.+).csv')
df_dom02 <- merge_common_files(income_files, 'ascii')
# dim = 310120    134; zero NAs
# Isolar colunas de interesse e renomear para ficar de acordo com IPEA original
df_dom02 <- df_dom02 %>% dplyr::select(Cod_setor, V002) %>% rename(Dom2_V002 = V002)


# Abrir arquivos do Censo - Pessoa03
income_files <- list_common_files(censo_original_files, '^Pessoa03_(.+).csv')
df_pess03 <- merge_common_files(income_files, 'ascii')
# dim = 310120    254; zero NAs
# Isolar colunas de interesse e renomear para ficar de acordo com IPEA original
df_pess03 <- df_pess03 %>% dplyr::select(Cod_setor, V002:V006)
colnames(df_pess03) <- c('Cod_setor', paste0("Pess3_V00", rep(2:6)))


# Abrir arquivos do Censo - Pessoa13
income_files <- list_common_files(censo_original_files, '^Pessoa13_(.+).csv')
df_pess13 <- merge_common_files(income_files, 'ascii')
# dim = 310120    137; zero NAs
# Isolar colunas de interesse e renomear para ficar de acordo com IPEA original
df_pess13 <- df_pess13 %>% dplyr::select(Cod_setor, V023:V134)
df_tmp1 <- df_pess13 %>% dplyr::select(Cod_setor, V023:V099)
df_tmp2 <- df_pess13 %>% dplyr::select(Cod_setor, V100:V134)
colnames(df_tmp1) <- c('Cod_setor', paste0("Pess13_V0", rep(23:99)))
colnames(df_tmp2) <- c('Cod_setor', paste0("Pess13_V", rep(100:134)))
df_pess13 <- left_join(df_tmp1, df_tmp2, by = 'Cod_setor')
rm(df_tmp1, df_tmp2)


# Abrir arquivos do Censo - Entorno04
income_files <- list_common_files(censo_original_files, '^Entorno04_(.+).csv')
df_ent04 <- merge_common_files(income_files, 'ascii')
# dim = 310120    241; zero NAs
# Isolar colunas de interesse e renomear para ficar de acordo com IPEA original
df_ent04 <- df_ent04 %>% 
  dplyr::select(Cod_setor, V683:V694) %>% 
  # Substituir 'X' por 0 em algumas das variáveis
  mutate_at(vars(starts_with("V")), ~ ifelse(.x == "X", 0, .x)) %>% 
  rename_at(vars(starts_with("V")), ~ paste0("Entorno04_", .x))



# Juntar todos os arquivos em um único dataframe
setores <- 
  df_basico %>% 
  left_join(df_dom_renda, by = 'Cod_setor') %>% 
  left_join(df_dom02, by = 'Cod_setor') %>%   
  left_join(df_pess03, by = 'Cod_setor') %>%
  left_join(df_pess13, by = 'Cod_setor') %>%
  left_join(df_ent04, by = 'Cod_setor') %>% 
  # Transformar variáveis para numeric
  mutate_at(vars(matches("V\\d{3}")), as.numeric)

# Descartar dataframes anteriores
rm(df_basico, df_dom_renda, df_dom02, df_pess03, df_pess13, df_ent04, income_files)



### 2. Merge dos dados de renda com shapes dos setores censitarios

# Filtrar apenas os municipios relacionado ao projeto
code_munis <- munis_list$munis_metro$code_muni %>% unlist %>% unique()
setores2 <- setores %>% filter(Cod_municipio %in% code_munis)


# ano <- 2019; sigla_muni <- 'nat'
merge_renda_setores_all <- function(ano, munis = "all") { 
  # Criar estrutura de pastas
  subfolder2 <- sprintf("%s/02_setores_censitarios/%s", files_folder, ano)
  subfolder4 <- sprintf("%s/04_setores_agregados/%s", files_folder, ano)
  dir.create(sprintf("%s", subfolder4), recursive = TRUE, showWarnings = FALSE)
  
  # Checar se arquivo resultante já existe. Se sim, avisar e pular a cidade
  out_file <- sprintf("setores_agregados_%s_%s.rds", munis, ano)
  
  if (out_file %nin% list.files(subfolder4)) {
    # Agrupar variáveis de idade/renda e somá-las
    setores3 <- 
      setores2 %>% 
      mutate(idade_0a5   = rowSums(across(Pess13_V023:Pess13_V039), na.rm = TRUE),
             idade_6a14  = rowSums(across(Pess13_V040:Pess13_V048), na.rm = TRUE),
             idade_15a18 = rowSums(across(Pess13_V049:Pess13_V052), na.rm = TRUE),
             idade_19a24 = rowSums(across(Pess13_V053:Pess13_V058), na.rm = TRUE),
             idade_25a39 = rowSums(across(Pess13_V059:Pess13_V073), na.rm = TRUE),
             idade_40a69 = rowSums(across(Pess13_V074:Pess13_V103), na.rm = TRUE),
             idade_70    = rowSums(across(Pess13_V104:Pess13_V134), na.rm = TRUE)) %>%
      mutate(moradores_SM_0_1Q  = rowSums(across(c(Entorno04_V693:Entorno04_V694, Entorno04_V683:Entorno04_V684)), na.rm = TRUE),
             moradores_SM_1Q_1M = rowSums(across(c(Entorno04_V685, Entorno04_V686)), na.rm = TRUE),
             moradores_SM_1M_1  = rowSums(across(Entorno04_V687:Entorno04_V688), na.rm = TRUE),
             moradores_SM_1_2   = rowSums(across(Entorno04_V689:Entorno04_V690), na.rm = TRUE),
             moradores_SM_2     = rowSums(across(Entorno04_V691:Entorno04_V692), na.rm = TRUE))
    
    ## Isolar as variáveis de interesse e renomear as que não foram agrupadas
    # Renda 6.19 - variavel escolhida: 
    # V003 = Total do rendimento nominal mensal dos domicílios particulares permanentes
    setores4 <- 
      setores3 %>% 
      dplyr::select(cod_uf = Cod_UF, cod_muni = Cod_municipio, cod_setor = Cod_setor, 
                    renda_total = DomRend_V003, moradores_total = Dom2_V002, 
                    # income brackets (renda)
                    matches("moradores_SM"),
                    # raca, cor
                    cor_branca   = Pess3_V002, cor_preta = Pess3_V003, 
                    cor_amarela  = Pess3_V004, cor_parda = Pess3_V005, 
                    cor_indigena = Pess3_V006,
                    # age variables
                    matches("idade"))
    
    # Converter NAs em zeros
    setores4[is.na(setores4)] <- 0
    
    # Fazer ajuste relacionado à soma das pessoas por faixa etária e por cor/raca,
    # dado de pop total (domicilios particulares permanentes) é menor do que soma 
    # da população por faixa etária. Este trecho do código busca fazer uma 
    # correção via cálculo de proporções de cada faixa etária e cor/raça
    setores5 <- setores4 %>%
      # Ajuste para faixa etária: criar uma coluna que soma todas as pessoas por
      # faixa etária, calcular a proporção de cada faixa etária frente a essa
      # soma e, finalmente, reatribuir essa proporção frente ao valor encontrado
      # em 'moradores_total' (antiga variável Dom2_V002). Devido ao arredondamento,
      # mesmo este reajuste não será exato
      mutate(age_total = rowSums(across(idade_0a5:idade_70), na.rm = TRUE)) %>%
      mutate_at(vars(matches("idade")), ~ .x / age_total) %>%
      mutate_at(vars(matches("idade")), ~ round(.x * moradores_total)) %>%
      # Ajuste para cor/raca, com processo similar ao anterior
      mutate(race_total = rowSums(across(cor_branca:cor_indigena), na.rm = TRUE)) %>%
      mutate_at(vars(matches("cor")), ~ .x / race_total) %>%
      mutate_at(vars(matches("cor")), ~ round(.x * moradores_total)) %>%
      # Descartar colunas temporárias
      dplyr::select(-age_total, -race_total)
    
    # Converter NAs para zero sem fazer cópia de setores5
    setDT(setores5)
    setores5[is.na(setores5)] <- 0
    
    # Criar variável de renda domiciliar per capita de cada setor censitário
    setores5 <- 
      setores5 %>% 
      mutate(renda_per_capita = renda_total / moradores_total, .after = 'moradores_total')
    
    
    # Função para fazer merge dos dados e salvar arquivos na pasta de dados
    merge_renda_setores <- function(sigla){
      # status message
      message('Rodando cidade: ', sigla,"\n")
      
      # Código do município
      code_muni <- subset(munis_list$munis_metro, abrev_muni == sigla & ano_metro == ano )$code_muni %>% unlist()
      # Subset dados dos setores
      dados <- subset(setores5, cod_muni %in% code_muni)
      
      # Ler shape dos setores
      sf <- readr::read_rds( sprintf("%s/setores_%s_%s.rds", subfolder2, sigla, ano) )
      
      # merge
      sf2 <- dplyr::left_join(sf, dados, by = c('code_tract' = 'cod_setor'))
      
      # salvar
      readr::write_rds(sf2,  sprintf("%s/%s", subfolder4, out_file), compress = 'gz')
    }
    
    
    # aplicar funcao -----------------
    if (munis == "all") {
      # seleciona todos municipios ou RMs do ano escolhido
      x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
    } else (x = munis)
    
    # Parallel processing using future.apply
    # purrr::walk(x, merge_renda_setores)
    if (future::supportsMulticore()) {
      future::plan(future::multicore)
    } else {
      future::plan(future::multisession)
    }
    invisible(future.apply::future_lapply(X = x, FUN = merge_renda_setores, future.packages = c('sf', 'dplyr', 'data.table')))
    
  } else {
    message('\nArquivo para a cidade ', munis, " já existe, pulando...\n")
  }
}

#  Carregar dados socioecônomicos dos setores censitários - usar uma sigla das presentes
# em munis_list$munis_metro[ano_metro == ano]$abrev_muni ou 'all' para todos
# merge_renda_setores_all(ano = 2019, munis = 'sgo')
# merge_renda_setores_all(ano = 2019, munis = 'all')

# merge_renda_setores_all(ano = 2019, munis = 'bho')

# A criação da condicional checando se o arquivo para determinado município 
# existe fez com que não seja possível usar o 'all' (embora aparentemente ele já
# estava não-funcional antes da modificação) - usar a seguinte construção para 
# todos os arquivos:
ano = 2019
x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
lapply(X = x, FUN = merge_renda_setores_all, ano = ano)

# .rs.restartR()