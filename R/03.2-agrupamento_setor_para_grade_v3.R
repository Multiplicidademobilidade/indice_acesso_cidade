# Agregar informações dos setores censitários (renda, idade) para a grade estatística

# carregar bibliotecas
source('fun/setup.R')

#' A função 'renda_de_setor_p_grade()' passa todas as variáveis que foram coletadas
#' dos setores censitários para as grades estatísticas  
renda_de_setor_p_grade_muni_1 <- function(ano, sigla_muni) {
  # sigla_muni <- 'nat'; ano <- 2019
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder3 <- sprintf("%s/03_grade_municipios", files_folder)
  subfolder3A <- sprintf("%s/%s", subfolder3, ano)
  subfolder5 <- sprintf("%s/05_setores_agregados", files_folder)
  subfolder5A <- sprintf("%s/%s", subfolder5, ano)
  subfolder7 <- sprintf("%s/XY_grade_municipio_com_renda_cor", files_folder)
  subfolder7A <- sprintf("%s/%s", subfolder7, ano)
  if ("XY_grade_municipio_com_renda_cor" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7)
  }
  if (ano %nin% list.dirs(subfolder7, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7A)
  }
  
  # Status message
  message(Sys.time(), ' - Trabalhando na cidade: ', sigla_muni, '\n')
  
  # Checar se arquivos resultantes já existe. Se sim, avisar e pular a cidade
  out_file1 <- sprintf("tmp1_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file2 <- sprintf("tmp2_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file3 <- sprintf("grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  
  if (out_file1 %nin% list.files(subfolder7A) & out_file2 %nin% list.files(subfolder7A) & out_file3 %nin% list.files(subfolder7A)){
    # Caminho para os arquivos
    path_setor <- sprintf("%s/setores_agregados_%s_%s.rds", subfolder5A, sigla_muni, ano)
    path_grade <- sprintf("%s/grade_%s_%s.rds", subfolder3A, sigla_muni, ano)
    
    # Ler shapes de setores censitários e grades estatísticas
    setor <- readr::read_rds(path_setor)
    grade <- readr::read_rds(path_grade)
    
    # Descartar grades vazias
    grade <- subset(grade, POP > 0)
    
    # Garantir que os dois shapes tenham a mesma projeção
    if (st_crs(setor)$input != st_crs(grade)$input){
      setor <- sf::st_transform(setor, sf::st_crs(grade))
    }
    
    # Criar id unico de cada grade e filtrar colunas
    grade$id_grade <- 1:nrow(grade)
    
    # Status message
    etapa1 <- Sys.time()
    message('\n', etapa1, ' - 1-2: Começando cálculo de área geral em grade_corrigida em: ', sigla_muni, '\n')
    
    # Corrigir grades de borda - cortar as grades da borda, tirar 
    # rebarbas e dividis a grade segundo recorte dos setores
    grade_corrigida <- 
      grade %>%
      # Deixar um registro da área de cada grade
      mutate(area_antes = as.numeric(st_area(.)))
    rm(grade)
    
    
    # Status message
    etapa2 <- Sys.time()
    message('\n', etapa2, ' - 2-2: Começando st_intersect() em grade_corrigida em: ', sigla_muni, '\n')
    
    # Fazer interseção dos setores censitários com a grade - na prática,
    # atribui os ids da coluna code_tract para a grade
    grade_corrigida <- 
      grade_corrigida %>% 
      st_intersection(setor %>% dplyr::select(code_tract))
    
    
    # Salvar arquivo temporário em disco
    write_rds(grade_corrigida, sprintf("%s/%s", subfolder7A, out_file1), compress = 'gz')
    
    # Status message
    etapa3 <- Sys.time()
    message('\n', sigla_muni, ' temporário 1 finalizado. Tempos:\n')
    message('Etapa 1: ', round(etapa2-etapa1, 1), '\nEtapa 2: ', round(etapa3-etapa2, 1), '\n')
    beep(sound = 1)
    
  } else {
    message('Arquivo(s) para a cidade ', sigla_muni, " já existe(m), pulando...\n")
  }
  
}


renda_de_setor_p_grade_muni_2 <- function(ano, sigla_muni) {
  # sigla_muni <- 'nat'; ano <- 2019
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder3 <- sprintf("%s/03_grade_municipios", files_folder)
  subfolder3A <- sprintf("%s/%s", subfolder3, ano)
  subfolder5 <- sprintf("%s/05_setores_agregados", files_folder)
  subfolder5A <- sprintf("%s/%s", subfolder5, ano)
  subfolder7 <- sprintf("%s/XY_grade_municipio_com_renda_cor", files_folder)
  subfolder7A <- sprintf("%s/%s", subfolder7, ano)
  if ("XY_grade_municipio_com_renda_cor" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7)
  }
  if (ano %nin% list.dirs(subfolder7, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7A)
  }
  
  # Status message
  message(Sys.time(), ' - Trabalhando na cidade: ', sigla_muni, '\n')
  
  # Checar se arquivos resultantes já existe. Se sim, avisar e pular a cidade
  out_file1 <- sprintf("tmp1_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file2 <- sprintf("tmp2_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file3 <- sprintf("grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  
  if (out_file1 %in% list.files(subfolder7A) & out_file2 %nin% list.files(subfolder7A) & out_file3 %nin% list.files(subfolder7A)){
    # Caminho para os arquivos
    path_setor <- sprintf("%s/setores_agregados_%s_%s.rds", subfolder5A, sigla_muni, ano)
    path_grade_corrigida <- sprintf("%s/%s", subfolder7A, out_file1)
    
    # Ler shapes de setores censitários e grades estatísticas
    setor <- readr::read_rds(path_setor)
    grade_corrigida <- readr::read_rds(path_grade_corrigida)
    
    # Status message
    etapa1 <- Sys.time()
    message('\n', etapa1, ' - 1-2: Começando preparo de base e cálculo de área geral em grade_corrigida em: ', sigla_muni, '\n')
    
    # Fazer o group_by() aqui vai dar um erro 'Assigned data `geom` must be 
    # compatible with existing data', dizendo que os dados existentes possuem
    # x linhas e os 'assigned data' possuem x+1 linhas. Vamos fazer esses
    # cálculos como um dataframe e depois o reassociaremos ao objeto 'grade_corrigida'
    grade_corrigida_df <-
      grade_corrigida %>% 
      as.data.frame() %>% 
      group_by(id_grade) %>%
      summarise(pop_total = first(POP),
                pop_homens = first(MASC),
                pop_mulheres = first(FEM),
                area_antes = first(area_antes))
    
    # Juntar resultados de volta ao objeto 'grade_corrigida' - uma vez que temos
    # uma nova coluna de 'area_antes', vamos descartar a original
    grade_corrigida <- 
      grade_corrigida %>% 
      dplyr::select(-area_antes) %>% 
      left_join(grade_corrigida_df, by = 'id_grade')
    rm(grade_corrigida_df)
    
    # Corrigir população das grades de borda que foram cortadas porque parte 
    # da grade eventualmente cai fora do município
    grade_corrigida <- 
      grade_corrigida %>%    
      mutate(area_depois = as.numeric(st_area(.))) %>%
      mutate(prop = area_depois/area_antes) %>%
      mutate(pop_total = prop * pop_total,
             pop_homens = prop * pop_homens,
             pop_mulheres = prop * pop_mulheres)
    
    # Selecionar colunas da GRADE
    grade_corrigida <- 
      grade_corrigida %>%
      rename(area_grade = area_depois) %>%
      dplyr::select(id_grade, pop_total, pop_homens, pop_mulheres, area_grade) %>% 
      arrange(id_grade)
    
    
    # Status message
    etapa2 <- Sys.time()
    message('\n', etapa2, ' - 2-3: Começando cálculo de área geral em setor em : ', sigla_muni, '\n')
    
    # Criar id único de cada setor e filtrar colunas DO SETOR
    # calcula area de cada setor
    setor <- 
      setor %>%
      mutate(id_setor = 1:n()) %>%
      mutate(area_setor = st_area(.)) %>%
      dplyr::select(id_setor, renda_total, area_setor, 
                    matches("moradores_SM"), # domicilios por renda
                    matches("cor_"), # cores
                    matches("idade")) # idade
    
    
    # agrega cor negra
    setDT(setor)[, cor_negra := cor_preta + cor_parda ]
    setor[, c('cor_preta', 'cor_parda') := NULL]
    
    # Calcular a proporção de cada cor em cada setor censitário - aqui, é 
    # calculada a proporção que cada segmento de população tem em relação à 
    # população dentro do próprio setor. A variável renda total não está aqui 
    # porque ela já representa o total da renda, não sendo segmentada
    setDT(setor)[,  pop_total := sum(cor_branca, cor_amarela, cor_indigena, cor_negra),  by=id_setor]
    setor[,  ":="(
      # renda
      moradores_SM_0_1Q = moradores_SM_0_1Q/pop_total,
      moradores_SM_1Q_1M = moradores_SM_1Q_1M/pop_total,
      moradores_SM_1M_1 = moradores_SM_1M_1/pop_total,
      moradores_SM_1_2 = moradores_SM_1_2/pop_total,
      moradores_SM_2 = moradores_SM_2/pop_total,
      
      # cor
      cor_b_prop = cor_branca/pop_total,
      cor_a_prop = cor_amarela/pop_total,
      cor_i_prop = cor_indigena/pop_total,
      cor_n_prop = cor_negra/pop_total,
      
      # idade
      idade_1_prop = idade_0a5/pop_total,
      idade_2_prop = idade_6a14/pop_total,
      idade_3_prop = idade_15a18/pop_total,
      idade_4_prop = idade_19a24/pop_total,
      idade_5_prop = idade_25a39/pop_total,
      idade_6_prop = idade_40a69/pop_total,
      idade_7_prop = idade_70/pop_total
    ), 
    
    by=id_setor]
    
    # volta para sf
    setor <- st_sf(setor)
    # head(setor, 3)
    
    
    # Status message
    etapa3 <- Sys.time()
    message('\n', etapa3, ' - 3-3: Começando st_intersection() grade_corrigida vs setor em: ', sigla_muni, '\n')
    
    # Função de reaportion com duas variáveis de referência (população e área);
    # resultado (ui_fim) é uma grade estatística com informação de renda 
    # inputada a partir do setor censitário
    
    ### aplicacao para renda --------------------------
    # tip from https://rpubs.com/rural_gis/255550
    ui <- sf::st_intersection(grade_corrigida, setor)
    
    
    # Salvar arquivo temporário em disco
    write_rds(ui, sprintf("%s/%s", subfolder7A, out_file2), compress = 'gz')
    
    # Status message
    etapa4 <- Sys.time()
    message('\n', sigla_muni, ' temporário 2 finalizado. Tempos:\n')
    message('Etapa 1: ', round(etapa2-etapa1, 1), '\nEtapa 2: ', round(etapa3-etapa2, 1), '\nEtapa 3: ', round(etapa4-etapa3, 1), '\n')
    beep(sound = 1)
    
  } else {
    message('Arquivo(s) para a cidade ', sigla_muni, " já existe(m), pulando...\n")
  }
  
}


renda_de_setor_p_grade_muni_3 <- function(ano, sigla_muni) {
  # sigla_muni <- 'nat'; ano <- 2019
  
  # Estrutura de pastas
  files_folder <- "../../indice-mobilidade_dados"
  subfolder7 <- sprintf("%s/XY_grade_municipio_com_renda_cor", files_folder)
  subfolder7A <- sprintf("%s/%s", subfolder7, ano)
  if ("XY_grade_municipio_com_renda_cor" %nin% list.dirs(files_folder, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7)
  }
  if (ano %nin% list.dirs(subfolder7, recursive = FALSE, full.names = FALSE)){
    dir.create(subfolder7A)
  }
  
  # status message
  message(Sys.time(), ' - Trabalhando na cidade: ', sigla_muni, '\n')
  
  # Checar se arquivos resultantes já existe. Se sim, avisar e pular a cidade
  out_file1 <- sprintf("tmp1_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file2 <- sprintf("tmp2_grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  out_file3 <- sprintf("grade_renda_cor_%s_%s.rds", sigla_muni, ano)
  
  if (out_file2 %in% list.files(subfolder7A) & out_file3 %nin% list.files(subfolder7A)){
    # Ler arquivo temporário, resultado do processo anterior
    path_ui <- sprintf("%s/%s", subfolder7A, out_file2)
    ui <- readr::read_rds(path_ui)
    
    # Status message
    etapa1 <- Sys.time()
    message('\n', etapa1, ' - 1-2: Começando st_make_valid() em: ', sigla_muni, '\n')
    
    # O shape possui várias features com geometria inválida, transformá-las
    # antes de prosseguir
    ui <- st_make_valid(ui)
    
    # Status message
    etapa2 <- Sys.time()
    message('\n', etapa2, ' - 2-2: Começando cálculo de área do pedaço em: ', sigla_muni, '\n')
    
    ui <- ui %>%
      # Calcular a area de cada pedaco
      dplyr::mutate(area_pedaco = st_area(.)) %>%
      
      # Calcular a proporcao de cada setor que esta naquele pedaco (essa sera a area a ponderar pela renda)
      dplyr::mutate(area_prop_setor = area_pedaco/area_setor) %>%
      
      # Calcular a proporcao de cada grade que esta naquele pedacao
      dplyr::mutate(area_prop_grade =  area_pedaco/area_grade) %>%
      
      # Calcular a quantidade (absoluto) de populacao em cada pedaco (baseado na grade)
      dplyr::mutate(pop_sub_grade = pop_total * area_prop_grade) %>%
      
      # Calcular a populacao do setor somando-se a pop das grades
      # necessario pq populacao da grade nao bate 100% com po do setor
      group_by(id_setor) %>%
      dplyr::mutate(total_pop_setor = sum(pop_sub_grade, na.rm = TRUE)) %>%
      ungroup() %>%
      
      # Calcular a populacao proporcional de cada pedaco dentro do setor
      dplyr::mutate(pop_prop_grade_no_setor =  pop_sub_grade/total_pop_setor) %>%
      
      # Calcular a renda dentro de cada pedaco
      # assume que renda do setor eh distribuida igualmente para cada pessoa dentro do setor
      dplyr::mutate(renda_pedaco = renda_total * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_0_1Q_pedaco = moradores_SM_0_1Q * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1Q_1M_pedaco = moradores_SM_1Q_1M * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1M_1_pedaco = moradores_SM_1M_1 * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_1_2_pedaco = moradores_SM_1_2 * pop_prop_grade_no_setor) %>%
      # dplyr::mutate(moradores_SM_2_pedaco = moradores_SM_2 * pop_prop_grade_no_setor) %>%
      
      # Calcular cor/raca dentro de cada pedaco
      # como essas variaveis estao agora como proporcoes
      dplyr::mutate(branca_pedaco = cor_b_prop * pop_sub_grade) %>%
      dplyr::mutate(amarela_pedaco = cor_a_prop * pop_sub_grade) %>%
      dplyr::mutate(indigena_pedaco = cor_i_prop * pop_sub_grade) %>%
      dplyr::mutate(negra_pedaco = cor_n_prop * pop_sub_grade) %>%
      
      ## exemplo visual para entender o que esta sendo feito
      # subset(ui, id_grade %in% c(1306) ) %>% select(., id_grade, cor_b_prop, area_prop_grade, pop_total)
      
      # Calcular proporcionais para idade
      dplyr::mutate(idade_1_pedaco = idade_1_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_2_pedaco = idade_2_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_3_pedaco = idade_3_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_4_pedaco = idade_4_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_5_pedaco = idade_5_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_6_pedaco = idade_6_prop * pop_sub_grade) %>%
      dplyr::mutate(idade_7_pedaco = idade_7_prop * pop_sub_grade)
    
    # # Grand Finale (uniao dos pedacos) - Agrupar por grade e somar a renda
    # ui_fim <- ui %>%
    #   st_set_geometry(NULL) %>%
    #   group_by(id_grade, pop_total, pop_homens, pop_mulheres) %>%
    #   dplyr::summarise(
    #     # renda
    #     renda = sum(renda_pedaco, na.rm = TRUE),
    #     # moradores_SM_0_1Q = sum(moradores_SM_0_1Q_pedaco, na.rm = TRUE),
    #     # moradores_SM_1Q_1M = sum(moradores_SM_1Q_1M_pedaco, na.rm = TRUE),
    #     # moradores_SM_1M_1 = sum(moradores_SM_1M_1_pedaco, na.rm = TRUE),
    #     # moradores_SM_1_2 = sum(moradores_SM_1_2_pedaco, na.rm = TRUE),
    #     # moradores_SM_2 = sum(moradores_SM_2_pedaco, na.rm = TRUE),
    #     # cor
    #     cor_branca = as.numeric(sum(branca_pedaco, na.rm = TRUE)),
    #     cor_amarela = as.numeric(sum(amarela_pedaco, na.rm = TRUE)),
    #     cor_indigena = as.numeric(sum(indigena_pedaco, na.rm = TRUE)),
    #     cor_negra = as.numeric(sum(negra_pedaco, na.rm = TRUE)),
    #     # para idade
    #     idade_0a5   = as.numeric(sum(idade_1_pedaco, na.rm = TRUE)),
    #     idade_6a14  = as.numeric(sum(idade_2_pedaco, na.rm = TRUE)),
    #     idade_15a18 = as.numeric(sum(idade_3_pedaco, na.rm = TRUE)),
    #     idade_19a24 = as.numeric(sum(idade_4_pedaco, na.rm = TRUE)),
    #     idade_25a39 = as.numeric(sum(idade_5_pedaco, na.rm = TRUE)),
    #     idade_40a69 = as.numeric(sum(idade_6_pedaco, na.rm = TRUE)),
    #     idade_70    = as.numeric(sum(idade_7_pedaco, na.rm = TRUE))
    #   ) %>%
    #   dplyr::mutate(renda = as.numeric(renda)) %>%
    #   ungroup()
    #    
    # ui_fim_sf <- grade_corrigida %>%
    #   dplyr::select(id_grade) %>%
    #   left_join(ui_fim, by = "id_grade") %>%
    #   # arredodandar valores
    #   mutate_at(vars(matches("pop|renda|moradores|cor|idade")), round)
    
    
    
    # Renomear as colunas 
    ui_fim_sf <- ui %>%
      dplyr::select(
        id_grade, pop_total, pop_homens, pop_mulheres,
        # renda
        renda = renda_pedaco,
        # moradores_SM_0_1Q = moradores_SM_0_1Q_pedaco,
        # moradores_SM_1Q_1M = moradores_SM_1Q_1M_pedaco,
        # moradores_SM_1M_1 = moradores_SM_1M_1_pedaco,
        # moradores_SM_1_2 = moradores_SM_1_2_pedaco,
        # moradores_SM_2 = moradores_SM_2_pedaco,
        
        # cor
        cor_branca = branca_pedaco,
        cor_amarela = amarela_pedaco,
        cor_indigena = indigena_pedaco,
        cor_negra = negra_pedaco,
        
        # para idade
        idade_0a5   = idade_1_pedaco,
        idade_6a14  = idade_2_pedaco,
        idade_15a18 = idade_3_pedaco,
        idade_19a24 = idade_4_pedaco,
        idade_25a39 = idade_5_pedaco,
        idade_40a69 = idade_6_pedaco,
        idade_70    = idade_7_pedaco
      )
    
    # Salvar em disco
    write_rds(ui_fim_sf, sprintf("%s/%s", subfolder7A, out_file3), compress = 'gz')
    
    # Status message
    etapa3 <- Sys.time()
    message('\n', sigla_muni, ' finalizado. Tempos:\nEtapa 1: ', round(etapa2-etapa1, 1), '\nEtapa 2: ', round(etapa3-etapa2, 1), '\n')
    beep(sound = 1)
    
  } else {
    message('Arquivo final para a cidade ', sigla_muni, " já existe, pulando...\n")
  }
  
}


# Agregar informações dos setores censitários (renda, idade) para a grade 
# estatística - usar uma sigla das presentes em:
# munis_list$munis_metro[ano_metro == ano]$abrev_muni ou 'all' para todos
# "bho" "cam" "cgr" "cur" "for" "goi" "mac" "man" "nat" "rec" "rio" "sal" "spo" 
# "tsa" "jpa" "ula" "vta" "oco" "sne" "sjc" "lda"
# renda_de_setor_p_grade(ano = 2019, munis = 'oco')
# renda_de_setor_p_grade(ano = 2019, munis = 'all')

# É importante mencionar que os processos de st_...() exigem bastante memória 
# RAM, em especial para estados grandes. Monitorar. Talvez seja preciso rodar
# por cidade e reiniciar a sessão como com .rs.restartR() após cada cidade

muni <- ''
renda_de_setor_p_grade_muni_1(ano = 2019, sigla_muni = muni)

renda_de_setor_p_grade_muni_2(ano = 2019, sigla_muni = muni)

renda_de_setor_p_grade_muni_3(ano = 2019, sigla_muni = muni)

# .rs.restartR()
# "vta" "oco" "lda" "nat" "ula" "jpa" "tsa"
# "bho" "cam" "cgr" "cur" "for" "goi" "mac" "man" "rec" "rio" "sal" "spo" 
# "sne" "sjc"