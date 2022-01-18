#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.4 Extrai grade estatistica de cada municipio

# carregar bibliotecas
source('fun/setup.R')

# função do geobr para dissolver polígonos
source("fun/dissolve_polygons.R")
options(future.availableCores.methods = "mc.cores")
options(mc.cores = 8)


### Funcao
# munis <- sigla <- "for"
criar_grade_muni_all <- function(ano, munis = "all") {
  # Criar pastas para armazenamento dos dados (original era 'data-raw'); a estrutura é:
  # ../../-mobilidade_dados/grade_municipios/[ano]/arquivos_municipios.rds
  files_folder <- "../../indice-mobilidade_dados"
  subfolder1 <- sprintf("%s/01_municipios", files_folder)
  subfolder1A <- sprintf("%s/%s", subfolder1, ano)
  subfolder3 <- sprintf("%s/03_grade_municipios", files_folder)
  subfolder3A <- sprintf("%s/%s", subfolder3, ano)
  dir.create(sprintf("%s", subfolder3A), recursive = TRUE, showWarnings = FALSE)
  
  # funcao para criar grade por municipio
  criar_grade_muni <- function(sigla){
    
    message('\nRodando cidade: ', sigla,"\n")
    
    # Checar se arquivo resultante já existe. Se sim, avisar e pular a cidade
    out_file <- sprintf("grade_%s_%s.rds", sigla, ano)
    
    if (out_file %nin% list.files(subfolder3A)){
      # Obter código do estado do município
      cod_estado <- 
        subset(munis_list$munis_df, abrev_muni==sigla)$abrev_estado %>% 
        as.character()
      
      # Ler as grades estatísticas dos estados
      grade <- read_statistical_grid(code_grid = cod_estado, year = 2010)
      # Estabelecer a projeção em WGS84
      grade <- grade %>% st_transform(crs = 4326)
      
      # Leitura do(s) municipio(s)
      muni <- readr::read_rds(sprintf("%s/municipio_%s_%s.rds", subfolder1A, sigla, ano) )
      # Dissolver os polígonos dos municípios componentes da RM
      # muni <- dissolve_polygons(muni, group_column = "code_state")
      # Estabelecer a projeção em WGS84
      muni <- muni %>% st_transform(crs = 4326)
      
      # Atribuir dados estatísticos a partir do shape do município; st_join() com
      # a opção 'largest = TRUE' faz com que os dados da grade sejam atribuídos de
      # acordo com a sobreposição que possem com o shape do município (ver help)
      # do st_join() e https://github.com/r-spatial/sf/issues/578. Este processo
      # é o mais demorado da função
      grade_muni <- st_join(grade, muni, largest = TRUE)
  
      # Manter apenas grades relacionadas ao município (em que code_state não é
      # nulo). O setDT faz essa transformação diretamente, sem precisar de uma
      # cópia do dataframe, o que requereria o dobro da memória
      grade_muni <- setDT(grade_muni)[!is.na(code_state)]
      
      # Transformar de volta para sf
      grade_muni <- st_sf(grade_muni, crs = 4326)
      
      # Limpar memória com garbage collection
      rm(grade, muni)
      gc(verbose = TRUE)
  
      # Gravar arquivo
      write_rds(grade_muni, sprintf("%s/%s", subfolder3A, out_file), compress = 'gz')
      
    } else {
        message('Arquivo para a cidade ', sigla, " já existe, pulando...\n")
    }
    
  }

  # Aplicar função
  if (munis == "all") {
    # seleciona todos municipios ou RMs do ano escolhido
    x = munis_list$munis_metro[ano_metro == ano]$abrev_muni
  } else (x = munis)

  # Parallel processing using future.apply
  # future::plan(multiprocess) to be deprecated; use 'multisession' / 'multicore' instead
  # https://github.com/sjspielman/dragon/issues/43
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  invisible(future.apply::future_lapply(X = x, FUN=criar_grade_muni, future.packages=c('sf', 'dplyr')))

}


# Extrair grade estatística de cada município - usar uma sigla das presentes
# em munis_list$munis_metro[ano_metro == ano]$abrev_muni ou 'all' para todos
# criar_grade_muni_all(ano = 2019, munis = 'sgo')
# criar_grade_muni_all(ano = 2019, munis = 'all')

# É importante mencionar que o processo do st_join() existente na função exige
# muita memória RAM, em especial para estados grandes. Um estado como o de MG
# usou 16 GB de RAM + 9 GB de swap. Além disso, mesmo as limpezas de memória
# previstas com o gc() no código não liberam a RAM usada pelo R-Studio, o que
# torna necessário reiniciar a sessão como um todo com .rs.restartR() após
# rodar cada cidade
criar_grade_muni_all(ano = 2019, munis = 'vta')
# .rs.restartR()
