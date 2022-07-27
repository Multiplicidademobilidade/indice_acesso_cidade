# Esse script é utilizado para gerar os mapas utilizados no relatorio

# Setup
source('fun/setup.R')

# Funcoes:
# criar_mapas_acesso <- linha 23
# criar_mapas_oportunidades <- linha 328
# Criar mapas populacao <- linha 513

# Cores e configuracoes
tmaptools::palette_explorer()
paleta <- c('#38040E', '#F4442E', '#FFC145', '#05E4FF', '#6184D8') 
paleta_NA <- '#DCDCDC'

tmap_options(
  output.format = "png",
  output.dpi = 500
)

# ----- FUNCAO POPULACAO -----

criar_mapas_populacao <- function(muni, formato, res){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  # Cores
  #paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65')
  paleta <- c('#e7eff0', '#b6ced1', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas/2019/", files_folder)
  save_folder <- sprintf("%s/%s/Populacao", subfolder19, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))
  
  file$pop_total <- ifelse(file$pop_total > 0, file$pop_total, NA)
  file$perc_negra <- (file$cor_negra/file$pop_total)*100 
  
  graficos <- c("total", "negra")
  
  # 3.0 Gera graficos e salva
  
  for (grafico in graficos) {
    if (grafico == "total"){
      main_title <- 'População'        # main_title em tm_layout
      coluna <- "pop_total"                       # col em tm_fill
      subtitulo <- "Número de habitantes"   # title em tm_fill
      estilo <- "quantile"                                # style em tm_fill
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = FALSE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 1,
                  inner.margins = c(0.02, 0.02, 0.02, 0.3))+
        tm_fill(col = coluna, 
                n=4,
                #breaks = breaks,
                style = estilo,
                palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"),
                legend.position = c('left','center'))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
      
    }else{
      main_title <- "População negra"
      coluna <- "perc_negra"
      subtitulo <- "Percentual por hexágono"
      estilo <- "equal"
      breaks = c(0,20,40,60,80,100)
      
      plot <- tm_shape(file) + 
        tm_layout(main.title = main_title, legend.outside = FALSE, frame = FALSE, 
                  #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                  title.size = 1, legend.title.size = 1,
                  inner.margins = c(0.02, 0.02, 0.02, 0.3))+
        tm_fill(col = coluna, 
                n=5,
                breaks = breaks,
                style = 'fixed',
                palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
                title = subtitulo,
                legend.format = list(text.separator="a", scientific=TRUE, format="f"),
                legend.position = c('left','center'))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
      #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
      #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/pop_%s_%s_%s_2019.%s", save_folder, grafico, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
    
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'cgr'


for (muni in munis){
  criar_mapas_populacao(muni = muni, res = "08", formato = "png")
}

criar_mapas_quantil <- function(muni, res = '08', formato = 'png'){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas/2019", files_folder)
  save_folder <- sprintf("%s/%s/Populacao", subfolder19, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))
  
  file <- file %>% mutate(perc_pop_negra = cor_negra / pop_total)
  quantis_pop_negra <- quantile(file$perc_pop_negra, na.rm=TRUE)
  
  # Executar as categorizações
  file <- file %>% 
    mutate(cat_pop_negra = case_when(perc_pop_negra >= quantis_pop_negra[4] ~ 4, # 75% ou mais
                                     perc_pop_negra >= quantis_pop_negra[3] & perc_pop_negra < quantis_pop_negra[4] ~ 3, 
                                     perc_pop_negra >= quantis_pop_negra[2] & perc_pop_negra < quantis_pop_negra[3] ~ 2,
                                     perc_pop_negra >= quantis_pop_negra[1] & perc_pop_negra < quantis_pop_negra[2] ~ 1))
  
  main_title <- "População negra"
  coluna <- "cat_pop_negra"
  subtitulo <- "Quantil por hexágono"
  estilo <- "equal"
  breaks = c(1,2,3,4)
  
  plot <- tm_shape(file) + 
    tm_layout(main.title = main_title,
              main.title.position = c("center", "TOP"),
              legend.outside = FALSE, frame = FALSE,
              title.size = 1, legend.title.size = 1,
              inner.margins = c(0.02, 0.02, 0.02, 0.4))+
    tm_fill(col = coluna, 
            n=4,
            breaks = breaks,
            style = 'cat',
            palette = paleta, colorNA = paleta_NA, textNA = "Sem população", 
            title = subtitulo,
            legend.format = list(text.separator="a", scientific=TRUE, format="f"),
            legend.position = c('left','center'))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5)#, position=c("right", "bottom"))#+
  
  file_name <- sprintf("%s/pop_negra_q_%s_%s_2019.%s", save_folder, muni, res, formato)
  tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
  
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'cgr'


for (muni in munis){
  criar_mapas_quantil(muni = muni, res = "08", formato = "png")
}


# ----- FUNCAO OPORTUNIDADES -----

criar_mapas_oportunidades <- function(muni, formato='png', res='08'){
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  #modes <- c('walk', 'bike', 'transit')
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas/2019", files_folder)
  save_folder <- sprintf("%s/%s/Oportunidades", subfolder19, muni)
  
  # !! adicionar um condicional
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos
  file <- readRDS(sprintf("%s/hex_agregado_%s_%s_2019.rds", subfolder14, muni, res))

  tt_total <- sum(file$empregos_total)
  st_total <- sum(file$saude_total)
  et_total <- sum(file$edu_total)
  
  file$empregos_total <- ifelse(file$empregos_total > 0, file$empregos_total, NA)
  file$saude_total <- ifelse(file$saude_total > 0, file$saude_total, NA)
  file$edu_total <- ifelse(file$edu_total > 0, file$edu_total, NA)

  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      main_title <- 'Trabalho'        # main_title em tm_layout
      coluna <- "empregos_total"                       # col em tm_fill
      subtitulo <- "Número de empregos"   # title em tm_fill
      estilo <- "jenks"                                # style em tm_fill
    } else if (oport == "saude"){
      main_title <- "Saúde"
      coluna <- "saude_total"
      subtitulo <- "Número de estabelecimentos"
      estilo <- "jenks"
    }else{
      main_title <- "Educação"
      coluna <- "edu_total"
      subtitulo <- "Número de estabelecimentos"
      estilo <- "jenks"

    }
    
  plot <- tm_shape(file) + 
      tm_layout(main.title = main_title,
                main.title.position = c("center","TOP"),
                legend.outside = FALSE, frame = FALSE, 
                #panel.labels = c('At?? 15 minutos','At?? 30 minutos','At?? 45 minutos','At?? 60 minutos'),
                title.size = 1, legend.title.size = 1,
                inner.margins = c(0.02, 0.02, 0.02, 0.4))+
      tm_fill(col = coluna, 
              n=4,
              #breaks = c(0,1,5,10,20,70),
              style = estilo,
              palette = paleta, colorNA = paleta_NA, textNA = "Zero", 
              title = subtitulo,
              legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
      tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))#+
    #tm_facets(by = "OPORT", nrow=1, free.coords = FALSE)+
    #tm_borders(alpha = 1)
      
      file_name <- sprintf("%s/oport_%s_%s_%s_2019.%s", save_folder, oport, muni, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", 'jpa', 'tsa','vta')

munis <- c("bho")

for (muni in munis){
  criar_mapas_oportunidades(muni = muni, formato = "png")
}
