# Esse script é utilizado para gerar os mapas utilizados no relatorio

# PARTE 3

# Setup
source('fun/setup.R')

# Cores e configuracoes
tmaptools::palette_explorer()
paleta_NA <- '#DCDCDC'

tmap_options(
  output.format = "png",
  output.dpi = 500
)

# ----- FUNCAO ACESSO OPORTUNIDADES -----

criar_mapas_acesso <- function(muni, formato){
  # Classifica o acesso em fatores:
  # 0
  # até 1
  # 1 a 5
  # 6 a 10
  # mais de 10
  
  # 0.0 Configuracoes iniciais
  # Plotagem
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  
  oportunidades <- c('trabalho', 'saude', 'educacao')
  modes <- c('ride_hailing', 'transit', 'walk', 'bike') # Fazendo para o auto
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Acessibilidade", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)
  
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  # 1.0 Carrega arquivos 
  # res 7
  file_aux_7 <- readRDS(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
  file_7_car <- readRDS(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))
  file_7_bus <- readRDS(sprintf("%s/acess_07_%s_onibus_2019.rds", subfolder17, muni))
  # res 8
  file_aux_8 <- readRDS(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))
  file_8 <- readRDS(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))
  
  # Total de estabelecimentos e empregos
  # (tanto faz usar a res 7 ou 8 para somar)
  tt_total <- sum(file_aux_7$empregos_total)
  st_total <- sum(file_aux_7$saude_total)
  et_total <- sum(file_aux_7$edu_total)
  
  data_plot_7_car <- st_join(file_aux_7, file_7_car, join=st_equals)
  data_plot_7_bus <- st_join(file_aux_7, file_7_bus, join=st_equals)
  data_plot_8 <- st_join(file_aux_8, file_8, join=st_equals)
  
  # 2.0 Prepara bases
  # Trabalharemos com percentuais - RES 7
  # Trabalho
  data_plot_7_car$CMATT30_p <- (data_plot_7_car$CMATT30 / tt_total)*100
  data_plot_7_bus$CMATT60_p <- (data_plot_7_bus$CMATT60 / tt_total)*100
  # Saude
  #  data_plot_7$CMAST30_p <- (data_plot_7$CMAST30 / st_total)*100
  #  data_plot_7$CMAST60_p <- (data_plot_7$CMAST60 / st_total)*100
  # Educacao
  #  data_plot_7$CMAET30_p <- (data_plot_7$CMAET30 / et_total)*100
  #  data_plot_7$CMAET60_p <- (data_plot_7$CMAET60 / et_total)*100
  
  # Trabalharemos com percentuais - RES 8
  # Trabalho
  data_plot_8$CMATT30_p <- (data_plot_8$CMATT30 / tt_total)*100
  #  # Saude
  #  data_plot_8$CMAST30_p <- (data_plot_8$CMAST30 / st_total)*100
  #  # Educacao
  #  data_plot_8$CMAET30_p <- (data_plot_8$CMAET30 / et_total)*100
  
  # transforma acesso a saude e educacao em fatores
  cmaf_labels <- c('Zero', '1', '2 a 5', '6 a 10', '10 ou mais')
  # RES 8
  data_plot_8$CMAST30_f <-  with(data_plot_8, ifelse(CMAST30 == 0, 1,
                                                     ifelse(between(CMAST30, 0, 1), 2,
                                                            ifelse(between(CMAST30,2,5), 3,
                                                                   ifelse(between(CMAST30,6,10), 4, 5)))))
  data_plot_8$CMAST30_f <- factor(data_plot_8$CMAST30_f, levels = 1:5, labels = cmaf_labels)
  data_plot_8$CMAET30_f <-  with(data_plot_8, ifelse(CMAET30 == 0, 1,
                                                     ifelse(between(CMAET30, 0, 1), 2,
                                                            ifelse(between(CMAET30,2,5), 3,
                                                                   ifelse(between(CMAET30,6,10), 4, 5)))))
  data_plot_8$CMAET30_f <- factor(data_plot_8$CMAET30_f, levels = 1:5, labels = cmaf_labels)
  # RES 7
  data_plot_7_car$CMAST30_f <-  with(data_plot_7_car, ifelse(CMAST30 == 0, 1,
                                                             ifelse(between(CMAST30, 0, 1), 2,
                                                                    ifelse(between(CMAST30,2,5), 3,
                                                                           ifelse(between(CMAST30,6,10), 4, 5)))))
  data_plot_7_car$CMAST30_f <- factor(data_plot_7_car$CMAST30_f, levels = 1:5, labels = cmaf_labels)
  data_plot_7_bus$CMAST60_f <-  with(data_plot_7_bus, ifelse(CMAST60 == 0, 1,
                                                             ifelse(between(CMAST60, 0, 1), 2,
                                                                    ifelse(between(CMAST60,2,5), 3,
                                                                           ifelse(between(CMAST60,6,10), 4, 5)))))
  data_plot_7_bus$CMAST60_f <- factor(data_plot_7_bus$CMAST60_f, levels = 1:5, labels = cmaf_labels)
  data_plot_7_car$CMAET30_f <-  with(data_plot_7_car, ifelse(CMAET30 == 0, 1,
                                                             ifelse(between(CMAET30, 0, 1), 2,
                                                                    ifelse(between(CMAET30,2,5), 3,
                                                                           ifelse(between(CMAET30,6,10), 4, 5)))))
  data_plot_7_car$CMAET30_f <- factor(data_plot_7_car$CMAET30_f, levels = 1:5, labels = cmaf_labels)
  data_plot_7_bus$CMAET60_f <-  with(data_plot_7_bus, ifelse(CMAET60 == 0, 1,
                                                             ifelse(between(CMAET60, 0, 1), 2,
                                                                    ifelse(between(CMAET60,2,5), 3,
                                                                           ifelse(between(CMAET60,6,10), 4, 5)))))
  data_plot_7_bus$CMAET60_f <- factor(data_plot_7_bus$CMAET60_f, levels = 1:5, labels = cmaf_labels)
  
  # 3.0 Gera graficos e salva
  
  for (oport in oportunidades) {
    if (oport == "trabalho"){
      titulo_aux1 <- "ao trabalho"
      col_30 <- c('origin', 'mode', 'CMATT30_p')
      col_60 <- c('origin', 'mode', 'CMATT60_p') 
      style <- 'fixed'
      breaks <-  c(0,20,40,60,80,100)
    } else if (oport == "saude"){
      titulo_aux1 <- "à saúde"
      col_30 <- c('origin', 'mode', 'CMAST30_f')
      col_60 <- c('origin', 'mode', 'CMAST60_f')
      style <- 'cat'
      breaks <- NULL
    }else{
      titulo_aux1 <- "à educação"
      col_30 <- c('origin', 'mode', 'CMAET30_f')
      col_60 <- c('origin', 'mode', 'CMAET60_f')
      style <- 'cat'
      breaks <- NULL
    }
    
    for (modo in modes){
      
      if (modo == "walk"){
        titulo_aux2 <- "a Pé"
        titulo <- sprintf("Acesso %s \n%s", titulo_aux1, titulo_aux2)
        dados <- data_plot_8%>%
          dplyr::select(col_30)%>%
          rename(CMA=3)
      }
      else if (modo == "bike"){
        titulo_aux2 <- "Bicicleta"
        titulo <- sprintf("Acesso %s \n%s", titulo_aux1, titulo_aux2)
        dados <- data_plot_8%>%
          dplyr::select(col_30)%>%
          rename(CMA=3)
      }
      else if (modo == 'ride_hailing'){
        titulo_aux2 <- "Carro compartilhado"
        titulo <- sprintf("Acesso %s \n%s", titulo_aux1, titulo_aux2)
        dados <- data_plot_7_car%>%
          dplyr::select(col_30)%>%
          rename(CMA=3)
      } else{
        titulo_aux2 <- "Ônibus"
        titulo <- sprintf("Acesso %s \n%s", titulo_aux1, titulo_aux2)
        dados <- data_plot_7_bus%>%
          dplyr::select(col_60)%>%
          rename(CMA=3)
        #pivot_longer(pivot_motor, names_to="CMA", values_to="Total")
      }
      
      plot <- dplyr::filter(dados, mode %in% c(modo, NA))%>%
        tm_shape()+ 
        tm_layout(main.title = titulo,
                  main.title.size = 1,
                  main.title.position = c("center","TOP"),
                  legend.outside = FALSE,
                  frame = FALSE, 
                  legend.title.size = 1,
                  frame.lwd = NA,
                  inner.margins = c(0.02, 0.02, 0.02, 0.4))+
        tm_fill(col = 'CMA', 
                n=5,
                breaks = breaks, #c(0,20,40,60,80,100),
                style = style,
                palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
                title = "Oportunidades acessíveis",
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("center", "bottom"))+
        tm_legend(position = c('right','center'))
      
      file_name <- sprintf("%s/acess_%s_%s_%s_2019.%s", save_folder, oport, modo, muni, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, width = 1200, units = "px")
    }
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula")

munis <- c('bho')

for (muni in munis){
  criar_mapas_acesso(muni = muni, formato = "png")
}

# ------ CMA/CMA' -----

grafico_cma_entorno <- function(muni, ano=2019, formato){
  
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Acessibilidade", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)

dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)

tmap_options(
  output.format = formato,
  output.dpi = 500
)

modos <- c("carro_compart", "onibus", "walk", "bike")
oportunidades <- c('trabalho', 'saude', 'educacao')

#walk
cma_walk <- read_rds(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
  dplyr::filter(mode=='walk')%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)

cma_ent_walk <-  read_rds(sprintf("%s/acess_ideal_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
  dplyr::filter(mode=='walk')%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
  rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
cma_walk <- st_join(cma_walk, cma_ent_walk, join=st_equals)

cma_walk$CMATT_r <- ifelse(is.na((cma_walk$CMATT30/cma_walk$CMATT30_i)*100),0,(cma_walk$CMATT30/cma_walk$CMATT30_i)*100)
cma_walk$CMAST_r <- ifelse(is.na((cma_walk$CMAST30/cma_walk$CMAST30_i)*100),0,(cma_walk$CMAST30/cma_walk$CMAST30_i)*100)
cma_walk$CMAET_r <- ifelse(is.na((cma_walk$CMAET30/cma_walk$CMAET30_i)*100),0,(cma_walk$CMAET30/cma_walk$CMAET30_i)*100)
# bike
cma_bike <- read_rds(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
  dplyr::filter(mode=='bike')%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)

cma_ent_bike <-  read_rds(sprintf("%s/acess_ideal_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
  dplyr::filter(mode=='bike')%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
  rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
cma_bike <- st_join(cma_bike, cma_ent_bike, join=st_equals)

cma_bike$CMATT_r <- ifelse(is.na((cma_bike$CMATT30/cma_bike$CMATT30_i)*100),0,(cma_bike$CMATT30/cma_bike$CMATT30_i)*100)
cma_bike$CMAST_r <- ifelse(is.na((cma_bike$CMAST30/cma_bike$CMAST30_i)*100),0,(cma_bike$CMAST30/cma_bike$CMAST30_i)*100)
cma_bike$CMAET_r <- ifelse(is.na((cma_bike$CMAET30/cma_bike$CMAET30_i)*100),0,(cma_bike$CMAET30/cma_bike$CMAET30_i)*100)


cma_car <- read_rds(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))%>%
  #dplyr::filter(mode==submodo)%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)

cma_ent_car <-  read_rds(sprintf("%s/acess_ideal_07_%s_carro_compart_2019.rds", subfolder17, muni))%>%
  #dplyr::filter(mode==submodo)%>%
  dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
  rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
cma_car <- st_join(cma_car, cma_ent_car, join=st_equals)

cma_car$CMATT_r <- ifelse(is.na((cma_car$CMATT30/cma_car$CMATT30_i)*100),0,(cma_car$CMATT30/cma_car$CMATT30_i)*100)
cma_car$CMAST_r <- ifelse(is.na((cma_car$CMAST30/cma_car$CMAST30_i)*100),0,(cma_car$CMAST30/cma_car$CMAST30_i)*100)
cma_car$CMAET_r <- ifelse(is.na((cma_car$CMAET30/cma_car$CMAET30_i)*100),0,(cma_car$CMAET30/cma_car$CMAET30_i)*100)

# bus
cma_bus <- read_rds(sprintf("%s/acess_07_%s_onibus_2019.rds", subfolder17, muni))%>%
  dplyr::select(origin, city, mode, CMATT60, CMAST60, CMAET60)
cma_ent_bus <- read_rds(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
viz_bus <- get_kring(h3_address = cma_ent_bus$id_hex, ring_size = 10, simple = TRUE)

# Definir entorno - A partir de cada hexágono, será estabelecido um
# entorno, onde serão agrupados o total de oportunidades por tipo
cma_ent_bus$saud_entorno <- 0
cma_ent_bus$educ_entorno <- 0
cma_ent_bus$trab_entorno <- 0

# Calcular quantidade de oportunidades no entorno de cada hexágono
for (i in 1:nrow(cma_ent_bus)) {
  cma_ent_bus[i,]$saud_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$saude_total)
  cma_ent_bus[i,]$educ_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$edu_total)
  cma_ent_bus[i,]$trab_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$empregos_total)
}

# Substituir valores 0 por 1 nas colunas de saud_entorno, educ_entorno e
# trab_entorno - este passo é necessário devido à divisão a seguir
cma_ent_bus$saud_entorno <- ifelse(cma_ent_bus$saud_entorno == 0, 1, cma_ent_bus$saud_entorno)
cma_ent_bus$educ_entorno <- ifelse(cma_ent_bus$educ_entorno == 0, 1, cma_ent_bus$educ_entorno)
cma_ent_bus$trab_entorno <- ifelse(cma_ent_bus$trab_entorno == 0, 1, cma_ent_bus$trab_entorno)

# Calcular a razão entre CMA para 60 minutos e quantidade de oportunidades no entorno
#  cma$educ_perc <- (cma$CMAET60 / cma$educ_entorno)*100
#  cma$saud_perc <- (cma$CMAST60 / cma$saud_entorno)*100
#  cma$trab_perc <- (cma$CMATT60 / cma$trab_entorno)*100

cma_ent_bus <- cma_ent_bus%>%
  dplyr::select(id_hex, trab_entorno, educ_entorno, saud_entorno)%>%
  rename(origin=id_hex)

cma_bus <- st_join(cma_bus, cma_ent_bus, join=st_equals)

cma_bus$CMATT_r <- ifelse(is.na((cma_bus$CMATT60/cma_bus$trab_entorno)*100),0,(cma_bus$CMATT60/cma_bus$trab_entorno)*100)
cma_bus$CMAST_r <- ifelse(is.na((cma_bus$CMAST60/cma_bus$saud_entorno)*100),0,(cma_bus$CMAST60/cma_bus$saud_entorno)*100)
cma_bus$CMAET_r <- ifelse(is.na((cma_bus$CMAET60/cma_bus$educ_entorno)*100),0,(cma_bus$CMAET60/cma_bus$educ_entorno)*100)

for (oport in oportunidades){
  if (oport == 'trabalho'){
    titulo_1 <- 'ao trabalho'
    col <- 'CMATT_r'
  }else if (oport == 'saude'){
    titulo_1 <- 'à saúde'
    col <- 'CMAST_r'
  }else{
    titulo_1 <- 'à educação'
    col <- 'CMAET_r'
  }
  
  for (modo in modos){
    
    if (modo=='carro_compart'){
      res <- '07'
      df <- cma_car
      titulo_2 <- 'Automóvel por aplicativo'
    }else if (modo=='onibus'){
      res <- '07'
      df <- cma_bus
      titulo_2 <- 'Ônibus'
    }else if (modo=='walk'){
      res <- '08'
      df <- cma_walk
      titulo_2 <- 'A pé'
    }else{
      res <- '08'
      df <- cma_bike
      titulo_2 <- 'Bicicleta'
    }
  
  plot <- df%>%
    tm_shape()+ 
    tm_layout(main.title = sprintf("Acesso %s \n%s", titulo_1, titulo_2), 
              legend.outside = FALSE, frame = FALSE, 
              #panel.labels = panel_labels,
              main.title.size = 1,
              main.title.position = c("center","TOP"),
              legend.title.size = 1,
              frame.lwd = NA,
              #panel.label.bg.color = NA,
              #panel.label.size = 0.8,
              inner.margins = c(0.02, 0.02, 0.02, 0.4))+
    tm_fill(col = col, 
            n=5,
            breaks = c(0,20,40,60,80,100),
            style = 'fixed', #"cat",
            palette = 'Blues', colorNA = "#DCDCDC", textNA = "Sem dados", 
            title = "CMA/CMA' (%)",
            legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
    tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
    #tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
    tm_legend(position = c('right','center'))
  
  file_name <- sprintf("%s/perc_cma_%s_%s_%s_%s_2019.%s", save_folder, muni, oport, modo, res, formato)
  tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'bho'

for (muni in munis){
  grafico_cma_entorno(muni = muni, 
                      formato = "png")
}

# COMP_AOD
# ------ CMA/CMA' * P -----

grafico_comp_aod <- function(muni, ano=2019, formato){
  
  # Cores
  paleta <- c('#e7eff0', '#b6ced1', '#86adb2', '#568c93', '#0d5b65') 
  paleta_NA <- '#808080'
  
  # Estrutura de pasta
  files_folder <- "../../indice_acesso_cidade_dados"
  subfolder14 <- sprintf("%s/14_hex_agregados/2019", files_folder)
  subfolder17 <- sprintf("%s/17_acesso_oportunidades/2019", files_folder)
  subfolder19 <- sprintf("%s/19_mapas_acessibilidades/2019/Comp_AOD", files_folder)
  save_folder <- sprintf("%s/%s", subfolder19, muni)
  
  dir.create(sprintf("%s", save_folder), recursive = TRUE, showWarnings = FALSE)
  
  tmap_options(
    output.format = formato,
    output.dpi = 500
  )
  
  modos <- c("carro_compart", "onibus", "walk", "bike")
  oportunidades <- c('trabalho', 'saude', 'educacao')
  
  agregado_7 <- read_rds(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))%>%
    dplyr::select(id_hex, sigla_muni, pop_total, cor_negra)
  agregado_7 <- agregado_7 %>% mutate(perc_pop_negra = cor_negra / pop_total)
  quantis_pop_negra_7 <- quantile(agregado_7$perc_pop_negra, na.rm=TRUE)
  agregado_7 <- agregado_7 %>% 
    mutate(cat_pop_negra = case_when(perc_pop_negra >= quantis_pop_negra_7[4] ~ 4, # 75% ou mais
                                     perc_pop_negra >= quantis_pop_negra_7[3] & perc_pop_negra < quantis_pop_negra_7[4] ~ 3, 
                                     perc_pop_negra >= quantis_pop_negra_7[2] & perc_pop_negra < quantis_pop_negra_7[3] ~ 2,
                                     TRUE ~1))
  
  agregado_8 <- read_rds(sprintf("%s/hex_agregado_%s_08_2019.rds", subfolder14, muni))%>%
    dplyr::select(id_hex, sigla_muni, pop_total, cor_negra)
  agregado_8 <- agregado_8 %>% mutate(perc_pop_negra = cor_negra / pop_total)
  quantis_pop_negra_8 <- quantile(agregado_8$perc_pop_negra, na.rm=TRUE)
  agregado_8 <- agregado_8 %>% 
    mutate(cat_pop_negra = case_when(perc_pop_negra >= quantis_pop_negra_8[4] ~ 4, # 75% ou mais
                                     perc_pop_negra >= quantis_pop_negra_8[3] & perc_pop_negra < quantis_pop_negra_8[4] ~ 3, 
                                     perc_pop_negra >= quantis_pop_negra_8[2] & perc_pop_negra < quantis_pop_negra_8[3] ~ 2,
                                     TRUE ~1))
  
  #walk
  cma_walk <- read_rds(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
    dplyr::filter(mode=='walk')%>%
    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)
  
  cma_ent_walk <-  read_rds(sprintf("%s/acess_ideal_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
    dplyr::filter(mode=='walk')%>%
    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
    rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
  cma_walk <- st_join(cma_walk, cma_ent_walk, join=st_equals)
  cma_walk <- st_join(cma_walk, agregado_8, join=st_equals)
  
  cma_walk$CMATT_r <- ifelse(is.na((cma_walk$CMATT30/cma_walk$CMATT30_i)),0,(cma_walk$CMATT30/cma_walk$CMATT30_i))
  cma_walk$CMAST_r <- ifelse(is.na((cma_walk$CMAST30/cma_walk$CMAST30_i)),0,(cma_walk$CMAST30/cma_walk$CMAST30_i))
  cma_walk$CMAET_r <- ifelse(is.na((cma_walk$CMAET30/cma_walk$CMAET30_i)),0,(cma_walk$CMAET30/cma_walk$CMAET30_i))
  
  cma_walk$comp_trab <- cma_walk$CMATT_r*cma_walk$cat_pop_negra
  cma_walk$comp_saud <- cma_walk$CMAST_r*cma_walk$cat_pop_negra
  cma_walk$comp_educ <- cma_walk$CMAET_r*cma_walk$cat_pop_negra
  
  # bike
  cma_bike <- read_rds(sprintf("%s/acess_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
    dplyr::filter(mode=='bike')%>%
    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)
  
  cma_ent_bike <-  read_rds(sprintf("%s/acess_ideal_08_%s_modos_ativos_2019.rds", subfolder17, muni))%>%
    dplyr::filter(mode=='bike')%>%
    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
    rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
  cma_bike <- st_join(cma_bike, cma_ent_bike, join=st_equals)
  cma_bike <- st_join(cma_bike, agregado_8, join=st_equals)
  
  cma_bike$CMATT_r <- ifelse(is.na((cma_bike$CMATT30/cma_bike$CMATT30_i)),0,(cma_bike$CMATT30/cma_bike$CMATT30_i))
  cma_bike$CMAST_r <- ifelse(is.na((cma_bike$CMAST30/cma_bike$CMAST30_i)),0,(cma_bike$CMAST30/cma_bike$CMAST30_i))
  cma_bike$CMAET_r <- ifelse(is.na((cma_bike$CMAET30/cma_bike$CMAET30_i)),0,(cma_bike$CMAET30/cma_bike$CMAET30_i))
  
  cma_bike$comp_trab <- cma_bike$CMATT_r*cma_bike$cat_pop_negra
  cma_bike$comp_saud <- cma_bike$CMAST_r*cma_bike$cat_pop_negra
  cma_bike$comp_educ <- cma_bike$CMAET_r*cma_bike$cat_pop_negra
  
  # carro_onibus
  cma_car <- read_rds(sprintf("%s/acess_07_%s_carro_compart_2019.rds", subfolder17, muni))%>%
    #dplyr::filter(mode==submodo)%>%
    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)
  
  cma_ent_car <- read_rds(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
  viz_car <- get_kring(h3_address = cma_ent_car$id_hex, ring_size = 10, simple = TRUE)
  
  cma_ent_car$saud_entorno <- 0
  cma_ent_car$educ_entorno <- 0
  cma_ent_car$trab_entorno <- 0
  
  # Calcular quantidade de oportunidades no entorno de cada hexágono
  for (i in 1:nrow(cma_ent_car)) {
    cma_ent_car[i,]$saud_entorno <- sum(cma_ent_car[cma_ent_car$id_hex %in% viz_car[[i]],]$saude_total)
    cma_ent_car[i,]$educ_entorno <- sum(cma_ent_car[cma_ent_car$id_hex %in% viz_car[[i]],]$edu_total)
    cma_ent_car[i,]$trab_entorno <- sum(cma_ent_car[cma_ent_car$id_hex %in% viz_car[[i]],]$empregos_total)
  }
  
  # Substituir valores 0 por 1 nas colunas de saud_entorno, educ_entorno e
  # trab_entorno - este passo é necessário devido à divisão a seguir
  cma_ent_car$saud_entorno <- ifelse(cma_ent_car$saud_entorno == 0, 1, cma_ent_car$saud_entorno)
  cma_ent_car$educ_entorno <- ifelse(cma_ent_car$educ_entorno == 0, 1, cma_ent_car$educ_entorno)
  cma_ent_car$trab_entorno <- ifelse(cma_ent_car$trab_entorno == 0, 1, cma_ent_car$trab_entorno)
  
#  cma_ent_car <-  read_rds(sprintf("%s/acess_ideal_07_%s_carro_compart_2019.rds", subfolder17, muni))%>%
#    #dplyr::filter(mode==submodo)%>%
#    dplyr::select(origin, city, mode, CMATT15, CMATT30, CMAST15, CMAST30, CMAET15, CMAET30)%>%
#    rename(c(CMATT15_i=CMATT15, CMATT30_i=CMATT30, CMAST15_i=CMAST15, CMAST30_i=CMAST30, CMAET15_i=CMAET15, CMAET30_i=CMAET30))
#  cma_car <- st_join(cma_car, cma_ent_car, join=st_equals)
#  cma_car <- st_join(cma_car, agregado_7, join=st_equals)
  
  cma_ent_car <- cma_ent_car%>%
    dplyr::select(id_hex, trab_entorno, educ_entorno, saud_entorno)%>%
    rename(origin=id_hex)
  
  cma_car <- st_join(cma_car, cma_ent_car, join=st_equals)
  cma_car <- st_join(cma_car, agregado_7, join=st_equals)
  
  cma_car$CMATT_r <- ifelse(is.na((cma_car$CMATT30/cma_car$trab_entorno)),0,(cma_car$CMATT30/cma_car$trab_entorno))
  cma_car$CMAST_r <- ifelse(is.na((cma_car$CMAST30/cma_car$saud_entorno)),0,(cma_car$CMAST30/cma_car$saud_entorno))
  cma_car$CMAET_r <- ifelse(is.na((cma_car$CMAET30/cma_car$educ_entorno)),0,(cma_car$CMAET30/cma_car$educ_entorno))
  
#  cma_car$CMATT_r <- ifelse(is.na((cma_car$CMATT30/cma_car$CMATT30_i)),0,(cma_car$CMATT30/cma_car$CMATT30_i))
#  cma_car$CMAST_r <- ifelse(is.na((cma_car$CMAST30/cma_car$CMAST30_i)),0,(cma_car$CMAST30/cma_car$CMAST30_i))
#  cma_car$CMAET_r <- ifelse(is.na((cma_car$CMAET30/cma_car$CMAET30_i)),0,(cma_car$CMAET30/cma_car$CMAET30_i))
  
  cma_car$comp_trab <- cma_car$CMATT_r*cma_car$cat_pop_negra
  cma_car$comp_saud <- cma_car$CMAST_r*cma_car$cat_pop_negra
  cma_car$comp_educ <- cma_car$CMAET_r*cma_car$cat_pop_negra
  
  # bus
  cma_bus <- read_rds(sprintf("%s/acess_07_%s_onibus_2019.rds", subfolder17, muni))%>%
    dplyr::select(origin, city, mode, CMATT60, CMAST60, CMAET60)
  cma_ent_bus <- read_rds(sprintf("%s/hex_agregado_%s_07_2019.rds", subfolder14, muni))
  viz_bus <- get_kring(h3_address = cma_ent_bus$id_hex, ring_size = 10, simple = TRUE)
  
  # Definir entorno - A partir de cada hexágono, será estabelecido um
  # entorno, onde serão agrupados o total de oportunidades por tipo
  cma_ent_bus$saud_entorno <- 0
  cma_ent_bus$educ_entorno <- 0
  cma_ent_bus$trab_entorno <- 0
  
  # Calcular quantidade de oportunidades no entorno de cada hexágono
  for (i in 1:nrow(cma_ent_bus)) {
    cma_ent_bus[i,]$saud_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$saude_total)
    cma_ent_bus[i,]$educ_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$edu_total)
    cma_ent_bus[i,]$trab_entorno <- sum(cma_ent_bus[cma_ent_bus$id_hex %in% viz_bus[[i]],]$empregos_total)
  }
  
  # Substituir valores 0 por 1 nas colunas de saud_entorno, educ_entorno e
  # trab_entorno - este passo é necessário devido à divisão a seguir
  cma_ent_bus$saud_entorno <- ifelse(cma_ent_bus$saud_entorno == 0, 1, cma_ent_bus$saud_entorno)
  cma_ent_bus$educ_entorno <- ifelse(cma_ent_bus$educ_entorno == 0, 1, cma_ent_bus$educ_entorno)
  cma_ent_bus$trab_entorno <- ifelse(cma_ent_bus$trab_entorno == 0, 1, cma_ent_bus$trab_entorno)
  
  # Calcular a razão entre CMA para 60 minutos e quantidade de oportunidades no entorno
  #  cma$educ_perc <- (cma$CMAET60 / cma$educ_entorno)*100
  #  cma$saud_perc <- (cma$CMAST60 / cma$saud_entorno)*100
  #  cma$trab_perc <- (cma$CMATT60 / cma$trab_entorno)*100
  
  cma_ent_bus <- cma_ent_bus%>%
    dplyr::select(id_hex, trab_entorno, educ_entorno, saud_entorno)%>%
    rename(origin=id_hex)
  
  cma_bus <- st_join(cma_bus, cma_ent_bus, join=st_equals)
  cma_bus <- st_join(cma_bus, agregado_7, join=st_equals)
  
  cma_bus$CMATT_r <- ifelse(is.na((cma_bus$CMATT60/cma_bus$trab_entorno)),0,(cma_bus$CMATT60/cma_bus$trab_entorno))
  cma_bus$CMAST_r <- ifelse(is.na((cma_bus$CMAST60/cma_bus$saud_entorno)),0,(cma_bus$CMAST60/cma_bus$saud_entorno))
  cma_bus$CMAET_r <- ifelse(is.na((cma_bus$CMAET60/cma_bus$educ_entorno)),0,(cma_bus$CMAET60/cma_bus$educ_entorno))
  
  cma_bus$comp_trab <- cma_bus$CMATT_r*cma_bus$cat_pop_negra
  cma_bus$comp_saud <- cma_bus$CMAST_r*cma_bus$cat_pop_negra
  cma_bus$comp_educ <- cma_bus$CMAET_r*cma_bus$cat_pop_negra
  
  for (oport in oportunidades){
    if (oport == 'trabalho'){
      titulo_1 <- 'ao trabalho'
      col <- 'comp_trab'
    }else if (oport == 'saude'){
      titulo_1 <- 'à saúde'
      col <- 'comp_saud'
    }else{
      titulo_1 <- 'à educação'
      col <- 'comp_educ'
    }
    
    for (modo in modos){
      
      if (modo=='carro_compart'){
        res <- '07'
        df <- cma_car
        titulo_2 <- 'Automóvel por aplicativo'
      }else if (modo=='onibus'){
        res <- '07'
        df <- cma_bus
        titulo_2 <- 'Ônibus'
      }else if (modo=='walk'){
        res <- '08'
        df <- cma_walk
        titulo_2 <- 'A pé'
      }else{
        res <- '08'
        df <- cma_bike
        titulo_2 <- 'Bicicleta'
      }
      
      plot <- df%>%
        tm_shape()+ 
        tm_layout(main.title = sprintf("Acesso %s \n%s", titulo_1, titulo_2), 
                  legend.outside = FALSE, frame = FALSE, 
                  #panel.labels = panel_labels,
                  main.title.size = 1,
                  main.title.position = c("center","TOP"),
                  legend.title.size = 1,
                  frame.lwd = NA,
                  #panel.label.bg.color = NA,
                  #panel.label.size = 0.8,
                  inner.margins = c(0.02, 0.02, 0.02, 0.4))+
        tm_fill(col = col, 
                n=4,
                breaks = c(0,1,2,3,4),
                style = 'fixed', #"cat",
                palette = paleta, colorNA = paleta_NA, textNA = "Sem dados", 
                title = "CMA/CMA' * P",
                legend.format = list(text.separator="a", scientific=TRUE, format="f"))+ # scientific e interessante
        tm_scale_bar(breaks = c(0, 2, 4), text.size = .5, position=c("right", "bottom"))+
        #tm_facets(by = "CMA", nrow=1, free.coords = FALSE)+
        tm_legend(position = c('right','center'))
      
      file_name <- sprintf("%s/comp_aod_%s_%s_%s_%s_2019.%s", save_folder, muni, oport, modo, res, formato)
      tmap_save(tm = plot, filename = file_name, height = 1600, units = "px")
    }
  }
}

munis <- c("bho", "cam", "cgr", "cur", "for", "goi", "man", "nat", "rec",
           "rio", "sne", "sjc", "spo", "ula", "tsa", "vta", "jpa")

munis <- 'bho'

for (muni in munis){
  grafico_comp_aod(muni = muni, 
                      formato = "png")
}
