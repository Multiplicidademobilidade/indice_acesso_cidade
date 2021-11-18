#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### 0.1.3 Geocode dados do censo escolar

# carregar bibliotecas
source('fun/setup.R')

# carregar funcoes
source('fun/educacao/educacao.R')

# ATENÇÃO: Os arquivos base para rodar este script devem ser baixados de forma
# manual - ver 'fun/educacao/educacao.R' para mais detalhes

# Atualizar anos na linha abaixo para criar as pastas para cada ano
# lapply(X = 2017:2019, FUN = criar_pastas_censo_educacao)
lapply(X = 2019, FUN = criar_pastas_censo_educacao)

# 1) Aplicar funcao filtro ----------------
# O resultado é o arquivo educacao_%s_filter.rds, que contém as matrículas de
# ensino infantil, fundamental e médio para cada escola
# lapply(X=2017:2019, FUN = educacao_filter)
lapply(X=2019, FUN = educacao_filter)

# 2) Juntar com geocode das escolas vindo do INEP
# lapply(X=2017:2019, FUN = educacao_juntar_geocode_inep)
lapply(X=2019, FUN = educacao_juntar_geocode_inep)

# 3) Fazer geocode das escolas que não possuem latlong no arquivo que vem do INEP
educacao_geocode_all(ano = 2019)

# Observação: os passos 2 e 3 abaixo (originais do IPEA) foram substituídos
# pelos 2 e 3 acima. Por isso, não assumiremos mais para frente que o arquivo
# educacao_%s_filter_geocoded_gmaps_gquality.rds, resultante da função
# geocode_filter() abaixo será o mesmo educacao_%s_filter_geocoded_gmaps.rds
# resultante da função educacao_geocode_all() acima


# # 2) Aplicar funcao geocode ---------------
# educacao_geocode_ipea(run_gmaps = FALSE)

# # 3) Selecionar somente as obsservacoes com boa qualidade de geocode ----------
# source("fun/filter_geocode.R")
# lapply(X=2017:2019, geocode_filter, "educacao")
