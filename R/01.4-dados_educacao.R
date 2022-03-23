#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Tratar e fazer geocode dos dados do censo escolar do INEP

# carregar bibliotecas
source('fun/setup.R')

# carregar funcoes
source('fun/educacao/educacao.R')

# ATENÇÃO: Os arquivos base para rodar este script devem ser baixados de forma
# manual e inseridos na pasta de trabalho 05_censo_escolar/[ano]- ver 
# 'fun/educacao/educacao.R' para mais detalhes

# Atualizar ano na linha abaixo para criar as pastas para cada ano
# lapply(X = 2017:2019, FUN = criar_pastas_censo_educacao)
ano = 2019
lapply(X = ano, FUN = criar_pastas_censo_educacao)

# 1) Aplicar funcao filtro ----------------
# O resultado é o arquivo educacao_%s_filter.rds, que contém as matrículas de
# ensino infantil, fundamental e médio para cada escola
# lapply(X=2017:2019, FUN = educacao_filter)
lapply(X = ano, FUN = educacao_filter)

# 2) Juntar com geocode das escolas vindo do INEP
# lapply(X=2017:2019, FUN = educacao_juntar_geocode_inep)
lapply(X = ano, FUN = educacao_juntar_geocode_inep)

# 3) Fazer geocode das escolas que não possuem latlong no arquivo que vem do INEP
educacao_geocode_all(ano = ano)
