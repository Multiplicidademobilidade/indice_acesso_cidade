
# Hierarquia organizacional dos arquivos

O primeiro arquivo, com numeração **00-00**, lista a estrutura de pastas que serão utilizadas e/ou criadas ao longo dos scripts.

Os arquivos de R estão numerados com o prefixo de acordo com a etapa à qual pertencem. A documentação sobre como conseguir os arquivos originais públicos (Censo, educação, saúde e centros de assistência social) está nos comentários dos próprios scripts. A exceção é a base de empregos da RAIS, cujo georreferenciamento realizado pelos dados de CEP foi feito de forma externa - é preciso, portanto, ter uma base similar para rodá-los.

As etapas são as seguintes:

  - `01`: **tratamento inicial** feito às bases de dados brutas de informações socieconômicas, uso do solo e transporte;
  - `02`: criação das **unidades de agregação (hexágonos)** e agregação das variáveis de transporte e uso do solo. Esta etapa conta com uma cópia do script 02-02 no formato de Jupyter Notebook pois o RStudio bloqueia por padrão a execução de tarefas de multiprocessamento. Este mesmo script rodará *muito* mais rápido no Jupyter;
  - `03`: criação das redes de transporte, configuração do **r5r** para roteamento e **geração das matrizes de distância**. A matriz de distância para o modo ônibus é gerada pelo Google Maps (consideradas as limitações dos termos de uso da plataforma);
  - `04`: cálculo dos **acessos a oportunidades**;
  - `05`: **criação dos índices** de Acesso às Oportunidades em prol da Redução de Desigualdades (IAOD), de Integração e novas viagens do automóvel por aplicativo (INIA) e de Acesso à Cidade (IAC);
  - `06`: criação dos **gráficos** relacionados aos índices.
