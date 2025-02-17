FROM r-base:4.3.1

# Instala dependências do sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libv8-dev

# Instala os pacotes R necessários
RUN R -e "install.packages(c('shiny', \
                           'dplyr', \
                           'DT', \
                           'patchwork', \
                           'tidyr', \
                           'stringr', \
                           'leaflet', \
                           'sf', \
                           'readxl', \
                           'geojsonio', \
                           'shinycssloaders', \
                           'plotly', \
                           'forcats'))"

# Cria diretório para o app
RUN mkdir -p /app
WORKDIR /app

# Copia os arquivos do app
COPY . /app/

# Expõe a porta que o Shiny usa
EXPOSE 3838

# Comando para executar o app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]

