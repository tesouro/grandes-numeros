FROM rstudio/plumber:v1.2.0
LABEL MAINTAINER="GT-CEAD"

RUN apt-get update -qq  && apt-get install -y  \
  git-core \
  libssl-dev \
  libxml2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  build-essential \
  ffmpeg \
  pngquant \
  cargo \
  libmagick++-dev \
  && rm -rf /var/lib/apt/lists/* 
  

RUN install2.r plumber && \
    Rscript -e "install.packages(c('rvest', 'xml2', 'ckanr', 'readxl', 'tidyverse', 'RColorBrewer','devtools', 'ggrepel', 'future', 'promises', 'magick'))" && \
    Rscript -e "library(devtools);install_github('tchiluanda/rtn');install_github('r-rust/gifski')" && \
    Rscript -e "require(devtools); install_version('gganimate', version = '1.0.7', repos = 'http://cran.us.r-project.org')"

# Copia o policy.xml para o /etc/Imagemagick-6/
COPY policy.xml /etc/ImageMagick-6/

COPY --chmod=777 codigo/ /apps

EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/apps/API_Grandes_Numeros_STN.R'); pr$run(host='0.0.0.0', port=8000)"]