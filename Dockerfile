FROM rstudio/plumber:v1.2.0
LABEL MAINTAINER="GT-CEAD"

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  build-essential \
  libcurl4-gnutls-dev \
  libssl-dev \
  libmagick++-dev \
  cargo \
  libudunits2-dev \
  libabsl-dev \
  cmake \
  pkg-config \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libsqlite3-dev \
  && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/noble/latest')); \
  install.packages('remotes'); \
  install.packages('sf'); \
  install.packages('transformr'); \
  remotes::install_version('dplyr', version = '1.1.4', repos = 'https://p3m.dev/cran/__linux__/noble/latest', upgrade = 'never'); \
  remotes::install_version('ggplot2', version = '3.5.1', repos = 'https://p3m.dev/cran/__linux__/noble/latest', upgrade = 'never'); \
  remotes::install_version('ggrepel', version = '0.9.5', repos = 'https://p3m.dev/cran/__linux__/noble/latest', upgrade = 'never'); \
  remotes::install_version('gganimate', version = '1.0.9', repos = 'https://p3m.dev/cran/__linux__/noble/latest', upgrade = 'never'); \
  remotes::install_version('gifski', version = '1.12.0-1', repos = 'https://p3m.dev/cran/__linux__/noble/latest', upgrade = 'never'); \
  install.packages(c( \
    'ckanr', \
    'readxl', \
    'tidyr', \
    'readr', \
    'stringr', \
    'RColorBrewer', \
    'future', \
    'promises', \
    'base64enc', \
    'magick', \
    'lubridate', \
    'zoo' \
  )); \
  print(packageVersion('gifski')); \
  stopifnot(as.character(packageVersion('dplyr')) == '1.1.4'); \
  stopifnot(as.character(packageVersion('ggplot2')) == '3.5.1'); \
  stopifnot(as.character(packageVersion('gganimate')) == '1.0.9'); \
  stopifnot(as.character(packageVersion('gifski')) == '1.12.0.1')" \

COPY policy.xml /etc/ImageMagick-6/
COPY --chmod=777 codigo/ /apps

EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/apps/API_Grandes_Numeros_STN.R'); pr$run(host='0.0.0.0', port=8000)"]