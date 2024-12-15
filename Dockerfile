FROM rocker/shiny-verse:4.4.1

# System dependencies
RUN apt-get update -y && apt-get install -y  make libx11-dev zlib1g-dev git libcurl4-openssl-dev libssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev libtiff-dev libxml2-dev pandoc xz-utils && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/focal/latest'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

# Install renv and restore lockfile
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.0.11")'
COPY renv.lock renv.lock
RUN R -e 'renv::restore(repos = "https://packagemanager.posit.co/cran/__linux__/focal/latest")'


EXPOSE 3838

# Copy app
COPY R /src/R
COPY inst /src/inst
COPY app.R /src/app.R
COPY DESCRIPTION /src/DESCRIPTION
COPY NAMESPACE /src/NAMESPACE

WORKDIR /src/
CMD Rscript app.R
