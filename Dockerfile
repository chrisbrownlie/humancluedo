FROM rocker/shiny-verse:4.4.1

# System deps
RUN apt-get update && apt-get install -y \
  librsvg2-dev

# Install and restore renv
RUN install2.r renv
RUN mkdir -p /renv/library
ENV RENV_PATHS_LIBRARY=/renv/library
COPY renv.lock /src/renv.lock
RUN R -e "renv::restore(lockfile='src/renv.lock', repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

EXPOSE 3838

# Copy app
COPY R /src/R
COPY inst src/inst
COPY app.R /src/app.R
COPY DESCRIPTION /src/DESCRIPTION
COPY NAMESPACE /src/NAMESPACE

WORKDIR /src/
CMD Rscript app.R
