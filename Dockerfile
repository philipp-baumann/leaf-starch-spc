# Pull docker image (includes RStudio server)
FROM rocker/rstudio:3.6.0

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libz-dev \
  libxml2-dev

ENV RENV_VERSION 0.12.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN mkdir /home/rstudio/leaf-starch-spc
WORKDIR /home/rstudio/leaf-starch-spc
COPY renv.lock ./
RUN R -e 'renv::restore()'

# Copy working directory, use .dockerignore for excluding directories
COPY . /home/rstudio/swr-spc/