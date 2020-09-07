# Pull docker image (includes RStudio server)
FROM rocker/rstudio:3.6.0

ENV ARG HTTP_PROXY
ENV ARG HTTPS_PROXY

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libz-dev \
  libxml2-dev

ENV RENV_VERSION 0.12.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /home/rstudio/
COPY renv.lock ./
COPY .Renviron ./

RUN echo '.libPaths("/home/rstudio/renv/library/R-3.6/x86_64-pc-linux-gnu")' >> /usr/local/lib/R/etc/Rprofile.site

RUN R -e 'renv::consent(provided = TRUE); renv::restore(clean = TRUE, confirm = FALSE)'

# Copy working directory, .dockerignore which files/dirs to exclude
COPY . /home/rstudio/

# Create output folders
RUN mkdir -p /home/rstudio/out/data \
  && mkdir -p /home/rstudio/out/figs \
  && mkdir -p /home/rstudio/pub/figs

RUN Rscript _make.R
