A non-destructive method to quantify starch content in red clover
================
Lea Frey, Philipp Baumann, Helge Aasen, Bruno Studer, Roland Kölliker

# The science behind

# Technical Description

This is the code repository that produces the outputs of the manuscript
with the above title.

The directory is self-contained and is designed to run reproducibly,
either on your local operating system or in a Docker container. The
description about how deploy this Docker image and run all analyses
within this project can be found below.

# Rerun all analyses

## Reproduce the analysis on your local machine (host operating system)

First, download this repository or clone it with git.

``` bash
git clone https://github.com/philipp-baumann/leaf-starch-spc
```

To restore all required packages at versions defined in the file
[`renv.lock`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/renv.lock)
based on the [renv](https://github.com/rstudio/renv) R package, execute
the following in the project directory.

``` r
install.packages("remotes")
remotes::install_github("rstudio/renv@0.12.0")
# Automatically installs packages from CRAN and github 
# as specified in `renv.lock`
renv::restore()
```

You can manually run the scripts in sequential order, but we recommend
to deploy the entire workflow in automated manner using
[drake](https://books.ropensci.org/drake/) R package. This gives you
tangible evidence of reproducibility.

``` r
# Make drake plan (targets and expressions in scripts: see ./code/:
# Starts a separate R process from R for safe interactivity
source("_make.R")
```

## Reproduce the analysis in Docker container (remote server or local machine)

Docker provides an open-source solution to create an isolated software
environment that captures the entire computational environment. This
makes the data analysis scalable and reproducible, independent of the
host operating system. To get started with Docker, there is an [rOpenSci
R Docker tutorial](https://ropenscilabs.github.io/r-docker-tutorial/)
that explains the motivation and basics of using Docker for reproducible
research. However, you can also just follow the steps outlined below.

A `Dockerfile` is a text file that contains a recipe to build an image
with a layered approach. A docker image is a running instance of a
container. [This
`Dockerfile`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/Dockerfile)
is based on the
[`rocker/rstudio:3.6.0`](https://hub.docker.com/r/rocker/rstudio/)
image, which bases itself on
[`rocker/r-ver`](https://hub.docker.com/r/rocker/r-ver/) with debian 9
(stretch) including version stable base R (v3.6.0) and the source build
tools. The RStudio image provides RStudio server within a Docker image,
that you can access via your browser. Basic instructions are given here,
but for getting started you can additionally consider [this
resource](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image).
The image is version-stable and uses the MRAN snapshot of the last day
that the R version 3.6.0 was the most recent release.

The workflow deployed here is fueled by the
[`{renv}`](https://rstudio.github.io/renv/articles/renv.html) package,
which manages the installation of specific package versions and sources,
and the [`{drake}`](https://docs.ropensci.org/drake/) package to keep
track of R code and data that produce the results.

The [drake manual lists two
examples](https://ropenscilabs.github.io/drake-manual/index.html#with-docker)
that combine `{drake}` workflows with Docker. This can give you some
more detail of how everything works under the hood.

### Docker recipe

The follwing steps generates the computational environments, runs all
computations, and let you grab the results of the entire analysis done
in R.

1.  Build the docker container with instructions from the
    [`Dockerfile`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/Dockerfile).

<!-- end list -->

``` bash
docker build -t leaf-starch-spc .
```

2.  Check wether the image is built.

<!-- end list -->

``` bash
docker images
```

3.  Launch the container and share the local volume (host) with the
    container

<!-- end list -->

``` bash
# https://www.rocker-project.org/use/managing_users/
sudo docker run -d -p 8787:8787 -e PASSWORD=spcclover -v "${pwd}:/home/rstudio" -e USERID=$UID leaf-starch-spc
```

4.  2.  Port-forwarding: The RStudio server service running within the
        docker image on the remote VM can be tunneled into your local
        browser session using ssh port forwarding. This is extremely
        convenient because you one can do interactive data analysis with
        “local feel”.

<!-- end list -->

``` bash
ssh -f -N -L 8787:localhost:8787 <your_user>@<host_ip_addres>
```

# File overview

The files in this project are organized as follows (only 2 folder levels
are shown):

    ## .
    ## ├── Dockerfile
    ## ├── R
    ## │   ├── helpers.R
    ## │   ├── modeling.R
    ## │   ├── select-spc-xvalues.R
    ## │   └── vip-wrappers.R
    ## ├── README.Rmd
    ## ├── README.md
    ## ├── _crop-images.R
    ## ├── _drake.R
    ## ├── _make.R
    ## ├── code
    ## │   ├── 10_read-clean-process-training.R
    ## │   ├── 20_build-spc-model-training.R
    ## │   ├── 21_interpret-training-vip.R
    ## │   ├── 22_remodel-vip-filtering.R
    ## │   ├── 23_remodel-cor-filtering.R
    ## │   ├── 24_remodel-starch-bands.R
    ## │   ├── 30_read-clean-process-test.R
    ## │   ├── 31_visualize-refdata.R
    ## │   ├── 40_predict-evaluate-train-test.R
    ## │   ├── 50_remodel-test.R
    ## │   ├── 51_interpret-test-vip.R
    ## │   ├── 52_remodel-test-vip-training.R
    ## │   └── 60_evaluate-test.R
    ## ├── code-legacy
    ## │   └── 25_remodel-mutual-information.R
    ## ├── cp-renv-lock.sh
    ## ├── data
    ## │   ├── test
    ## │   │   ├── metadata
    ## │   │   ├── reference
    ## │   │   └── spectra
    ## │   └── training
    ## │       ├── metadata
    ## │       ├── reference
    ## │       └── spectra
    ## ├── leaf-starch-spc.Rproj
    ## ├── out
    ## │   ├── data
    ## │   │   ├── wavelength-vip-training-highlight.csv
    ## │   │   ├── wavelength-vip-training-highlight.xlsx
    ## │   │   └── wavelength-vip-training.csv
    ## │   └── figs
    ## │       ├── boxplot-starch-genotype-ed.pdf
    ## │       ├── boxplot-starch-leaf-age.pdf
    ## │       ├── boxplot-starch-sets.pdf
    ## │       ├── eval-training-mlr-cv.pdf
    ## │       ├── eval-training-raw-cv.pdf
    ## │       ├── eval-training-self-cv.pdf
    ## │       ├── eval-vip-bigger1.pdf
    ## │       ├── eval-vip-filtering.pdf
    ## │       ├── eval-vip-top50.pdf
    ## │       ├── eval.pdf
    ## │       ├── predobs-test-corfilt.pdf
    ## │       ├── predobs-test-genotype-harvest-time.pdf
    ## │       ├── predobs-test-genotype.pdf
    ## │       ├── predobs-train-genotype.pdf
    ## │       ├── predobs-training-harvest-time.pdf
    ## │       ├── spc-starch-pls-test-vip.pdf
    ## │       ├── spc-starch-pls-vip.pdf
    ## │       ├── spc-train-check.pdf
    ## │       ├── spc-train-raw.pdf
    ## │       ├── spc-train.pdf
    ## │       ├── test-eval-allvars-vip-bigger1.pdf
    ## │       ├── test-eval-pls-allvars-vip.pdf
    ## │       ├── test-eval-vip-bigger1.pdf
    ## │       └── test-eval.pdf
    ## ├── packages.R
    ## ├── pub
    ## │   ├── figs
    ## │   │   ├── Fig2.pdf
    ## │   │   ├── Fig4.pdf
    ## │   │   ├── Fig5.pdf
    ## │   │   ├── Fig6.pdf
    ## │   │   ├── Fig6.png
    ## │   │   ├── Fig7.pdf
    ## │   │   ├── Fig8.pdf
    ## │   │   ├── S2.pdf
    ## │   │   ├── S3.pdf
    ## │   │   ├── S4.pdf
    ## │   │   ├── S5.pdf
    ## │   │   ├── eval.pdf
    ## │   │   ├── figs-cropped
    ## │   │   └── figs-original
    ## │   ├── figs.zip
    ## ├── renv
    ## │   ├── activate.R
    ## │   ├── library
    ## │   │   └── R-3.6
    ## │   └── settings.dcf
    ## ├── renv.lock
    ## ├── ssd-to-vm.sh
    ## └── vm-to-ssd.sh
