A non-destructive method to quantify starch content in red clover
================
Lea Frey, Philipp Baumann, Helge Aasen, Bruno Studer, Roland Kölliker

# Description

This is the code repository that produces the outputs of the manuscript
with the above title.

The directory is self-contained and is designed to run reproducibly,
either on your local operating system or in a Docker container. The
description about how deploy this Docker image and run all analyses
within this project can be found below.

# Reproduce the analysis locally

## Restore packages

To restore all required packages at versions defined in the file
[`renv.lock`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/renv.lock)
based on the [renv](https://github.com/rstudio/renv) R package, execute
the following in the project directory:

``` r
install.packages("remotes")
remotes::install_github("rstudio/renv@0.7.0-111")
# Automatically installs packages from CRAN and github 
# as specified in `renv.lock`
renv::restore()
```

## Rerun all analyses

You can either manually run the scripts in sequential order, or deploy
the entire workflow reproducibly and automatedly using the
[drake](https://books.ropensci.org/drake/) R package:

``` r
source("00_setup-make.R")
```

## Reproduce the analysis in Docker container

A Docker image can be built as described in the file
[`Dockerfile`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/Dockerfile).
It bases upon the
[`rocker/rstudio:3.6.1`](https://hub.docker.com/r/rocker/rstudio) image
(R 3.6.1 plus RStudio), on top of which it installs system libraries for
the [tidyverse](https://www.tidyverse.org/) and the
[renv](https://github.com/rstudio/renv) package. Further, it
automatically restores all deployed package versions, and then runs the
drake plan specified in
[`00_setup-make.R`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/00_setup-make.R).
Henceforth, the entire analysis is reproduced in a running instance of
the image (also called container).

Kindly note that the Dockerfile is not yet complete upon manuscript
submission. Further instructions how to build the image, run a container
instance, and retrieve the results will follow soon.

# File overview

The files in this project are organized as follows (only 2 folder levels
are shown):

    ## .
    ## ├── %
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
    ## ├── code
    ## │   ├── 10_read-clean-process-training.R
    ## │   ├── 20_build-spc-model-training.R
    ## │   ├── 21_interpret-training-vip.R
    ## │   ├── 22_remodel-vip-filtering.R
    ## │   ├── 23_remodel-cor-filtering.R
    ## │   ├── 24_remodel-starch-bands.R
    ## │   ├── 25_remodel-mutual-information.R
    ## │   ├── 30_read-clean-process-test.R
    ## │   ├── 31_visualize-refdata.R
    ## │   ├── 40_predict-evaluate-train-test.R
    ## │   ├── 50_remodel-test.R
    ## │   ├── 51_interpret-test-vip.R
    ## │   ├── 52_remodel-test-vip-training.R
    ## │   └── 60_evaluate-test.R
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
    ## ├── models
    ## │   ├── pls_starch.Rds
    ## │   └── pls_starch_test.Rds
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
