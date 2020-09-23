A non-destructive method to quantify starch content in red clover
================
Lea Frey, Philipp Baumann, Helge Aasen, Bruno Studer, Roland Kölliker

# The science behind

# Technical description

This is the code repository that produces the outputs of the manuscript
with the above title.

The directory is self-contained and is designed to run reproducibly,
either on your host operating system (local or remote) or in a Docker
container (local or remote; relying on kernel of the host). The
practical instructions to deploy this Docker image and run all analyses
within this project can be found below. Attribution is given to Thomas
Knecht aka Mr. Propper, who gave me the courage and critical ideas about
the orchestration.

# Rerun all analyses (1. or 2.)

## 1\. Reproduce the analysis within the host operating system

First, download this repository or clone it with git.
([Git](https://git-scm.com/) is a popular free and open source version
control software. Simply download to feel it.)

``` bash
git clone https://github.com/philipp-baumann/leaf-starch-spc
```

Windows users probably want to download the R 3.6.3 or older version of
[rtools](https://cran.r-project.org/bin/windows/Rtools/history.html) to
build packages from source. MacOS users will require
[XCode](https://developer.apple.com/xcode/) for the compiler toolchain.
To restore all required packages at versions defined in the file
[`renv.lock`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/renv.lock)
based on the [renv](https://github.com/rstudio/renv) R package, execute
the following in the project directory. You might first want to set up
the project directory in RStudio (see
[here](https://r4ds.had.co.nz/workflow-projects.html)) unless you work
in a terminal.

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

## 2\. Reproduce the analysis within Docker container (remote server or local machine)

Docker provides an open-source solution to create an isolated software
environment that captures the entire computational environment. This
makes the data analysis scalable and reproducible, independent of the
host operating system. To get started with Docker, there is an [rOpenSci
R Docker tutorial](https://ropenscilabs.github.io/r-docker-tutorial/)
that explains the motivation and basics of using Docker for reproducible
research. However, you can also just follow the steps outlined below.

A `Dockerfile` is a text file that contains a recipe to build an image
with a layered approach. A docker container is a running instance of an
image. [This
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
in section 1.5 that combine `{drake}` workflows with Docker. This can
give you some more detail of how everything works under the hood.

### Docker recipe

The following docker bash commands generates the computational
environment, runs all computations, and let you grab the results of the
entire analysis done in R.

1.  Build the docker image with instructions from the
    [`Dockerfile`](https://github.com/philipp-baumann/leaf-starch-spc/blob/master/Dockerfile).

<!-- end list -->

``` bash
# Cache configuration: https://github.com/rstudio/renv/issues/362
# https://github.com/rstudio/renv/issues/400
docker build -t leaf-starch-spc .
```

2.  Check whether the image is built.

<!-- end list -->

``` bash
docker images
```

3.  Launch the container from the built image. Share two local paths as
    volumes (host) with the container. The analysis worflow orchestrated
    by {drake} will write output files (Figures) explained in the
    accompanying manuscript.

<!-- end list -->

``` bash
# https://www.rocker-project.org/use/managing_users/
# https://github.com/rocker-org/rocker/wiki/Sharing-files-with-host-machine
docker run --rm -d -p 8787:8787 \
    -e PASSWORD=spcclover \
    -v "$(pwd)/out:/home/rstudio/out" \
    -v "$(pwd)/pub:/home/rstudio/pub" \
    -e USERID=$UID -e GROUPID=$GID leaf-starch-spc
```

4.  Open RStudio server and kick-off the workflow. There are two
    suggestions deployment, one via docker running on your computer (4.
    i.), and the other via docker on a virtual machine tunnelled via ssh
    (4. ii.)

<!-- end list -->

``` bash
cat _make.R
```

    ## #!/usr/bin/env Rscript
    ## 
    ## renv::restore()
    ## 
    ## library("drake")
    ## r_make()
    ## 
    ## cat("build_time_seconds ", round(sum(build_times()$elapsed), 2),
    ##   file = here::here("_build-time.txt"))

`drake::r_make()` invokes `_drake.R`, calling `drake::make()` in a
separate processs in the operating system to sanitize the make process.

``` r
# Run in the R console in RStudio Server
source("_make.R")
```

4.i. **Local port-forwarding via ssh**: The RStudio server service
running within the docker image on the remote VM can be tunneled into
your local browser session using ssh port forwarding. This is extremely
convenient because you one can do interactive data analysis with “local
feel”.

``` bash
ssh -f -N -L 8787:localhost:8787 <your_user>@<host_ip_address>
```

Simply open RStudio Server in your browser on <localhost:8787>. Then,
login with user `rstudio` and password `spcclover`

# File overview

The files in this project are organized as follows (only 2 folder levels
are shown):

    ## .
    ## ├── Dockerfile
    ## ├── Dockerfile_legacy
    ## ├── Makefile
    ## ├── R
    ## │   ├── helpers.R
    ## │   ├── modeling.R
    ## │   ├── select-spc-xvalues.R
    ## │   └── vip-wrappers.R
    ## ├── README.Rmd
    ## ├── README.md
    ## ├── _convert-images.R
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
    ## ├── cp-images.sh
    ## ├── cp-renv-lock.sh
    ## ├── data
    ## │   ├── test
    ## │   └── training
    ## ├── docker-base
    ## │   ├── Dockerfile_base
    ## │   ├── disable_auth_rserver.conf
    ## │   ├── pam-helper.sh
    ## │   └── userconf.sh
    ## ├── docker-compose.yml
    ## ├── leaf-starch-spc.Rproj
    ## ├── out
    ## │   ├── data
    ## │   └── figs
    ## ├── packages.R
    ## ├── pub
    ## │   ├── approval
    ## │   ├── figs
    ## │   ├── figs.zip
    ## │   ├── review-submission-2
    ## │   ├── submission-01
    ## │   ├── submission-02
    ## │   └── writing
    ## ├── renv
    ## │   ├── activate.R
    ## │   ├── library
    ## │   └── settings.dcf
    ## ├── renv.lock
    ## ├── ssd-to-vm.sh
    ## └── vm-to-ssd.sh
