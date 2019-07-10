A non-destructive method to quantify starch content in red clover
================
Lea Frey, Philipp Baumann, Helge Aasen, Bruno Studer, Roland Kölliker

# Project description

# File overview

The files in this project are organized as follows (only 2 folder levels
are shown):

    ## .
    ## ├── 00_setup-make.R
    ## ├── 10_read-clean-process-training.R
    ## ├── 20_build-spc-model-training.R
    ## ├── 21_interpret-model-vip.R
    ## ├── 30_read-clean-process-test.R
    ## ├── 40_predict-test-spc.R
    ## ├── R
    ## │   ├── helpers.R
    ## │   └── vip-wrappers.R
    ## ├── README.Rmd
    ## ├── README.md
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
    ## │   └── pls_starch.Rds
    ## ├── out
    ## │   └── figs
    ## │       ├── eval.pdf
    ## │       ├── predobs-test-genotype.pdf
    ## │       ├── spc-starch-pls-vip.pdf
    ## │       ├── spc-train.pdf
    ## │       └── test-eval.pdf
    ## └── transfer-vm.sh
