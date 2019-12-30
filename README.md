A non-destructive method to quantify starch content in red clover
================
Lea Frey, Philipp Baumann, Helge Aasen, Bruno Studer, Roland Kölliker

# Description

This is the code repository for the manuscript for the manuscript with
the above title.

The directory is self-contained and is designed to run reproducibly in a
docker container. Description about how deploy Docker containers can be
found below.

# Comments on reproduciblity

# File overview

The files in this project are organized as follows (only 2 folder levels
are shown):

    ## .
    ## ├── 00_setup-make.R
    ## ├── 10_read-clean-process-training.R
    ## ├── 20_build-spc-model-training.R
    ## ├── 21_interpret-training-vip.R
    ## ├── 22_remodel-vip-filtering.R
    ## ├── 23_remodel-cor-filtering.R
    ## ├── 24_remodel-starch-bands.R
    ## ├── 25_remodel-mutual-information.R
    ## ├── 30_read-clean-process-test.R
    ## ├── 40_predict-evaluate-train-test.R
    ## ├── 50_remodel-test.R
    ## ├── 51_interpret-test-vip.R
    ## ├── 52_remodel-test-vip-training.R
    ## ├── 60_evaluate-remodel-test.R
    ## ├── R
    ## │   ├── helpers.R
    ## │   ├── modeling.R
    ## │   ├── select-spc-xvalues.R
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
    ## │   ├── pls_starch.Rds
    ## │   └── pls_starch_test.Rds
    ## ├── out
    ## │   ├── data
    ## │   │   ├── wavelength-vip-training-highlight.csv
    ## │   │   ├── wavelength-vip-training-highlight.xlsx
    ## │   │   └── wavelength-vip-training.csv
    ## │   └── figs
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
    ## ├── pub
    ## │   └── writing
    ## │       ├── PSC_symposium_2019_lea-comm-pb.docx
    ## │       ├── PSC_symposium_2019_lea.docx
    ## │       ├── Supplement3.docx
    ## │       ├── Supplement3_PB.docx
    ## │       ├── paper-plant-methods-clover-starch-spc.pdf
    ## │       ├── paper_plant_methods_FieldSpec4_LeaFreyKoro3.docx
    ## │       ├── paper_plant_methods_FieldSpec4_LeaFreyKoro3Baumann.docx
    ## │       ├── paper_plant_methods_LeaR.docx
    ## │       ├── paper_plant_methods_LeaR_pb.docx
    ## │       ├── paper_plant_methods_LeaR_pb.pdf
    ## │       ├── paper_plant_methods_Lea_Roli2.docx
    ## │       ├── paper_plant_methods_Lea_Roli2_PB.docx
    ## │       └── paper_plant_methods_Lea_Roli2_PB_tab-filt.docx
    ## └── ssd-to-vm.sh
