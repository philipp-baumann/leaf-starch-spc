################################################################################
## Project:     A non-destructive method to quantify starch content
##              in red clover (T. pratense)
## Description: Model starch in the test set using PLS regression
################################################################################

## Train a PLS regression model on the test set ================================

pls_starch_test <- fit_pls(
  spec_chem = spc_test_predict,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  resampling_seed = 142L,
  pls_ncomp_max = 10
)
