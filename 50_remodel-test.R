################################################################################
## Project:
## Description:
################################################################################

## Train a PLS regression model on the test set ================================

pls_starch_test <- fit_pls(
  spec_chem = spc_test_predict,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv", pls_ncomp_max = 10
)

pls_starch_test_rds <- readr::write_rds(x = pls_starch_test,
  path = here("models", "pls_starch_test.Rds"))
