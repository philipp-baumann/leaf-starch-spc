## Project:     A non-destructive method to quantify starch content
##              in red clover (T. pratense)
## Description: Remodel the test set with predictors that had VIP > 1 during
##              training
################################################################################


## Remodel with VIP filtered variables selected in training ====================

pls_starch_test_vip_bigger1 <- fit_pls(
  spec_chem = spc_test_predict_vip_bigger1,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  resampling_seed = 142L,
  pls_ncomp_max = 10
)
