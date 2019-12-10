################################################################################
## Project:
## Description:
################################################################################


## Remodel with VIP filtered variables selected in training ====================

seed_pls_starch_test_vip_bigger1 <- set.seed(131L)

pls_starch_test_vip_bigger1 <- fit_pls(
  spec_chem = spc_test_predict_vip_bigger1,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",  pls_ncomp_max = 10
)
