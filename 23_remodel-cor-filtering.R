################################################################################
## Project:
## Description:
################################################################################

## Remodel with spectral variables filtered using correlation ==================

spc_pre_train <- rbindlist(spc_train_model$spc_pre)
starch_train_vec <- spc_train_model$starch

cor_spc_starch <- lineup::corbetw2mat(
  x = spc_pre_train, 
  y = matrix(
    rep(starch_train_vec, ncol(spc_pre_train)), ncol = ncol(spc_pre_train))
)

# top 50 correlations
cor_spc_top50 <- cor_spc_starch[order(abs(cor_spc_starch),
  decreasing = TRUE)[1:50]]

# Select the subset 50 spectral variables with maximum correlation
wl_cor_top50 <- as.numeric(names(cor_spc_top50))

spc_train_model_corfilt <- select_spc_xvalues(
  spc_tbl = spc_train_model, xvalues = wl_cor_top50, column_in = "spc_pre",
  xvalues_in = "xvalues_pre")

pls_starch_corfilt <- fit_pls(
  spec_chem = spc_train_model_corfilt,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",  pls_ncomp_max = 10
)
