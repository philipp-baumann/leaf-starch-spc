################################################################################
## Project:
## Description:
################################################################################

## Return wavelengths with VIP > 1 =============================================

wavelengths_vip_bigger1 <- df_vip_pls %>%
  filter(vip > 1) %>%
  pull(wavenumber)

spc_train_model_vip_bigger1 <- select_spc_xvalues(
  spc_tbl = spc_train_model, xvalues = wavelengths_vip_bigger1)


## Return top 50 wavelengths with highest VIP ==================================

wavelengths_vip_top50 <- df_vip_pls %>%
  arrange(desc(vip)) %>%
  .[1:50, ] %>%
  pull(wavenumber)

spc_train_model_vip_top50 <- select_spc_xvalues(
  spc_tbl = spc_train_model, xvalues = wavelengths_vip_top50)


## Remodel with VIP filtering ==================================================

seed_pls_starch_vip_bigger1 <- set.seed(131L)

pls_starch_vip_bigger1 <- fit_pls(
  spec_chem = spc_train_model_vip_bigger1,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",  pls_ncomp_max = 10
)

pls_starch_vip_bigger1_pdf <- ggsave(filename = "eval-vip-bigger1.pdf",
  plot = pls_starch_vip_bigger1$p_model, path = here("out", "figs"))

seed_pls_starch_vip_top50 <- set.seed(131L)

pls_starch_vip_top50 <- fit_pls(
  spec_chem = spc_train_model_vip_top50,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",  pls_ncomp_max = 10
)

pls_starch_vip_top50_pdf <- ggsave(filename = "eval-vip-top50.pdf",
  plot = pls_starch_vip_top50$p_model, path = here("out", "figs"))
