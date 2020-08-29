################################################################################
## Project:     A non-destructive method to quantify starch content
##              in red clover (T. pratense)
## Description: Train PLS regression models for starch based on Savitzky-Golay
##              pre-processed spectra and raw spectra
################################################################################

## Train a PLS regression model ================================================

pls_starch <- fit_pls(
  spec_chem = spc_train_model,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",
  resampling_seed = 142L,
  pls_ncomp_max = 10
)

# Fit model on raw spectral data
pls_starch_raw <- spc_train_model %>%
  mutate(spc_pre = spc_rs) %>%
  fit_pls(
    spec_chem = .,
    response = starch,
    evaluation_method = "resampling",
    tuning_method = "resampling",
    resampling_method = "rep_kfold_cv", 
    resampling_seed = 142L,
    pls_ncomp_max = 10
  )
  

# Colour model cross-validation plot by genotype -------------------------------

train_predobs <- pls_starch$predobs %>%
  filter(dataType == "Cross-validation") %>%
  as_tibble() %>%
  inner_join(x = .,
    y = spc_train_model %>% 
      select(sample_id, genotype, leaf_age, harvest_time, starch)) %>%
  mutate(
    genotype_nodot = stringr::str_split(string = genotype,
      pattern = "[.]")
  ) %>%
  mutate(
    genotype_nodot = unlist(purrr::map(genotype_nodot, 1))
  )
