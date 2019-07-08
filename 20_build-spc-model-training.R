################################################################################
## Project:
## Description:
################################################################################

## Train a PLS regression model ================================================

pls_starch <- fit_pls(
  spec_chem = spc_train_model,
  response = starch,
  evaluation_method = "resampling",
  tuning_method = "resampling",
  resampling_method = "rep_kfold_cv",  pls_ncomp_max = 10
)

pls_starch_rds <- readr::write_rds(x = pls_starch,
  path = here("models", "pls_starch.Rds"))

# Colour model cross-validation based model evaluation by leaf age -------------

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