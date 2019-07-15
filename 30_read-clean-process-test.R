################################################################################
## Project:
## Description:
################################################################################

## Read test spectra ===========================================================

files_test <- dir(here("data", "test", "spectra"), full.names = TRUE)

spc_test_allvars <- read_asd_bin(fnames = files_test)


## Select only relevant spectral columns =======================================

spc_test <- spc_test_allvars %>%
  select(- c(spc_radiance, spc_reference))


## Correct join wavelengths offset for different sensors =======================

spc_test_offcorr <- simplerspec:::correct_join_offset(spc_tbl = spc_test)


## Add the IDs for the Genotype or the type of reference (black or white) ======

id_metadata_test_raw <- read_csv(file = "data/test/metadata/id_metadata.csv")

# Remove ".ref" at the end of the files ----------------------------------------

id_metadata_test <- simplerspec:::remove_id_extension(
  data = id_metadata_test_raw, id_col = "sample_id", id_new_nm = "sample_id")


## Read laboratory reference data: starch measurements =========================

# Apply column specifications; otherwise `starch` is not parsed as double,
# but character instead
starch_test <- read_csv(
  file = here("data", "test", "reference", "starch-reference-test.csv"),
  col_types = cols(
    starch = col_double(),
    sample_rep = col_character()))


## Join spectral data, metadata, and lab starch reference data =================

metadata_starch_test <- inner_join(x = id_metadata_test, y = starch_test)

spc_metadata_test <- inner_join(x = spc_test_offcorr, y = metadata_starch_test)


## Process data ================================================================

spc_test_proc <- spc_metadata_test %>%
  partition_spc() %>%
  split(f = .$part_id) %>%
  furrr::future_map(
    ~ .x %>%
      resample_spc(x_unit = "wavelength",
        wl_lower = 350, wl_upper = 2500, wl_interval = 1) %>%
      average_spc() %>%
      preprocess_spc(select = "sg_1_w21") %>%
      select_spc_vars(every = 4)
  ) %>%
  dplyr::bind_rows()


## Remove spectra that were read as background spectrum ========================

abs_900nm_test <- foreach::foreach(i = seq_along(spc_test_proc$spc),
  .combine = 'rbind') %do% {
    abs_900nm <- spc_test_proc$spc[[i]][1, "2500"] %>%
      as_vector();
    tibble(
      abs_900nm = abs_900nm,
      abs_bigger_0.8 = abs_900nm > 0.8)
  }

## Filter observations and filter variables based on training VIP > 1 ==========

# Filter which row has absorbance < 0.4 at 3500 cm^-1
remove_idx_test <- abs_900nm_test$abs_bigger_0.8 %>% which()

spc_test_predict <- spc_test_proc[- remove_idx, ] %>%
  select_spc_xvalues(spc_tbl = ., xvalues = wavelengths_vip_bigger1)
  
