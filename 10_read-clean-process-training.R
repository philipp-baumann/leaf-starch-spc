################################################################################
## Project:
## Description:
################################################################################

## Read the training data ======================================================

files_train <- dir("data/training/spectra", full.names = TRUE)

# spc_train <- furrr::future_map(.x = files_train,
#     ~ read_asd_bin(fnames = .x)) %>%
#   unlist() %>%
#   bind_rows()

spc_train_raw <- read_asd_bin(fnames = files_train)

## Select only relevant spectral columns =======================================

spc_train <- spc_train_raw %>%
  select(- c(spc_radiance, spc_reference))

## Correct join wavelengths offset for different sensors =======================

spc_train_offcorr <- simplerspec:::correct_join_offset(spc_tbl = spc_train)

## Add the IDs for the Genotype or the type of reference (black or white) ======

id_metadata_raw <- read_csv(file = "data/training/metadata/id_metadata.csv")

# Remove ".ref" at the end of the files ----------------------------------------

id_metadata <- simplerspec:::remove_id_extension(data = id_metadata_raw,
  id_col = "sample_id", id_new_nm = "sample_id")

## Read laboratory reference data: starch measurements =========================

starch <- read_csv(file = here("data", "training", "reference",
  "reference-starch.csv"),
  col_types = cols(
    starch = col_double(),
    sample_age_rep = col_character()))

## Join spectral data, metadata, and lab starch reference data =================

metadata_starch <- inner_join(x = id_metadata, y = starch)

spc_train_meta <- inner_join(x = spc_train_offcorr, y = metadata_starch)


## Clean starch data ===========================================================

# replace negative starch values with 0
spc_train_cleaned <- spc_train_meta %>%
  mutate(
    starch = case_when(
      starch < 0 ~ 0,
      TRUE ~ starch))

# Remove rows which contain NA for starch
spc_train_narm <- na.omit(spc_train_cleaned)


## Process data ================================================================

spc_train_proc <- spc_train_narm %>%
  partition_spc() %>%
  split(f = .$part_id) %>%
  furrr::future_map(
    ~ .x %>%
      resample_spc(x_unit = "wavelength",
        wl_lower = 350, wl_upper = 2500, wl_interval = 1) %>%
      average_spc(by = "sample_age_rep") %>%
      group_by(sample_age_rep) %>%
      # Keep only one row per `sample_age_pos` group after averaging
      slice(1L) %>%
      preprocess_spc(select = "sg_1_w21")
  ) %>%
  dplyr::bind_rows()
  
## String manipulation to create `genotype`, `leaf_age` and `rep` columns ======

spc_train_groupids <- spc_train_proc %>%
  mutate(
    genotype = stringr::str_split(string = sample_age_rep,
      pattern = "_")[[1]][1]
  ) %>%
   mutate(
    leaf_age = stringr::str_split(string = sample_age_rep,
      pattern = "_")[[1]][2]
  ) %>%
  mutate(
    rep = stringr::str_split(string = sample_age_rep,
      pattern = "_")[[1]][3]
  )

p_spc_train_check <- plot_spc_ext(
  spc_tbl = spc_train_groupids, 
  lcols_spc = c("spc", "spc_pre"),
  group_id = "sample_id", group_panel = FALSE) +
  xlab("Wavelength [nm]") +
  scale_x_continuous()

p_spc_train_check_pdf <- ggsave(filename = "spc-train-check.pdf",
  plot = p_spc_train_check, path = here("out", "figs"))


## Remove spectrum that was read as background spectrum ========================

# Quick-and-dirty solution to get column index position at 900nm
which_pos_rm <- which(spc_train_groupids$wavelengths_rs[[1]] == 900)
# -> has to correspond to V551

abs_900nm <- foreach::foreach(i = seq_along(spc_train_groupids$spc),
  .combine = 'rbind') %do% {
  abs_900nm <- spc_train_groupids$spc[[i]][1, "900"] %>% # before: "2500", dfdf
    as_vector();
    tibble(
      abs_900nm = abs_900nm,
      abs_bigger_0.1 = abs_900nm < 0.1)
  }

# Filter which row has absorbance < 0.2 at 3500 cm^-1===========================

remove_idx <- abs_900nm$abs_bigger_0.1 %>% which()

spc_train_model <- spc_train_groupids[- remove_idx, ]


## Plot spectra ================================================================

spc_train_model_plot <- spc_train_model %>%
  mutate(
    harvest_time = replace(harvest_time, harvest_time == "ED", "End of day"),
    harvest_time = replace(harvest_time, harvest_time == "EN", "End of night")
  )

p_spc_train_model_raw <- 
  spc_train_model_plot %>%
  plot_spc_ext(spc_tbl = ., lcols_spc = c("spc"),
    group_id = "harvest_time", ylab = "Absorbance") +
    scale_x_continuous() +
    scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
    xlab("") +
    theme_bw() +
    theme(
      strip.background = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
p_spc_train_model_pre <- 
  spc_train_model_plot %>%
  plot_spc_ext(spc_tbl = ., lcols_spc = c("spc_pre"),
    group_id = "harvest_time",ylab = "Pre-processed Abs.") +
    scale_x_continuous() +
    scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
    xlab("Wavelength [nm]") +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

p_spc_train_model <- ggarrange(p_spc_train_model_raw, p_spc_train_model_pre,
  heights = c(1, 1), ncol = 1, nrow = 2, align = "v")

p_spc_train_model_pdf <- ggsave(filename = "spc-train.pdf",
  plot = p_spc_train_model, path = here("out", "figs"),
  width = 7, height = 4)



