################################################################################
## Project:
## Description:
################################################################################

pkgs <- c(
  # reproducibility and data wrangling
  "here", "drake", "tidyverse", "data.table",
  "simplerspec", "ChemometricsWithR", # spectroscopy
  "future", "furrr", "doFuture", # parallel computation
  "ggpubr", "cowplot", # graphics
  "lineup", "varrank") # correlation and information theory metrics
purrr::walk(pkgs, library, character.only = TRUE)

funs <- list(
   here("R", "helpers.R"),
   here("R", "modeling.R"),
   here("R", "vip-wrappers.R"),
   here("R", "select-spc-xvalues.R")
)

walk(funs, source)

## Scripts to run
scripts <- c(
  "10_read-clean-process-training.R",
  "20_build-spc-model-training.R",
  "21_interpret-training-vip.R",
  "22_remodel-vip-filtering.R",
  "23_remodel-cor-filtering.R",
  "24_remodel-starch-bands.R",
  "30_read-clean-process-test.R", # rest takes longer; graphs
  "31_visualize-refdata.R",
  "40_predict-evaluate-train-test.R",
  "50_remodel-test.R",
  "51_interpret-test-vip.R",
  "52_remodel-test-vip-training.R",
  "60_evaluate-test.R"
)

plans <- map(rlang::set_names(scripts), code_to_plan)

## Define and make the plan ====================================================

# Set up parallization
plan(multiprocess)
registerDoFuture()

## Finalize into one master drake plan, make configuration, and run workflow ===

# Combine multiple plans into one
plan <- bind_rows(
  plans
)

## Build targets (R objects) listed in plan ====================================

# Record start time
start_time <- Sys.time()
make(plan, cache_log_file = TRUE, lock_envir = FALSE)
end_time <- Sys.time()

exec_time <- end_time - start_time

# Predict runtime to rebuild the entire drake cache
predict_runtime(config, from_scratch = TRUE)

# make(plan, parallelism = "future", jobs = 48, lazy_load = TRUE,
#   keep_going = TRUE, lock_envir = FALSE, memory_strategy = "speed")
