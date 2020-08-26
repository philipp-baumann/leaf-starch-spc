################################################################################
## Project:     A non-destructive method to quantify leaf starch content in red
##              clover
## Description: 
################################################################################

# Packages
source("packages.R")

# Functions
sourceDirectory("R")

# Script-based workflow: convert to drake plan; 
# all assigned R objects in scripts are drake targets
scripts <- listDirectory("code")
plans <- map(rlang::set_names(scripts), drake::code_to_plan)

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

# Within-target parallelism with multicore or local socket cluster
drake::r_make(plan, cache_log_file = TRUE, lock_envir = FALSE)

# Predict runtime to rebuild the entire drake cache
predict_runtime(config, from_scratch = TRUE)

# make(plan, parallelism = "future", jobs = 48, lazy_load = TRUE,
#   keep_going = TRUE, lock_envir = FALSE, memory_strategy = "speed")
