################################################################################
## Project:     A non-destructive method to quantify leaf starch content in red
##              clover
## Description: 
################################################################################

# Packages
source("packages.R")

# Functions
R.utils::sourceDirectory("R")

# Script-based workflow: convert to drake plan; 
# all assigned R objects in scripts are drake targets
scripts <- R.utils::listDirectory("code", fullNames = TRUE)
plans <- lapply(setNames(object = scripts, nm = scripts), drake::code_to_plan)

## Define and make the plan ====================================================

# Set up parallization
future::plan(future::multiprocess)
doFuture::registerDoFuture()

## Finalize into one master drake plan, make configuration, and run workflow ===

# Combine multiple plans into one; use fast object serialization with qs package
plan <- do.call(rbind, plans) %>% mutate(format = "qs")

# Visualize workflow
# drake::vis_drake_graph(plan) # requires visNetwork package
# drake::drake_ggraph(plan) # requires ggraph package

## Build targets (R objects) listed in plan ====================================

# Within-target parallelism with multicore or local socket cluster
drake::make(plan, cache_log_file = TRUE, lock_envir = FALSE)

drake::drake_config(plan)

# make(plan, parallelism = "future", jobs = 48, lazy_load = TRUE,
#   keep_going = TRUE, lock_envir = FALSE, memory_strategy = "speed")
