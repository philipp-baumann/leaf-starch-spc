################################################################################
## Project:
## Description:
################################################################################

pkgs <- c("here", "drake", "tidyverse", "simplerspec", "data.table", "future",
  "doFuture", "ggpubr", "ChemometricsWithR")
purrr::walk(pkgs, library, character.only = TRUE)

funs <- list(
   here("R", "helpers.R"),
   here("R", "vip-wrappers.R"),
   here("R", "select-spc-xvalues.R")
)

walk(funs, source)

## Scripts to run
scripts <- c(
  "10_read-clean-process-training.R",
  "20_build-spc-model-training.R",
  "30_read-clean-process-test.R",
  "40_predict-test-spc.R"
)

plans <- map(rlang::set_names(scripts), code_to_plan)

## Define and make the plan ====================================================

# Set up parallization
plan(multicore)
registerDoFuture()

# Modified from example in section "future":
# https://ropensci.github.io/drake/articles/parallelism.html

future_plans <- list(
  eval_seq = future::sequential,
  eval_par = future::multicore
)
evaluator <- NULL

make_evaluator <- function(plan, future_plan, evaluator = NULL) {
  for (i in seq_len(nrow(plan))) {
    evaluator <- c(evaluator, future_plan)
  }
  evaluator
}

# Make the targets with the multisession future backend...
evaluator <- map(
  plans, 
  ~ make_evaluator(plan = .x, future_plan = future_plans$eval_par))

# Specify explicitly which scripts are sequential
rows_eval_seq <- c("")

for (i in rows_eval_seq) {
  evaluator[[i]] <- make_evaluator(plan = plans[[i]],
    future_plan = future_plans$eval_seq)
}

# Add evalutor columns for each sub-plan to specify future strategy
for (i in names(plans)) {
  plans[[i]]$evaluator <- evaluator[[i]]
}

## Finalize into one master drake plan, make configuration, and run workflow ===

# Combine multiple plans into one
plan <- bind_rows(
  plans
)


make(plan, parallelism = "future", jobs = 48, lazy_load = TRUE,
  keep_going = TRUE, lock_envir = FALSE, memory_strategy = "speed")
