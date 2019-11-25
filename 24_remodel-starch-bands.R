################################################################################
## Project:
## Description:
################################################################################

## Normalize starch bands known from literature to the spectral band with 
## lowest standard deviation across measurements ===============================

wl_starch <- c("556", "702", "1300", "1960")

spc_rs_train <- rbindlist(spc_train_model$spc_rs)
# Find spectral variable with the lowest standard deviation
spc_rs_train_sd_min <- spc_rs_train[, lapply(.SD, sd)] %>%
  .[, idx := apply(.SD, 1, which.min)] %>%
  pull(idx)

wl_sd_min <- colnames(spc_rs_train[, spc_rs_train_sd_min, with = FALSE])
v_sd_min <- unlist(spc_rs_train[, wl_sd_min, with = FALSE])
  
wl_starch_sd_min <- c(wl_starch, wl_sd_min)

spc_rs_starch_sdsel <- 
  spc_rs_train %>%
  .[, ..wl_starch_sd_min] %>%
  .[, c(wl_starch) := lapply(.SD, function(x) x / `670`),
    .SDcols = wl_starch] %>%
  .[, c(wl_sd_min) := NULL]


## Train a multiple linear model with selected normalized starch bands =========

seed_mlr_starch_norm <- set.seed(123L)

starch_norm_train <- spc_train_model$starch

idx_mlr_starch <- caret::createMultiFolds(y = starch_norm_train,
  k = 10, times = 5)

ctrl_mlr_starch <- caret::trainControl(method = "repeatedcv",
  index = idx_mlr_starch, savePredictions = TRUE, selectionFunction = "oneSE")

mlr_starch_norm <- caret::train(
  x = spc_rs_starch_sdsel,
  y = starch_norm_train,
  method = "lm",
  tuneLength = 2,
  trControl = ctrl_mlr_starch,
  preProcess = c("center", "scale"))


## Extract predictions and recalculate metrics =================================

eval_mlr_starch <- mlr_starch_norm$pred %>%
  add_cv_repeat(repeat_fold_col = Resample, repeat_col = Repeat) %>%
  group_by(Repeat) %>%
  nest() %>%
  mutate(
    metrics = map(data, ~ evaluate_model(data = .x, obs = obs, pred = pred))
  ) %>%
  pull(metrics) %>%
  bind_rows() %>%
  select(n, r2, rmse) %>%
  mutate(
    n = mean(n),
    r2 = mean(r2),
    rmse = mean(rmse)
  ) %>%
  slice(1L)

predobs_mlr_starch <- mlr_starch_norm$pred %>%
  group_by(rowIndex) %>%
  mutate(
    obs = mean(obs),
    pred = mean(pred)
  ) %>%
  slice(1L) %>%
  arrange(rowIndex) %>%
  bind_cols(
    .,
    spc_train_model %>% select(sample_id, leaf_age)
  )

# Plot model evaluation for MLR model with normalized starch spectral bands ----

training_eval_lm_mlr <- lm(pred ~ obs, data = predobs_mlr_starch)

training_eval_mlr <- eval_mlr_starch  %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

# Get xy limits for plot
xyrange_training_mlr <- xy_range(data = predobs_mlr_starch,
  x = obs, y = pred)

p_eval_training_mlr <- predobs_mlr_starch %>%
  mutate(eval_type = paste0("Training starch bands (cv) (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = ., aes(x = obs, y = pred)) +
    geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.4) +
    geom_abline(slope = 1) +
    geom_abline(slope = training_eval_lm_mlr$coefficients[2],
      intercept = training_eval_lm_mlr$coefficients[1], linetype = 2) +
    coord_fixed(ratio = 1) +
    facet_wrap(~ eval_type) +
    geom_text(data = training_eval_mlr,
      aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
    geom_text(data = training_eval_mlr,
      aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
    xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
    ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
    xlim(xyrange_training_mlr[1] - 0.02 * diff(range(xyrange_training_mlr)),
         xyrange_training_mlr[2] + 0.02 * diff(range(xyrange_training_mlr))) +
    ylim(xyrange_training_mlr[1] - 0.02 * diff(range(xyrange_training_mlr)),
         xyrange_training_mlr[2] + 0.02 * diff(range(xyrange_training_mlr))) +
    labs(colour = "Leaf age", shape = "Leaf age") +
    theme_bw() +
    theme(
      strip.background = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

p_eval_training_mlr_pdf <- ggsave(filename = "eval-training-mlr-cv.pdf",
  plot = p_eval_training_mlr,
  path = here("out", "figs"), width = 3, height = 3.5)
