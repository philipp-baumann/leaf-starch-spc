################################################################################
## Project:
## Description:
################################################################################

## Extract training auto-prediction and evaluate training ======================

training_self_predobs <- predict_from_spc(
    model_list = list("pls_starch" = pls_starch),
    spc_tbl = spc_train_model) %>%
  select(sample_id, harvest_time, starch, pls_starch) %>%
  mutate(eval_type = paste0("Training (n = ", nrow(.), ")"))

training_self_eval <- evaluate_model(data = training_self_predobs, 
  obs = starch, pred = pls_starch) %>% 
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

training_self_eval_lm <- lm(pls_starch ~ starch, data = training_self_predobs)

# Get xy limits for plot
xyrange_training_self <- xy_range(data = training_self_predobs,
  x = starch, y = pls_starch)

p_training_self_eval <- ggplot(data = training_self_predobs,
    aes(x = starch, y = pls_starch)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = training_self_eval_lm$coefficients[2],
    intercept = training_self_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_training_self[1] - 0.02 * diff(range(xyrange_training_self)),
       xyrange_training_self[2] + 0.02 * diff(range(xyrange_training_self))) +
  ylim(xyrange_training_self[1] - 0.02 * diff(range(xyrange_training_self)),
       xyrange_training_self[2] + 0.02 * diff(range(xyrange_training_self))) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Extract cross-validated training predictions and do model evaluation ========

training_eval_lm <- lm(pred ~ obs, data = pls_starch$predobs)

training_eval <- pls_starch$stats %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

# Get xy limits for plot
xyrange_training <- xy_range(data = pls_starch$predobs,
  x = obs, y = pred)

p_training_eval <- pls_starch$predobs %>%
  mutate(eval_type = paste0("Training cross-validated (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = , aes(x = obs, y = pred)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1) +
    geom_abline(slope = training_eval_lm$coefficients[2],
      intercept = training_eval_lm$coefficients[1], linetype = 2) +
    coord_fixed(ratio = 1) +
    facet_wrap(~ eval_type) +
    geom_text(data = training_eval,
      aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
    geom_text(data = training_eval,
      aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
    # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
    xlab("") +
    # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
    ylab("") +
    xlim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
         xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
    ylim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
         xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
    theme_bw() +
    theme(
      strip.background = element_rect(colour = "black", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )


## Combine training self-prediction and cross-validated training evaluation ====

p_eval_training_self_cv <- cowplot::plot_grid(
  p_training_self_eval, p_training_eval,
  ncol = 2) +
  draw_label(expression(paste("Measured starch [", mg~g^-1, " DM]")), 
    x = 0.55, y = 0, vjust = -0.3, angle = 0, size = 10) +
  draw_label(expression(paste("Predicted starch [", mg~g^-1, " DM]")), 
    x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 10)

p_eval_train_self_cv_pdf <- ggsave(filename = "eval-training-self-cv.pdf",
  plot = p_eval_training_self_cv,
  path = here("out", "figs"), width = 6, height = 3)


## Predict starch for test set; use exisiting training model (`pls_starch`)
## and new test data (preprocessed spectra in `spc_train`) =====================

test_predobs <- predict_from_spc(
    model_list = list("pls_starch" = pls_starch),
    spc_tbl = spc_test_predict) %>%
  select(sample_id, sample_rep, harvest_time, starch, pls_starch) %>%
  mutate(eval_type = paste0("Test using full training (n = ", nrow(.), ")"))

test_eval <- evaluate_model(data = test_predobs, 
  obs = starch, pred = pls_starch) %>% 
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

## Do a test set evaluation ====================================================

# Get xy limits for plot
xyrange_test <- xy_range(data = test_predobs, x = starch, y = pls_starch)
xyrange_test_vip_bigger1 <- xy_range(data = test_predobs_vip_bigger1,
  x = starch, y = pls_starch)

test_eval_lm <- lm(pls_starch ~ starch, data = test_predobs)
test_eval_lm_vip_bigger1 <- lm(pls_starch ~ starch,
  data = test_predobs_vip_bigger1)

p_test_eval <- ggplot(data = test_predobs, aes(x = starch, y = pls_starch)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = test_eval_lm$coefficients[2],
    intercept = test_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  ylim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_test_eval_pdf <- ggsave(filename = "test-eval.pdf", plot = p_test_eval,
  path = here("out", "figs"), width = 3, height = 3)


## Combine cross-validated training and test set evaluation ====================

p_eval_training_test <- cowplot::plot_grid(p_training_eval, p_test_eval,
  ncol = 2) +
  draw_label(expression(paste("Measured starch [", mg~g^-1, " DM]")), 
    x = 0.55, y = 0, vjust = -0.3, angle = 0, size = 10) +
  draw_label(expression(paste("Predicted starch [", mg~g^-1, " DM]")), 
    x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 10)

p_eval_pdf <- ggsave(filename = "eval.pdf", plot = p_eval_training_test,
  path = here("out", "figs"), width = 6, height = 3)


## Test evaluation grouped by genotype =========================================

# New genotype column
test_predobs_genotype <- test_predobs %>%
  mutate(genotype = map_chr(sample_rep,
    ~ stringr::str_replace(.x, pattern = "_[[:digit:]]+", replacement = ""))
  )

# Model evaluation by genotype
test_eval_genotype <- test_predobs_genotype %>%
  split(.$genotype) %>%
  map(~ evaluate_model(data = ., obs = starch, pred = pls_starch)) %>%
  imap(~ tibble::add_column(.x, genotype = .y, .before = 1)) %>%
  bind_rows() %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

# Quick plot predicted vs. observed starch
p_test_predobs_genotype <- 
  test_predobs_genotype %>%
  ggplot(aes(x = starch, y = pls_starch),
    data = .) +
  geom_abline(slope = 1, colour = "black") +
  geom_point(aes(colour = harvest_time)) +
  scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
  facet_wrap(~ genotype) +
  geom_text(data = test_eval_genotype,
      aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -2.75, parse = TRUE) +
  geom_text(data = test_eval_genotype,
      aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -1.75, parse = TRUE) +
  coord_fixed(ratio = 1) +
  xlim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  ylim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  xlab(expression(paste("Measured starch [mg ", g^{-1}, " DW]"))) +
  ylab(expression(paste("Predicted starch [mg ", g^{-1}, " DW]"))) +
  labs(colour = "Harvest time") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_test_predobs_genotype_pdf <- ggsave(
  filename = "predobs-test-genotype.pdf",
  plot = p_test_predobs_genotype, path = here("out", "figs"),
  width = 7, height = 2.5)


## Test evaluation grouped by measurement time 
## (end of night (EN) vs. end of day (ED)) =====================================

# Model evaluation by harvest time
test_eval_harvest_time <- test_predobs %>%
  split(.$harvest_time) %>%
  map(~ evaluate_model(data = ., obs = starch, pred = pls_starch)) %>%
  imap(~ tibble::add_column(.x, harvest_time = .y, .before = 1)) %>%
  bind_rows() %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

regline_test_harvest <- test_predobs_genotype %>%
  split(.$harvest_time) %>%
  map(~ lm(pls_starch ~ starch, data = .x)) %>%
  map(broom::tidy) %>%
  imap(~ add_column(.x, harvest_time = .y, .before = 1)) %>%
  map(~ tidyr::spread(.x, term, estimate) %>%
    rename(int = `(Intercept)`, slope = starch)) %>%
  map(~ list(
    int = pull(.x, int) %>% .[!is.na(.)],
    slope = pull(.x, slope) %>% .[!is.na(.)]
    )
  ) %>%
  imap(~ as_tibble(.x) %>% add_column(harvest_time = .y, .before = 1)) %>%
  bind_rows()

p_test_predobs_harvest_time <-
  test_predobs %>%
  mutate(genotype = map_chr(sample_rep,
    ~ stringr::str_replace(.x, pattern = "_[[:digit:]]+", replacement = ""))
  ) %>%
  ggplot(aes(x = starch, y = pls_starch), data = .) +
  geom_abline(slope = 1, colour = "black") +
  geom_abline(data = regline_test_harvest, aes(intercept = int, slope = slope),
    colour = "black", linetype = 2) +
  geom_point(aes(colour = genotype, shape = genotype), alpha = 0.7) +
  # scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
  facet_wrap(~ harvest_time) +
  geom_text(data = test_eval_harvest_time,
      aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -2.75, parse = TRUE) +
  geom_text(data = test_eval_harvest_time,
      aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -1.75, parse = TRUE) +
  coord_fixed(ratio = 1) +
  xlim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  ylim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  xlab(expression(paste("Measured starch [mg ", g^{-1}, " DW]"))) +
  ylab(expression(paste("Predicted starch [mg ", g^{-1}, " DW]"))) +
  labs(colour = "Genotype", shape = "Genotype") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_test_predobs_harvest_time_pdf <- ggsave(
  filename = "predobs-test-genotype-harvest-time.pdf",
  plot = p_test_predobs_harvest_time, path = here("out", "figs"),
  width = 7, height = 2.5)


## Training evaluation grouped by measurement time
## (end of night (EN) vs. end of day (ED)) =====================================

# Augument predictions with metadata
train_predobs_meta <- spc_train_model %>%
  select(sample_id, sample_age_rep, harvest_time, genotype, leaf_age, rep) %>%
  inner_join(x = ., y = pls_starch$predobs)

training_eval_harvest_time <- train_predobs_meta %>%
  # mutate(genotype = str_replace(genotype, pattern = ".[[:digit:]]", "")) %>%
  split(.$harvest_time) %>%
  map(~ evaluate_model(data = ., obs = obs, pred = pred)) %>%
  imap(~ tibble::add_column(.x, harvest_time = .y, .before = 1)) %>%
  bind_rows() %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

regline_train_harvest <- train_predobs_meta %>%
  split(.$harvest_time) %>%
  map(~ lm(pred ~ obs, data = .x)) %>%
  map(broom::tidy) %>%
  imap(~ add_column(.x, harvest_time = .y, .before = 1)) %>%
  map(~ tidyr::spread(.x, term, estimate) %>%
    rename(int = `(Intercept)`, slope = obs)) %>%
  map(~ list(
    int = pull(.x, int) %>% .[!is.na(.)],
    slope = pull(.x, slope) %>% .[!is.na(.)]
    )
  ) %>%
  imap(~ as_tibble(.x) %>% add_column(harvest_time = .y, .before = 1)) %>%
  bind_rows()

p_training_predobs_harvest_time <-
  train_predobs_meta %>%
  ggplot(aes(x = obs, y = pred), data = .) +
  geom_abline(slope = 1, colour = "black") +
  geom_abline(data = regline_train_harvest, aes(intercept = int, slope = slope),
    colour = "black") +
  geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.5) +
  # scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
  facet_wrap(~ harvest_time) +
  geom_text(data = training_eval_harvest_time,
      aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -2.75, parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
      aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -1.75, parse = TRUE) +
  coord_fixed(ratio = 1) +
  xlim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  ylim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  xlab(expression(paste("Measured starch [mg ", g^{-1}, " DW]"))) +
  ylab(expression(paste("Predicted starch [mg ", g^{-1}, " DW]"))) +
  # scale_shape_manual(values = c(0, 1, 2)) + 
  labs(colour = "Leaf age", shape = "Leaf age") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_training_predobs_harvest_time_pdf <- ggsave(
  filename = "predobs-training-harvest-time.pdf",
  plot = p_training_predobs_harvest_time, path = here("out", "figs"),
  width = 7, height = 2.5)

