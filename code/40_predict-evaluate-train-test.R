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
    ncomp = as.character(as.expression(paste0("ncomp == ", "~",
      "'", sprintf("%.0f", pls_starch$stats$ncomp), "'"))),
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
    one_one = "1:1"
  )

training_self_eval_lm <- lm(pls_starch ~ starch, data = training_self_predobs)

# Expression for regresson equation
training_self_lm_eqn <- lm_eqn(lm_object = training_self_eval_lm)

# Get xy limits for plot
xyrange_training_self <- xy_range(data = training_self_predobs,
  x = starch, y = pls_starch)

p_training_self_eval <-
  training_self_predobs %>%
  inner_join(x = ., y = spc_train_model %>% select(sample_id, leaf_age)) %>%
  ggplot(data = .,
    aes(x = starch, y = pls_starch)) +
  geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = training_self_eval_lm$coefficients[2],
    intercept = training_self_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = ncomp), size = 2.75,
      hjust = 1.1, vjust = -7.5, parse = TRUE) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 2.75,
      hjust = 1.1, vjust = -5.5, parse = TRUE) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 2.75,
      hjust = 1.06, vjust = -5, parse = TRUE) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = bias), size = 2.75,
      hjust = 1.08, vjust = -3.25, parse = TRUE) +
  geom_text(data = training_self_eval,
    aes(x = Inf, y = -Inf, label = rpd), size = 2.75,
      hjust = 1.08, vjust = -1.0, parse = TRUE) +
  annotate("text", x = 100, y = 45, label = training_self_lm_eqn,
    parse = TRUE, size = 2.75) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_training_self[1] - 0.02 * diff(range(xyrange_training_self)),
       xyrange_training_self[2] + 0.02 * diff(range(xyrange_training_self))) +
  ylim(xyrange_training_self[1] - 0.02 * diff(range(xyrange_training_self)),
       xyrange_training_self[2] + 0.02 * diff(range(xyrange_training_self))) +
  labs(colour = "Leaf age", shape = "Leaf age") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Extract cross-validated training predictions and do model evaluation ========

training_eval_lm <- lm(pred ~ obs, data = pls_starch$predobs)

# Expression for regresson equation
training_lm_eqn <- lm_eqn(lm_object = training_eval_lm)

training_eval <- pls_starch$stats %>%
  # Modify columns for plot annotation
  mutate(
    ncomp = as.character(as.expression(paste0("ncomp == ", "~",
      "'", sprintf("%.0f", pls_starch$stats$ncomp), "'"))),
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
    one_one = "1:1"
  )

# Get xy limits for plot
xyrange_training <- xy_range(data = pls_starch$predobs,
  x = obs, y = pred)

p_training_eval <- 
  pls_starch$predobs %>%
  inner_join(x = ., y = spc_train_model %>% select(sample_id, leaf_age)) %>%
  mutate(eval_type = paste0("Training cross-validated (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = ., aes(x = obs, y = pred)) +
  geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = training_eval_lm$coefficients[2],
    intercept = training_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = training_eval,
    aes(x = Inf, y = -Inf, label = ncomp), size = 2.75,
      hjust = 1.1, vjust = -7.5, parse = TRUE) +
  geom_text(data = training_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 2.75,
      hjust = 1.1, vjust = -5.5, parse = TRUE) +
  geom_text(data = training_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 2.75,
      hjust = 1.06, vjust = -5, parse = TRUE) +
  geom_text(data = training_eval,
    aes(x = Inf, y = -Inf, label = bias), size = 2.75,
      hjust = 1.08, vjust = -3.25, parse = TRUE) +
  geom_text(data = training_eval,
    aes(x = Inf, y = -Inf, label = rpd), size = 2.75,
      hjust = 1.08, vjust = -1.0, parse = TRUE) +
  annotate("text", x = 100, y = 45, label = training_lm_eqn,
    parse = TRUE, size = 2.75) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  ylab("") +
  xlim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  ylim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  labs(colour = "Leaf age", shape = "Leaf age") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Combine training self-prediction and cross-validated training evaluation ====

p_eval_training_self_cv <- cowplot::plot_grid(
  p_training_self_eval + theme(legend.position = "none"),
  p_training_eval + theme(legend.position = "none"),
  p_training_self_eval %>% get_legend() +
    theme(
      legend.margin = margin(0, -10, 0, 0),
      legend.margin = margin(0, -10, 0, 0),
      legend.title = theme_text(size = 8)),
  nrow = 1,
  rel_widths = c(1, 1, 0.2),
  labels = c("A", "B")
  ) +
  draw_label(expression(paste("Measured starch [", mg~g^-1, " DM]")), 
    x = 0.5, y = 0, vjust = -0.3, angle = 0, size = 10) +
  draw_label(expression(paste("Predicted starch [", mg~g^-1, " DM]")), 
    x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 10)

p_eval_train_self_cv_pdf <- ggsave(filename = "eval-training-self-cv.pdf",
  plot = p_eval_training_self_cv,
  path = here("out", "figs"), width = 6.5, height = 3)

p_eval_train_self_cv_pdf_pub <- ggsave(
  filename = "Fig7.pdf",
  plot = p_eval_training_self_cv,
  path = here("pub", "figs"), width = 6.69, height = 3)


## Model evaluation for PLSR training with raw spectral data ===================

training_raw_eval_lm <- lm(pred ~ obs, data = pls_starch_raw$predobs)

# Expression for regression equation
training_raw_lm_eqn <- lm_eqn(lm_object = training_raw_eval_lm)

training_raw_eval <- pls_starch_raw$stats %>%
  # Modify columns for plot annotation
  mutate(
    ncomp = as.character(as.expression(paste0("ncomp == ", "~",
      "'", sprintf("%.0f", pls_starch_raw$stats$ncomp), "'"))),
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
    one_one = "1:1"
  )

# Get xy limits for plot
xyrange_training_raw <- xy_range(data = pls_starch_raw$predobs,
  x = obs, y = pred)

p_training_raw_eval <- 
  pls_starch_raw$predobs %>%
  inner_join(x = ., y = spc_train_model %>% select(sample_id, leaf_age)) %>%
  mutate(eval_type = paste0("Training CV (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = ., aes(x = obs, y = pred)) +
    geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.4) +
    geom_abline(slope = 1) +
    geom_abline(slope = training_raw_eval_lm$coefficients[2],
      intercept = training_raw_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = training_raw_eval,
    aes(x = 7.5, y = 57, label = ncomp), size = 2.75,
      hjust = 0, vjust = 0, parse = TRUE) +
  geom_text(data = training_raw_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 2.75,
      hjust = 1.1, vjust = -5.5, parse = TRUE) +
  geom_text(data = training_raw_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 2.75,
      hjust = 1.06, vjust = -5, parse = TRUE) +
  geom_text(data = training_raw_eval,
    aes(x = Inf, y = -Inf, label = bias), size = 2.75,
      hjust = 1.08, vjust = -3.25, parse = TRUE) +
  geom_text(data = training_raw_eval,
    aes(x = Inf, y = -Inf, label = rpd), size = 2.75,
      hjust = 1.08, vjust = -1.0, parse = TRUE) +
  annotate("text", x = 97, y = 62, label = training_raw_lm_eqn,
    parse = TRUE, size = 2.75) +
  xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_training_raw[1] - 0.02 * diff(range(xyrange_training_raw)),
       xyrange_training_raw[2] + 0.02 * diff(range(xyrange_training_raw))) +
  ylim(xyrange_training_raw[1] - 0.02 * diff(range(xyrange_training_raw)),
       xyrange_training_raw[2] + 0.02 * diff(range(xyrange_training_raw))) +
  labs(colour = "Leaf age", shape = "Leaf age") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_eval_train_raw_pdf <- ggsave(filename = "eval-training-raw-cv.pdf",
  plot = p_training_raw_eval,
  path = here("out", "figs"), width = 3.34, height = 2.5)

p_eval_train_raw_pdf_pub <- ggsave(filename = "S2.pdf",
  plot = p_training_raw_eval,
  path = here("pub", "figs"), width = 3.34, height = 2.5)


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
    ncomp = as.character(as.expression(paste0("ncomp == ", "~",
      "'", sprintf("%.0f", pls_starch$stats$ncomp), "'"))),
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
    one_one = "1:1"
  )

test_predobs_vip_bigger1 <- predict_from_spc(
    model_list = list("pls_starch" = pls_starch_vip_bigger1),
    spc_tbl = spc_test_predict_vip_bigger1) %>%
  select(sample_id, sample_rep, harvest_time, starch, pls_starch) %>%
  mutate(eval_type = paste0("Test using VIP training (n = ", nrow(.), ")"))

test_vip_bigger1_eval <- evaluate_model(data = test_predobs_vip_bigger1,
  obs = starch, pred = pls_starch) %>% 
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
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

# Expression for regression equation
test_eval_lm_eqn <- lm_eqn(lm_object = test_eval_lm)

p_test_eval <- 
  test_predobs %>%
  mutate(
    leaf_age = "m"
  ) %>%
  ggplot(data = ., aes(x = starch, y = pls_starch)) +
  geom_point(aes(colour = leaf_age, shape = leaf_age),
    colour = "#F8766D", alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = test_eval_lm$coefficients[2],
    intercept = test_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = ncomp), size = 2.75,
      hjust = 1.1, vjust = -7.25, parse = TRUE) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 2.75,
      hjust = 1.1, vjust = -5.5, parse = TRUE) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 2.75,
      hjust = 1.06, vjust = -5, parse = TRUE) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = bias), size = 2.75,
      hjust = 1.08, vjust = -3.25, parse = TRUE) +
  geom_text(data = test_eval,
    aes(x = Inf, y = -Inf, label = rpd), size = 2.75,
      hjust = 1.08, vjust = -1.0, parse = TRUE) +
  annotate("text", x = 119, y = 76, label = test_eval_lm_eqn,
    parse = TRUE, size = 2.75) +
  xlab(expression(paste("Measured starch [mg ", g^{-1}, " DW]"))) +
  ylab(expression(paste("Predicted starch [mg ", g^{-1}, " DW]"))) +
  xlim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  ylim(xyrange_test[1] - 0.02 * diff(range(xyrange_test)),
       xyrange_test[2] + 0.02 * diff(range(xyrange_test))) +
  labs(colour = "Leaf age", shape = "Leaf age") +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

p_test_eval_pdf <- ggsave(filename = "test-eval.pdf", plot = p_test_eval,
  path = here("out", "figs"), width = 3.34, height = 3.34)

p_test_eval_pdf_pub <- ggsave(
  filename = "Fig8.pdf", plot = p_test_eval,
  path = here("pub", "figs"), width = 3.34, height = 3.34)


## Combine cross-validated training and test set evaluation ====================

p_training_eval_nolabs <- 
  p_training_eval +
  theme(legend.position = "none")

p_test_eval_nolabs <- 
  p_test_eval + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none")

p_training_eval_legend <- get_legend(
  p_training_eval +
    theme(
      legend.box.margin = margin(0, 0, 0, 12),
      legend.justification = "center"
    )
)

# arrange the three plots in a single row
p_eval_prow <- plot_grid(
  p_training_eval_nolabs,
  p_test_eval_nolabs,
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1
)

p_eval_training_test <- cowplot::plot_grid(
  p_eval_prow,
  p_training_eval_legend,
  align = "vh", axis = "m",
  rel_widths = c(3, .4),
  ncol = 2) +
  draw_label(expression(paste("Measured starch [", mg~g^-1, " DM]")), 
    x = 0.55, y = 0, vjust = -0.3, angle = 0, size = 10) +
  draw_label(expression(paste("Predicted starch [", mg~g^-1, " DM]")), 
    x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 10)

p_eval_pdf <- ggsave(filename = "eval.pdf", plot = p_eval_training_test,
  path = here("out", "figs"), width = 6, height = 3)

p_eval_pdf_pub <- ggsave(filename = "eval.pdf", plot = p_eval_training_test,
  path = here("pub", "figs"), width = 6.69, height = 3)


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
  geom_abline(slope = 1, colour = "grey") +
  geom_point(aes(colour = harvest_time, shape = harvest_time)) +
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
  labs(shape = "Harvest time") +
  scale_shape_manual(values = c(1, 2)) +
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
    n = as.character(as.expression(paste0("italic(n) == ", "~", n))),
    ncomp = rep(as.character(as.expression(paste0("ncomp == ", "~",
      "'", sprintf("%.0f", pls_starch$stats$ncomp), "'"))), 2),
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.1f", rmse), "'"))),
    bias = as.character(as.expression(paste0("bias == ", "~",
      "'", sprintf("%.1f", bias), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    rpd = as.character(as.expression(paste0("RPD == ", "~",
      "'", sprintf("%.1f", rpd), "'"))),
    one_one = "1:1"
  )

train_harvest_lm_lst <- train_predobs_meta %>%
  split(.$harvest_time) %>%
  map(~ lm(pred ~ obs, data = .x))

regline_train_harvest <- 
  train_harvest_lm_lst %>%
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

# Expression for regresson equation
training_harvest_lm_eqn <- 
  train_harvest_lm_lst %>%
  map(~ lm_eqn(lm_object = .x)) %>%
  map(~ tibble(label = .x)) %>%
  imap_dfr(~ add_column(.x, harvest_time = .y))

p_training_predobs_harvest_time <-
  train_predobs_meta %>%
  ggplot(aes(x = obs, y = pred), data = .) +
  geom_abline(slope = 1, colour = "black") +
  geom_abline(data = regline_train_harvest, aes(intercept = int, slope = slope),
    colour = "black", linetype = 2) +
  geom_point(aes(colour = leaf_age, shape = leaf_age), alpha = 0.5) +
  # scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
  facet_wrap(~ harvest_time) +
  geom_text(data = training_eval_harvest_time,
    aes(x = 116.5, y = 41, label = n), size = 2.75,
    parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
    aes(x = Inf, y = -Inf, label = ncomp), size = 2.75,
    hjust = 1.1, vjust = -7.5, parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
    aes(x = Inf, y = -Inf, label = r2), size = 2.75,
      hjust = 1.1, vjust = -5.5, parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
    aes(x = Inf, y = -Inf, label = rmse), size = 2.75,
    hjust = 1.06, vjust = -5, parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
    aes(x = Inf, y = -Inf, label = bias), size = 2.75,
    hjust = 1.08, vjust = -3.25, parse = TRUE) +
  geom_text(data = training_eval_harvest_time,
    aes(x = Inf, y = -Inf, label = rpd), size = 2.75,
    hjust = 1.08, vjust = -1.0, parse = TRUE) +
  geom_text(data = training_harvest_lm_eqn,
    aes(x = 105, y = 52.5, label = label), size = 2.75,
    parse = TRUE) +
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
  width = 6.69, height = 3.4)

p_training_predobs_harvest_time_pdf_pub <- ggsave(
  filename = "S3.pdf",
  plot = p_training_predobs_harvest_time, path = here("pub", "figs"),
  width = 6.69, height = 3.4)


## Training evaluation grouped by genotype =====================================

# Remove dot and number in genotype
train_predobs_meta_genotype <-  
  train_predobs_meta %>%
  mutate(
    genotype_nodot = stringr::str_split(string = genotype,
      pattern = "[.]")
  ) %>%
  mutate(
    genotype_nodot = unlist(purrr::map(genotype_nodot, 1))
  )

# Model evaluation by genotype
train_eval_genotype <-
  train_predobs_meta_genotype %>%
  split(.$genotype_nodot) %>%
  map(~ evaluate_model(data = ., obs = obs, pred = pred)) %>%
  imap(~ tibble::add_column(.x, genotype_nodot = .y, .before = 1)) %>%
  bind_rows() %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

# Plot predicted vs. observed by genotype
p_train_predobs_genotype <- 
  train_predobs_meta_genotype %>%
  ggplot(aes(x = obs, y = pred),
    data = .) +
  geom_abline(slope = 1, colour = "grey") +
  geom_point(aes(colour = harvest_time, shape = harvest_time)) +
  scale_colour_manual(values = c("#d7191c", "#2b83ba")) +
  facet_wrap(~ genotype_nodot) +
  geom_text(data = train_eval_genotype,
      aes(x = -Inf, y = Inf, label = r2), size = 2.75,
      hjust = -0.07, vjust = 1.5, parse = TRUE) +
  geom_text(data = train_eval_genotype,
      aes(x = -Inf, y = Inf, label = rmse), size = 2.75,
      hjust = -0.07, vjust = 4, parse = TRUE) +
  coord_fixed(ratio = 1) +
  xlim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  ylim(xyrange_training[1] - 0.02 * diff(range(xyrange_training)),
       xyrange_training[2] + 0.02 * diff(range(xyrange_training))) +
  xlab(expression(paste("Measured starch [mg ", g^{-1}, " DW]"))) +
  ylab(expression(paste("Predicted starch [mg ", g^{-1}, " DW]"))) +
  labs(colour = "Harvest time", shape = "Harvest time") +
  scale_shape_manual(values = c(1, 2)) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text.x = element_text(size = 8),
    text = element_text(size = 7)
  )

p_train_predobs_genotype_pdf <- ggsave(
  filename = "predobs-train-genotype.pdf",
  plot = p_train_predobs_genotype, path = here("out", "figs"),
  width = 6.69, height = 5.5)


## Test predictions using correlation filtered training model ==================

spc_test_predict_corfilt <- select_spc_xvalues(
  spc_tbl = spc_test_predict, xvalues = wl_cor_top50, column_in = "spc_pre",
  xvalues_in = "xvalues_pre") %>%
  mutate(eval_type = paste0("Test correlation filtering (n = ", nrow(.), ")"))

test_corfilt_predobs <- predict_from_spc(
  model_list = list("pls_starch_corfilt" = pls_starch_corfilt),
  spc_tbl = spc_test_predict_corfilt)

test_corfilt_eval <- evaluate_model(data = test_corfilt_predobs, 
  obs = starch, pred = pls_starch_corfilt) %>% 
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

xyrange_test_corfilt <- xy_range(data = test_corfilt_predobs,
  x = starch, y = pls_starch_corfilt)

test_eval_lm_corfilt <- lm(
  pls_starch_corfilt ~ starch, 
  data = test_corfilt_predobs)

p_test_corfilt_predobs <- test_corfilt_predobs %>%
  ggplot(aes(x = starch, y = pls_starch_corfilt)) +
  geom_point() + # alpha = 0.4
  geom_abline(slope = 1) +
  geom_abline(slope = test_eval_lm_corfilt$coefficients[2],
    intercept = test_eval_lm_corfilt$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = test_corfilt_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
  geom_text(data = test_corfilt_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_test_corfilt[1] - 0.02 * diff(range(xyrange_test_corfilt)),
       xyrange_test_corfilt[2] + 0.02 * diff(range(xyrange_test_corfilt))) +
  ylim(xyrange_test_corfilt[1] - 0.02 * diff(range(xyrange_test_corfilt)),
       xyrange_test_corfilt[2] + 0.02 * diff(range(xyrange_test_corfilt))) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_test_corfilt_predobs_pdf <- ggsave(
  filename = "predobs-test-corfilt.pdf",
  plot = p_test_corfilt_predobs, path = here("out", "figs"),
  width = 3.5, height = 3.5)



## Test predictions using selected normalized starch bands =====================

spc_rs_test_predict <- rbindlist(spc_test_predict$spc_rs)

# spc_rs_test_starch_sdsel <- spc_rs_test_predict[, ..wl_starch_sd_min]
# 
# spc_rs_test_starch_sdsel[, c(wl_starch) := lapply(.SD,
#   function(x) x / `670`), .SDcols = wl_starch]
# 
# spc_rs_test_starch_sdsel[, c(wl_sd_min) := NULL]

## Avoid duplicated target in drake; do piping
spc_rs_test_starch_sdsel <- 
  spc_rs_test_predict %>%
  .[, c(wl_starch_sd_min), with = FALSE] %>%
  .[, c(wl_starch) := lapply(.SD,
    function(x) x / eval(parse(text = wl_sd_min))), .SDcols = c(wl_starch)] %>%
  .[, c(wl_sd_min) := NULL]

spc_test_predict_starchfilt_vec <- predict.lm(
  object = mlr_starch_norm$finalModel, newdata = spc_rs_test_starch_sdsel)

spc_test_predict_starchfilt <- spc_test_predict %>%
  mutate(mlr_starchfilt = spc_test_predict_starchfilt_vec)

test_starchfilt_eval <-  evaluate_model(data = spc_test_predict_starchfilt, 
  obs = starch, pred = mlr_starchfilt) %>% 
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )


