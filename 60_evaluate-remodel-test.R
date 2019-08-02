################################################################################
## Project:
## Description:
################################################################################

## Extract cross-validated predictions for VIP filtered test re-prediction
## and do model evaluation =====================================================

test_vip_bigger1_eval <- pls_starch_test_vip_bigger1$stats %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )

test_eval_lm_vip_bigger1 <- lm(pred ~ obs,
  data = pls_starch_test_vip_bigger1$predobs)

# Get xy limits for plot
xyrange_test_vip_bigger1 <- xy_range(data = pls_starch_test_vip_bigger1$predobs,
  x = obs, y = pred)

p_test_vip_bigger1_eval <- pls_starch_test_vip_bigger1$predobs %>%
  mutate(
    eval_type = paste0("Test recalibration (cv), VIP train > 1 (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = .,
    aes(x = obs, y = pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = test_eval_lm_vip_bigger1$coefficients[2],
    intercept = test_eval_lm_vip_bigger1$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = test_vip_bigger1_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
  geom_text(data = test_vip_bigger1_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_test_vip_bigger1[1] - 0.02 * diff(range(xyrange_test_vip_bigger1)),
       xyrange_test_vip_bigger1[2] + 0.02 * diff(range(xyrange_test_vip_bigger1))) +
  ylim(xyrange_test_vip_bigger1[1] - 0.02 * diff(range(xyrange_test_vip_bigger1)),
       xyrange_test_vip_bigger1[2] + 0.02 * diff(range(xyrange_test_vip_bigger1))) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Create model evaluation graph for re-calibrated test set ====================

test_pls_eval <- pls_starch_test$stats %>%
  # Modify columns for plot annotation
  mutate(
    rmse = as.character(as.expression(paste0("RMSE == ", "~",
      "'", sprintf("%.0f", rmse), "'"))),
    r2 = as.character(as.expression(paste0("italic(R)^2 == ", "~",
      "'", sprintf("%.2f", r2), "'"))),
    one_one = "1:1"
  )
  
test_pls_eval_lm <- lm(pred ~ obs, data = pls_starch_test$predobs)

p_test_pls_eval <- pls_starch_test$predobs %>%
  mutate(
    eval_type = paste0(
      "Test recalibration (cv), full model (n = ", nrow(.), ")")
  ) %>%
  ggplot(data = .,
    aes(x = obs, y = pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1) +
  geom_abline(slope = test_pls_eval_lm$coefficients[2],
    intercept = test_pls_eval_lm$coefficients[1], linetype = 2) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ eval_type) +
  geom_text(data = test_pls_eval,
    aes(x = Inf, y = -Inf, label = r2), size = 3,
      hjust = 1.27, vjust = -3.5, parse = TRUE) +
  geom_text(data = test_pls_eval,
    aes(x = Inf, y = -Inf, label = rmse), size = 3,
      hjust = 1.08, vjust = -2.5, parse = TRUE) +
  # xlab(expression(paste("Measured starch [", mg~g^-1, " DM]"))) +
  xlab("") +
  ylab("") +
  # ylab(expression(paste("Predicted starch [", mg~g^-1, " DM]"))) +
  xlim(xyrange_test_vip_bigger1[1] - 0.02 * diff(range(xyrange_test_vip_bigger1)),
       xyrange_test_vip_bigger1[2] + 0.02 * diff(range(xyrange_test_vip_bigger1))) +
  ylim(xyrange_test_vip_bigger1[1] - 0.02 * diff(range(xyrange_test_vip_bigger1)),
       xyrange_test_vip_bigger1[2] + 0.02 * diff(range(xyrange_test_vip_bigger1))) +
  theme_bw() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## Compile graph that compares test re-calibration without vs. with VIP filtered
## variables as determined during training =====================================

p_eval_test_pls <- cowplot::plot_grid(
  p_test_pls_eval, p_test_vip_bigger1_eval,
  ncol = 2) +
  draw_label(expression(paste("Measured starch [", mg~g^-1, " DM]")), 
    x = 0.55, y = 0, vjust = -0.3, angle = 0, size = 10) +
  draw_label(expression(paste("Predicted starch [", mg~g^-1, " DM]")), 
    x = 0, y = 0.5, vjust = 1.5, angle = 90, size = 10)

p_eval_test_pls_pdf <- ggsave(
  filename = "test-eval-pls-allvars-vip.pdf",
  plot = p_eval_test_pls, path = here("out", "figs"), 
  width = 7, height = 3.25)
