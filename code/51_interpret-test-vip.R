################################################################################
## Project:     A non-destructive method to quantify starch content
##              in red clover (T. pratense)
## Description: Interpret the PLS regression of the test set with a
##              plot showing Variable Importance in the Projection (VIP)
################################################################################

## Transform data for plotting =================================================

# Extract raw spectra (list-column <spc>) and preprocessessed spectra 
# (list-column <spc_pre>) from starch test model into a list containing
# two data.tables as separate elements -----------------------------------------

dts_test <- bind_lcols_dts(spc_tbl = pls_starch_test$data$calibration, 
  lcols = c("spc", "spc_pre"))

# Convert list of data.tables to long form -------------------------------------

dts_test_long <- purrr::imap(dts_test, function(dt, nm) {
  dt <- data.table::melt(dt, 
    id.vars = c("spc_id", "lcol_type", "group_id"),
    variable.factor = FALSE,
    variable.name = "wavelength");
  dt[, wavelength := as.numeric(wavelength)]
})

# Annotate peaks in raw spectra based on peak picking --------------------------

# Extract raw spectra in single data.table
spc_test_model_dt <- pull(spc_test_predict, spc) %>%
  data.table::rbindlist()

# Extract first spectrum in data.table as vector
spc_test_1 <- as.matrix(spc_test_model_dt)[1, ]

# Find possible peaks
peaks_test <- pick.peaks(spc_test_1, 100)
# Filter spectral noise from prominent peaks (manually)
len_test <- length(peaks_test)

# Check visually
plt_spc_test <- plot(spc_test_1, type = "l")
# plt_peaks_test <- abline(v = peaks_test, col = "blue")

# Return wavenumbers of peaks
wl_test <- spc_test_predict$wavelengths[[1]]
wl_peaks_test <- wl_test[peaks_test]

# Merge soil component abbreviations to peaks
constituents_test <- rep("", length(wl_peaks_test))

annotation_const_test <- paste0(
  round(wl_peaks_test, 0), constituents_test, sep = "")


## Plot spectra and VIP scores =================================================

# General graph settings
alpha_vip_test <- 0.15
xlab_vip_test <- expression(paste("Wavelength [nm]"))
ylab1_vip_test <-  "Reflectance (raw)"; ylab2_test <- "Reflectance (filtered)"
group_id_vip_test <- "sample_id"

# Pretty Axis breaks
brk_vip_test <- pretty(
  as.numeric(names(dts_test[["spc"]])[!names(dts_test[["spc"]]) %in% 
    c("spc_id", "lcol_type", "group_id")])
)

x_lim_test <- minmax(
  as.numeric(names(dts_test[["spc"]])[!names(dts_test[["spc"]]) %in%
    c("spc_id", "lcol_type", "group_id")])
)

# Extract VIP (variable importance in projection) scores and
# PLS regression coefficients
df_vip_pls_test <- extract_multi_pls_vip_coef(
    mout_list = list("PLSR test starch" = pls_starch_test)) %>%
  # rename(wavelength = wavenumber) %>%
  mutate(coef_bigger0 = as.integer(coef > 0)) %>% 
  mutate(coef_bigger0 = coef_bigger0 / 10) %>% 
  mutate(coef_bigger0 = coef_bigger0 - 0.05)

# Determine highlighted regions above VIP = 1
rects_test <- create_vip_rects(
  df_vip_pls_test[df_vip_pls_test$model == "PLSR test starch", ])

# Plot mean replicate spectra --------------------------------------------------

p_spc_test <- ggplot(dts_test_long[["spc"]], 
    aes(x = wavelength, y = value)) +
  geom_rect(data = rects_test, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(dts_test_long[["spc"]]$value),
      ymax = max(dts_test_long[["spc"]]$value), group = group), 
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(data = dts_test_long[["spc"]], inherit.aes = FALSE,
    aes(x = wavelength, y = value, group = group_id),
      alpha = alpha_vip_test, size = 0.2) +
  scale_x_continuous(limits = x_lim_test, breaks = brk_vip_test) +
  ylim(c(-0.05, 0.55)) +
  labs(x = xlab_vip_test, y = ylab1_vip_test) +
  theme_bw() +
  theme(
    plot.margin = unit(c(1, 5, -30, 6),
    units = "points"),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 10, vjust = 0.25)  
  )

p_spc_pre_test <- ggplot(dts_test_long[["spc_pre"]],
    aes(wavelength, value)) +
  geom_rect(data = rects_test, inherit.aes = FALSE,
    aes(xmin = start,
      xmax = end, ymin = min(dts_test_long[["spc_pre"]]$value),
      ymax = max(dts_test_long[["spc_pre"]]$value), group = group),
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(aes(group = group_id),
    alpha = alpha_vip_test, size = 0.2) +
  labs(x = xlab_vip_test, y = ylab2_test) +
  theme_bw() +
  theme(
    plot.margin = unit(c(0, 5, 4, 1),
    units = "points")) +
  scale_x_continuous(limits = x_lim, breaks = brk_vip_test) +
  ylim(c(-0.009, 0.013)) +
  theme(
    plot.margin = unit(c(1, 5, -25, 6),
    units = "points"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10, vjust = 0.25),
    axis.text.x = element_blank()
)

# Plot VIP ---------------------------------------------------------------------

p_vip_test <- ggplot(data = df_vip_pls_test,
    aes(x = wavenumber, y = vip)) +
  geom_rect(data = rects_test, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(df_vip_pls_test$vip),
    ymax = max(df_vip_pls_test$vip), group = group), color = "transparent",
    fill = "orange", alpha = 0.3) +
  geom_hline(yintercept = 1, colour = "black") +
  geom_line(aes(colour = model), size = 0.55) +
  ylim(c(min(df_vip_pls_test$vip) - 0.3, max(df_vip_pls_test$vip))) +
  xlab("") +
  ylab("VIP") +
  scale_x_continuous(limits = x_lim_test, breaks = brk_vip_test) +
  theme_bw() +
  theme(
    plot.margin = unit(c(1, 5, -23, 6), units = "points"), # unit(c(0, 5, 1, 1)
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 10, vjust = 0.25),
    legend.position = "none"
  )

# Plot PLS regression coefficients ---------------------------------------------

p_coef_test <- ggplot(data = df_vip_pls_test,
    aes(x = wavenumber, y = coef)) +
  geom_rect(data = rects_test, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(df_vip_pls_test$coef),
    ymax = max(df_vip_pls_test$coef), group = group), color = "transparent",
    fill = "orange", alpha = 0.3) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(aes(colour = model), size = 0.55) +
  xlab(xlab_vip_test) +
  ylab(expression(paste("PLSR coef. (", italic(b), ")"))) +
  scale_x_continuous(limits = x_lim_test, breaks = brk_vip_test) +
  theme_bw() +
  theme(
    plot.margin = unit(c(0, 5, 1, 1), units = "points"), 
    axis.title.x = element_text(size = 10, vjust = 0.25),
    axis.title.y = element_text(size = 10, vjust = 0.25),
    legend.position = "none"
  )

# Arrange plots in panels without margins --------------------------------------
# No margins in between

p_vip_test_comb <- cowplot::plot_grid(
  p_spc_test, p_spc_pre_test, p_vip_test, p_coef_test,
  rel_heights = c(0.35, 0.3, 0.2, 0.3), ncol = 1, align = "v")

p_vip_test_comb_pdf <- ggsave(filename = "spc-starch-pls-test-vip.pdf",
  plot = p_vip_test_comb, 
  path = here("out", "figs"), width = 6.69, height = 5.25)

p_vip_test_comb_pdf_pub <- ggsave(filename = "S6.pdf",
  plot = p_vip_test_comb, 
  path = here("pub", "figs"), width = 6.69, height = 5.25)
