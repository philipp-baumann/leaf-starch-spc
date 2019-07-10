################################################################################
## Project:
## Description:
################################################################################

# Create a list of models
mout_list <- list(
  "PLSR starch" = pls_starch
)

## Transform data for plotting =================================================

# Extract raw spectra (list-column <spc>) and preprocessessed spectra 
# (list-column <spc_pre>) from starch training model into a list containing
# two data.tables as separate elements ----

dts <- bind_lcols_dts(spc_tbl = pls_starch$data$calibration, 
  lcols = c("spc", "spc_pre"))

# Convert list of data.tables to long form -------------------------------------

dts_long <- purrr::imap(dts, function(dt, nm) {
  dt <- data.table::melt(dt, 
    id.vars = c("spc_id", "lcol_type", "group_id"),
    variable.factor = FALSE,
    variable.name = "wavelength");
  dt[, wavelength := as.numeric(wavelength)]
})

# Annotate peaks in raw spectra based on peak picking --------------------------

# Extract raw spectra in single data.table
spc_train_model_dt <- pull(spc_train_model, spc) %>%
  data.table::rbindlist()

# Extract first spectrum in data.table as vector
spc_1 <- as.matrix(spc_train_model_dt)[1, ]

# Find possible peaks
peaks <- pick.peaks(spc_1, 100)
# Filter spectral noise from prominent peaks (manually)
len <- length(peaks)

# Check visually
plt_spc <- plot(spc_1, type = "l")
plt_peaks <- abline(v = peaks, col = "blue")

# Return wavenumbers of peaks
wl <- spc_train_model$wavelengths[[1]]
wl_peaks <- wl[peaks]

# Merge soil component abbreviations to peaks
constituents <- rep("", length(wl_peaks))

# constituents[1] <- " O-H (K)"
# constituents[3] <- " O-H (K)"
# constituents[4] <- " Alkyl C-H"
# constituents[5] <- " Alkyl C-H"
# constituents[10:13] <- rep(" Si-O (Q)", 4)
# constituents[22] <- " Al-OH (K)"
# constituents[23] <- " Al-OH (K)"

annotation_const <- paste0(round(wl_peaks, 0), constituents, sep = "")

df_peak <- tibble(
  wavenumber = wl_peaks,
  annotation = annotation_const,
  y_peaks = spc_max_peaks
)

## Plot spectra and VIP scores =================================================

# Prepare plotting data --------------------------------------------------------

# General graph settings
alpha <- 0.15
xlab <- expression(paste("Wavelength [nm]"))
ylab1 <-  "Absorbance (Abs.)"; ylab2 <- "Preproc. Abs."
group_id <- "sample_id"

# Pretty Axis breaks
brk <- pretty(as.numeric(names(dts[["spc"]])[!names(dts[["spc"]]) %in% 
  c("spc_id", "lcol_type", "group_id")]))
minmax <- function(x) {c(min(x), max(x))}
x_lim <- minmax(as.numeric(names(dts[["spc"]])[!names(dts[["spc"]]) %in%
  c("spc_id", "lcol_type", "group_id")]))

# Extract VIP (variable importance in projection) scores and
# PLS regression coefficients
df_vip_pls <- extract_multi_pls_vip_coef(mout_list = mout_list) %>%
  # rename(wavelength = wavenumber) %>%
  mutate(coef_bigger0 = as.integer(coef > 0)) %>% 
  mutate(coef_bigger0 = coef_bigger0 / 10) %>% 
  mutate(coef_bigger0 = coef_bigger0 - 0.05)

# Determine highlighted regions above VIP = 1
rects <- create_vip_rects(df_vip_pls[df_vip_pls$model == "PLSR starch", ])

# Plot mean replicate spectra --------------------------------------------------

p_spc <- ggplot(dts_long[["spc"]], 
    aes(x = wavelength, y = value)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(dts_long[["spc"]]$value),
      ymax = max(dts_long[["spc"]]$value), group = group), 
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(data = dts_long[["spc"]], inherit.aes = FALSE,
    aes(x = wavelength, y = value, group = group_id),
      alpha = alpha, size = 0.2) +
  # geom_vline(data = df_peak, aes(xintercept = wavenumber), color = "#e41a1c",
  #   linetype = "dotted", size = 0.4, alpha = 0.6) +
  # geom_text_repel(data = df_peak, aes(x = wavenumber, y = y_peaks, 
  #   label = annotation_const), size = 2, angle = 90, col = "#e41a1c", 
  #   fontface = "bold", arrow = arrow(length = unit(0.02, "npc")),
  #   direction = "both", fill = "white", nudge_y = 0.45) +
  scale_x_continuous(limits = x_lim, breaks = brk) +
  ylim(c(0, 0.55)) +
  labs(x = xlab, y = ylab1) +
  theme_bw() +
  theme(
    plot.margin = unit(c(1, 5, -30, 6),
    units = "points"), axis.text.x = element_blank())

p_spc_pre <- ggplot(dts_long[["spc_pre"]],
    aes(wavelength, value)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start,
      xmax = end, ymin = min(dts_long[["spc_pre"]]$value),
      ymax = max(dts_long[["spc_pre"]]$value), group = group),
      color = "transparent",
      fill = "orange", alpha = 0.3) +
  geom_line(aes(group = group_id),
    alpha = alpha, size = 0.2) +
  labs(x = xlab, y = ylab2) +
  theme_bw() +
  theme(
    plot.margin = unit(c(0, 5, 1, 1),
    units = "points")) +
  scale_x_continuous(limits = x_lim, breaks = brk) +
  theme(
    plot.margin = unit(c(1, 5, -30, 6),
    units = "points"),
    axis.title.y = element_text(vjust = 0.25),
    axis.text.x = element_blank())

# Plot VIP ---------------------------------------------------------------------

p_vip <- ggplot(data = df_vip_pls,
    aes(x = wavenumber, y = vip)) +
  geom_rect(data = rects, inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = min(df_vip_pls$vip),
    ymax = max(df_vip_pls$vip), group = group), color = "transparent",
    fill = "orange", alpha = 0.3) +
  geom_hline(yintercept = 1, colour = "black") +
  geom_line(aes(colour = model), size = 0.55) +
  geom_point(aes(x = wavenumber, y = coef_bigger0),
    alpha = 1/5, size = 0.7, colour = "black", shape = 124) +
  xlab(xlab) +
  ylab("VIP") +
  scale_x_continuous(limits = x_lim, breaks = brk) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 5, 1, 1),
    units = "points"), axis.title.y = element_text(vjust = 0.25),
    legend.position = "bottom") +
  guides(colour = guide_legend(title = "Model/outcome"))

# Arrange plots in panels without margins --------------------------------------
# No margins in between

p_vip_comb <- cowplot::plot_grid(
  p_spc, p_spc_pre, p_vip, rel_heights = c(0.35, 0.3, 0.6),
  ncol = 1, align = "v")

ggsave(filename = "spc-starch-pls-vip.pdf", plot = p_vip_comb, 
  path = "out/figs", width = 6.5, height = 5)
