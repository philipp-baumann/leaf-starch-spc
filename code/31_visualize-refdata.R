################################################################################
## Project:
## Description:
################################################################################

# Wilcoxon-sign-rank test ======================================================

# harvest times ----------------------------------------------------------------

starch_train <- spc_train_groupids %>%
  group_by(sample_age_rep) %>%
  slice(1L) %>%
  select(-c(metadata, wavelengths, spc, part_id, wavelengths_rs, spc_rs, 
    wavenumbers_rs, spc_mean, spc_pre, xvalues_pre))

wilcox_starch_harvest <- pairwise.wilcox.test(
  x = starch_train$starch,
  g = starch_train$harvest_time,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# leaf age ---------------------------------------------------------------------

wilcox_starch_age <- pairwise.wilcox.test(
  x = starch_train$starch,
  g = starch_train$leaf_age,
  paired = FALSE,
  p.adjust.method = "bonferroni")

# genotypes --------------------------------------------------------------------

wilcox_starch_genotype <- pairwise.wilcox.test(
  x = starch_train$starch,
  g = starch_train$genotype,
  paired = FALSE,
  p.adjust.method = "bonferroni")

## Boxplot separated for sets and harvest times ================================

# loadd(spc_test_predict)

# grouped according to set
test_train_order <- c("Training", "Test")

starch_train_test <- 
  bind_rows(
    starch_train %>%
      select(sample_id, genotype, harvest_time, starch) %>%
      mutate(set = "Training"), 
    spc_test_predict %>%
      select(sample_id, sample_rep, harvest_time, starch) %>%
      mutate(
        set = "Test",
        genotype = map_chr(sample_rep,
          ~ stringr::str_replace(.x, pattern = "_[[:digit:]]+", 
          replacement = "")
        )
      ) %>% 
      select(-sample_rep)
  ) %>%
  mutate(
    set = factor(set, levels = test_train_order, order = TRUE)
  )

p_bp_starch <- 
  ggplot(starch_train_test, aes(x = harvest_time, y = starch, fill = set)) + 
  geom_boxplot() +
  facet_wrap(~ set) +
  xlab("Harvest time")+
  ylab(expression(plain(Starch) ~~ group("[",mg ~~ g^{-1} ~ DW,"]"))) +
  scale_fill_manual(values = c("#d7191c", "#2b83ba")) +
  annotate("text", x = 1, y =-10, label = "a", size = 4) +
  annotate("text", x = 2, y = -10, label = "b", size = 4) +
  theme_bw(base_size =12, base_rect_size =1) +
  theme(
    strip.text.x = element_text(size = 12),
    axis.text=element_text(size=10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background =element_rect(colour="black", fill = NA)
  )

p_bp_starch_pdf <- ggsave(
  filename = "boxplot-starch-sets.pdf",
  plot = p_bp_starch, path = here("out", "figs"),
  width = 3.34, height = 2.8)

p_bp_starch_pub <- ggsave(
  filename = "Fig2.pdf",
  plot = p_bp_starch, path = here("pub", "figs"),
  width = 3.34, height = 2.8)

## Boxplot leaf age ============================================================

# Read entire reference starch data set including whole plants -----------------

# Reorder groups
group_order <- c("y", "m", "o", "wp")

starch_train_whole <- readr::read_csv(
  file = here("data", "training", "reference",
    "reference-starch-incl-whole.csv")) %>%
  rename(
    harvest_time = ht,
    leaf_age = age
  ) %>%
  mutate(
    leaf_age = factor(leaf_age, levels = group_order, order = TRUE)
  ) %>%
  filter(starch < 200) # remove outlier

p_bp_starch_leafage_ed <-
  starch_train_whole %>%
  filter(harvest_time == "ED") %>%
  ggplot(data = ., aes(x = leaf_age, y = starch)) + 
  geom_boxplot(fill = "#d7191c", colour ="black") +
  xlab("Leaf fraction") +
  ylab(expression(plain(Starch) ~~ group("[",mg ~~ g^{-1} ~ DW,"]"))) +
  scale_x_discrete(
    breaks = c("y", "m", "o", "wp"),
    labels = c("young", "mature", "old", "whole plant")) +
  theme_bw(base_size = 12, base_rect_size = 1)+ # But change axis line
  theme(
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = "#d7191c")
  ) +
  annotate("text", x = 1, y = 70, label = "a", size = 4) +
  annotate("text", x = 2, y = 102, label = "b", size = 4) +
  annotate("text", x = 3, y = 102, label = "b", size = 4) +
  annotate("text", x = 4, y = 75, label = "b", size = 4)

p_bp_starch_leafage_ed_pdf <- ggsave(
  filename = "boxplot-starch-leaf-age.pdf",
  plot = p_bp_starch_leafage_ed, path = here("out", "figs"),
  width = 3.34, height = 2.8)

p_bp_starch_leafage_ed_pub_pdf <- ggsave(
  filename = "Fig4.pdf",
  plot = p_bp_starch_leafage_ed, path = here("pub", "figs"),
  width = 3.34, height = 2.8)


## Boxplot genotypes ===========================================================

genotype_label <- tribble(
  ~genotype, ~y_offset, ~text,
  "GH129",   84,        "a",
  "LE1408",  67,        "ab",
  "LE1421",  67,        "ab",
  "LE1436",  75,        "ab",
  "LE1619",  120,       "ab",
  "LE2622",  87,        "a",
  "LE2692",  73,        "ab",
  "LE2768",  77,        "a",
  "MR20",    100,       "ab",
  "MR28",    62,        "b",
  "MR3",     90,        "ab",
  "TP0345",  98,        "a"
  ) %>%
  mutate(
    genotype = as.factor(genotype)
  )

p_bp_ed_genotype <-
  starch_train_whole %>%
  filter(harvest_time == "ED") %>%
  mutate(
    genotype = as.factor(genotype)
  ) %>%
  mutate(
    genotype = fct_recode(genotype, MR20 = "MR20_3", MR3 = "MR3_3")
  ) %>%
  ggplot(data = ., aes(x = genotype, y = starch)) + 
  geom_boxplot(fill = "#d7191c", colour = "black") +
  xlab("Genotype")+
  ylab(expression(plain(Starch) ~~ group("[", mg ~~ g^{-1} ~ DW, "]")))+
  theme_bw(base_size = 10, base_rect_size = 1)+
  theme(
    axis.text = element_text(size = 8, angle = 90),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = NA)
  ) +
  geom_text(data = genotype_label, 
    aes(x = genotype, y = y_offset, label = text), inherit.aes = FALSE)
  

p_bp_starch_leafage_ed_pdf <- ggsave(
  filename = "boxplot-starch-genotype-ed.pdf",
  plot = p_bp_ed_genotype, path = here("out", "figs"),
  width = 6.69, height = 3.5)

p_bp_starch_leafage_ed_pub_pdf <- ggsave(
  filename = "Fig5.pdf",
  plot = p_bp_ed_genotype, path = here("pub", "figs"),
  width = 6.69, height = 3.5)
