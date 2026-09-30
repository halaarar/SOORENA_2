# Run from the repository root: Rscript figures/report/Rfiles/Figure3_class_distribution.R
# =============================================================================
# Figure: Table 3 — Per-class distribution across Train/Validation/Test/Total
# Description: 4-panel faceted lollipop chart (2×2 grid). Each facet shows
#              mechanism counts for one data split. Free x-scales per facet.
# Caption: Figure 2. Per-class distribution of autoregulatory mechanisms across
#          training, validation, and test subsets of the labeled dataset
#          (n = 1,332). X-axes are scaled independently per panel.
# Output: figures/report/Figure3_class_distribution.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# --- Data --------------------------------------------------------------------
table3 <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  Train      = c(497, 110, 103, 85, 83, 27, 27),
  Validation = c(107,  24,  22, 19, 18,  5,  5),
  Test       = c(107,  24,  22, 18, 17,  6,  6),
  Total      = c(711, 158, 147, 122, 118, 38, 38)
)

# Mechanism order: sorted by Train count ascending (largest sits at top)
mech_order <- table3 %>%
  arrange(Train) %>%
  pull(Mechanism)

# Reshape to long format (all 4 splits) and compute percentages
table3_long <- table3 %>%
  pivot_longer(
    cols      = c(Train, Validation, Test, Total),
    names_to  = "Split",
    values_to = "n"
  ) %>%
  group_by(Split) %>%
  mutate(
    pct   = n / sum(n) * 100,
    label = paste0(n, " (", sprintf("%.1f", pct), "%)")
  ) %>%
  ungroup() %>%
  mutate(
    Mechanism = factor(Mechanism, levels = mech_order),
    Split = factor(Split,
                   levels = c("Train", "Validation", "Test", "Total"),
                   labels = c(
                     "Train\n(n = 932)",
                     "Validation\n(n = 200)",
                     "Test\n(n = 200)",
                     "Total\n(n = 1,332)"
                   ))
  )

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Plot --------------------------------------------------------------------
p <- ggplot(table3_long, aes(x = n, y = Mechanism)) +

  # Stem
  geom_segment(
    aes(x = 0, xend = n, y = Mechanism, yend = Mechanism),
    colour = col_accent, linewidth = 0.9, alpha = 0.60
  ) +

  # Dot — smaller size reduces physical overlap with labels
  geom_point(size = 2, colour = col_accent, alpha = 0.90) +

  # Label — hjust pushes proportionally right of the dot per facet scale
  geom_text(
    aes(label = label),
    hjust = -0.25, size = 2.6, colour = col_ink
  ) +

  # 4 facets in a 2×2 grid, each with its own x-axis scale
  facet_wrap(
    ~ Split, nrow = 2, ncol = 2,
    scales = "free_x"
  ) +

  labs(x = "Number of Publications", y = NULL) +

  scale_x_continuous(expand = expansion(mult = c(0.02, 0.30))) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +

  theme_classic(base_size = 11) +
  theme(
    strip.text       = element_text(
      colour = col_ink, face = "bold", size = 10
    ),
    strip.background = element_rect(fill = "#fef5f0", colour = NA),
    axis.title.x     = element_text(
      colour = col_ink, size = 10, margin = margin(t = 6)
    ),
    axis.text.y      = element_text(colour = col_ink,  size = 9),
    axis.text.x      = element_text(colour = col_muted, size = 8),
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.line.x      = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks.x     = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    panel.spacing    = unit(1.2, "lines"),
    legend.position  = "none",
    plot.margin      = margin(15, 20, 15, 15)
  )

# --- Save --------------------------------------------------------------------
output_path <- "figures/report/Figure3_class_distribution.png"

ggsave(
  filename = output_path,
  plot     = p,
  width    = 9,
  height   = 7.5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)