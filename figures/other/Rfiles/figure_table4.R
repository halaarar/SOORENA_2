# Run from the repository root: Rscript figures/other/Rfiles/figure_table4.R
# =============================================================================
# Figure: Table 4 — Class weights used for Stage 2 multi-class training
# Description: Dot plot showing inverse-frequency class weights per mechanism.
# Output: figures/other/figure_table4.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)

# --- Data --------------------------------------------------------------------
table4 <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  weight = c(0.27, 1.21, 1.29, 1.57, 1.60, 4.93, 4.93)
)

# Sort ascending by weight (bottom = most frequent / lowest weight)
table4 <- table4 %>%
  arrange(weight, Mechanism) %>%
  mutate(
    Mechanism = factor(Mechanism, levels = Mechanism),
    label     = sprintf("%.2f", weight)
  )

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Plot --------------------------------------------------------------------
p <- ggplot(table4, aes(x = weight, y = Mechanism)) +

  # Reference line at weight = 1 (neutral / balanced)
  geom_vline(
    xintercept = 1, linetype = "dashed",
    colour = col_muted, linewidth = 0.4, alpha = 0.7
  ) +

  # Stem
  geom_segment(
    aes(x = 0, xend = weight, y = Mechanism, yend = Mechanism),
    colour = col_accent, linewidth = 1.0, alpha = 0.60
  ) +

  # Dot
  geom_point(size = 4, colour = col_accent, alpha = 0.90) +

  # Label to the right
  geom_text(
    aes(x = weight + 0.15, label = label),
    hjust = 0, size = 3.4, colour = col_ink
  ) +

  # Annotation explaining the reference line
  annotate(
    "text", x = 1.04, y = 0.55, label = "weight = 1",
    hjust = 0, size = 2.8, colour = col_muted, fontface = "italic"
  ) +

  labs(x = "Class Weight", y = NULL) +

  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.18)),
    breaks = c(0, 1, 2, 3, 4, 5)
  ) +

  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +

  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(
      colour = col_ink, size = 11, margin = margin(t = 8)
    ),
    axis.text.y      = element_text(colour = col_ink,  size = 11),
    axis.text.x      = element_text(colour = col_muted, size = 10),
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.line.x      = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks.x     = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    plot.margin      = margin(15, 25, 15, 15)
  )

# --- Save --------------------------------------------------------------------
output_path <- "figures/other/figure_table4.png"

ggsave(
  filename = output_path,
  plot     = p,
  width    = 7,
  height   = 4,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)