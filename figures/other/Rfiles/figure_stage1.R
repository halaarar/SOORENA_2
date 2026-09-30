# Run from the repository root: Rscript figures/other/Rfiles/figure_stage1.R
# =============================================================================
# Figure: Stage 1 — Two-panel figure
# Panel A: Test performance metrics (Table 5) as a dot plot
# Panel B: Confusion matrix with enlarged fonts, compact layout
# Output: figures/other/figure_stage1.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# =============================================================================
# Panel A: Performance metrics dot plot
# =============================================================================

metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Score  = c(96.0, 97.8, 90.0, 93.8)
) %>%
  arrange(Score) %>%
  mutate(
    Metric = factor(Metric, levels = Metric),
    label  = sprintf("%.1f%%", Score)
  )

panel_a <- ggplot(metrics, aes(x = Score, y = Metric)) +

  # Stem
  geom_segment(
    aes(x = 80, xend = Score, y = Metric, yend = Metric),
    colour = col_accent, linewidth = 1.2, alpha = 0.60
  ) +

  # Dot
  geom_point(size = 5, colour = col_accent, alpha = 0.90) +

  # Label
  geom_text(
    aes(x = Score + 0.7, label = label),
    hjust = 0, size = 3.6, colour = col_ink
  ) +

  labs(x = "Score (%)", y = NULL) +

  scale_x_continuous(
    limits = c(80, 102),
    breaks = c(80, 85, 90, 95, 100),
    expand = expansion(mult = c(0, 0))
  ) +

  scale_y_discrete(expand = expansion(add = c(0.7, 0.7))) +

  theme_classic(base_size = 12) +
  theme(
    axis.title.x     = element_text(
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
    plot.margin      = margin(10, 15, 10, 10)
  )

# =============================================================================
# Panel B: Confusion matrix
# =============================================================================

cm <- data.frame(
  True      = factor(
    c("No Mechanism", "No Mechanism", "Has Mechanism", "Has Mechanism"),
    levels = c("Has Mechanism", "No Mechanism")
  ),
  Predicted = factor(
    c("No Mechanism", "Has Mechanism", "No Mechanism", "Has Mechanism"),
    levels = c("No Mechanism", "Has Mechanism")
  ),
  n         = c(396, 4, 20, 180),
  correct   = c(TRUE, FALSE, FALSE, TRUE)
)

panel_b <- ggplot(cm, aes(x = Predicted, y = True, fill = n)) +

  geom_tile(colour = "white", linewidth = 1.2) +

  geom_text(
    aes(label = n),
    size = 7, fontface = "bold",
    colour = ifelse(cm$n > 150, "white", col_ink)
  ) +

  scale_fill_gradient(
    low  = "#fef5f0",
    high = col_accent,
    guide = "none"
  ) +

  labs(x = "Predicted", y = "True") +

  theme_classic(base_size = 12) +
  theme(
    axis.title.x     = element_text(
      colour = col_ink, size = 12, face = "bold", margin = margin(t = 8)
    ),
    axis.title.y     = element_text(
      colour = col_ink, size = 12, face = "bold", margin = margin(r = 8)
    ),
    axis.text        = element_text(colour = col_ink, size = 11),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )

# =============================================================================
# Combine panels with patchwork
# =============================================================================

tag_theme <- theme(
  plot.tag        = element_text(size = 13, face = "bold", colour = col_ink),
  plot.background = element_rect(fill = col_bg, colour = NA)
)

combined <- (panel_a + tag_theme) + (panel_b + tag_theme) +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(tag_levels = "A")

# --- Save --------------------------------------------------------------------
output_path <- "figures/other/figure_stage1.png"

ggsave(
  filename = output_path,
  plot     = combined,
  width    = 10,
  height   = 4,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)