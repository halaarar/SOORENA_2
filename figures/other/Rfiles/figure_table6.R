# Run from the repository root: Rscript figures/other/Rfiles/figure_table6.R
# =============================================================================
# Figure: Table 6 — Overall Stage 2 test-set classification performance
# Description: Lollipop chart showing all 7 metrics (Accuracy, Macro Precision,
#              Macro Recall, Macro F1, Weighted Precision, Weighted Recall,
#              Weighted F1), sorted ascending, single colour, no legend.
# Output: figures/other/figure_table6.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)

# --- Data — all 7 metrics from Table 6 ---------------------------------------
table6 <- data.frame(
  Metric = c(
    "Accuracy",
    "Macro Precision", "Macro Recall", "Macro F1",
    "Weighted Precision", "Weighted Recall", "Weighted F1"
  ),
  Score = c(95.5, 94.6, 98.1, 96.2, 95.9, 95.5, 95.5)
) %>%
  arrange(Score) %>%
  mutate(
    Metric = factor(Metric, levels = Metric),
    label  = sprintf("%.1f%%", Score)
  )

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Plot --------------------------------------------------------------------
p <- ggplot(table6, aes(x = Score, y = Metric)) +

  # Stem
  geom_segment(
    aes(x = 93, xend = Score, y = Metric, yend = Metric),
    colour = col_accent, linewidth = 1.1, alpha = 0.55
  ) +

  # Dot
  geom_point(size = 4.5, colour = col_accent, alpha = 0.95) +

  # Label — offset so numbers clear the dot
  geom_text(
    aes(x = Score + 0.3, label = label),
    hjust = 0, size = 3.4, colour = col_ink
  ) +

  labs(x = "Score (%)", y = NULL) +

  scale_x_continuous(
    limits = c(93, 102),
    breaks = c(93, 94, 95, 96, 97, 98, 99, 100),
    expand = expansion(mult = c(0, 0))
  ) +

  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +

  theme_classic(base_size = 12) +
  theme(
    axis.title.x     = element_text(
      colour = col_ink, size = 11, margin = margin(t = 8)
    ),
    axis.text.y      = element_text(colour = col_ink,   size = 11),
    axis.text.x      = element_text(colour = col_muted, size = 10),
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.line.x      = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks.x     = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    legend.position  = "none",
    plot.margin      = margin(15, 20, 15, 15)
  )

# --- Save --------------------------------------------------------------------
output_path <- "figures/other/figure_table6.png"

ggsave(
  filename = output_path,
  plot     = p,
  width    = 7.5,
  height   = 5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)