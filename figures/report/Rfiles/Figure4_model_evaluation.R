# Run from the repository root: Rscript figures/report/Rfiles/Figure4_model_evaluation.R
# =============================================================================
# Combined Figure: Panels from Fig 4, 5, and 6
# Panel A: Stage 1 test-set performance metrics  (originally figure_stage1 Panel A)
# Panel B: Stage 1 confusion matrix              (originally figure_stage1 Panel B)
# Panel C: Stage 2 class weights                 (originally figure_table4)
# Panel D: Stage 2 overall test-set metrics      (originally figure_table6)
# Layout:  2 × 2 patchwork, tagged A–D
# Output:  figures/report/Figure4_model_evaluation.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# --- Shared palette -----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

tag_theme <- theme(
  plot.tag        = element_text(size = 13, face = "bold", colour = col_ink),
  plot.background = element_rect(fill = col_bg, colour = NA)
)

base_theme <- theme_classic(base_size = 12) +
  theme(
    axis.text.y      = element_text(colour = col_ink,   size = 10),
    axis.text.x      = element_text(colour = col_muted, size = 9),
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.line.x      = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks.x     = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    plot.margin      = margin(12, 18, 12, 12)
  )

# =============================================================================
# Panel A: Stage 1 test-set performance metrics
# =============================================================================

stage1_metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Score  = c(96.0, 97.8, 90.0, 93.8)
) %>%
  arrange(Score) %>%
  mutate(
    Metric = factor(Metric, levels = Metric),
    label  = sprintf("%.1f%%", Score)
  )

panel_a <- ggplot(stage1_metrics, aes(x = Score, y = Metric)) +
  geom_segment(
    aes(x = 80, xend = Score, y = Metric, yend = Metric),
    colour = col_accent, linewidth = 1.1, alpha = 0.60
  ) +
  geom_point(size = 4, colour = col_accent, alpha = 0.90) +
  geom_text(
    aes(x = Score + 0.7, label = label),
    hjust = 0, size = 3.2, colour = col_ink
  ) +
  labs(x = "Score (%)", y = NULL) +
  scale_x_continuous(
    limits = c(80, 103),
    breaks = c(80, 85, 90, 95, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.7, 0.7))) +
  base_theme +
  theme(axis.title.x = element_text(colour = col_ink, size = 10, margin = margin(t = 7)))

# =============================================================================
# Panel B: Stage 1 confusion matrix
# =============================================================================

cm <- data.frame(
  True = factor(
    c("No Mechanism", "No Mechanism", "Has Mechanism", "Has Mechanism"),
    levels = c("Has Mechanism", "No Mechanism")
  ),
  Predicted = factor(
    c("No Mechanism", "Has Mechanism", "No Mechanism", "Has Mechanism"),
    levels = c("No Mechanism", "Has Mechanism")
  ),
  n       = c(396, 4, 20, 180),
  correct = c(TRUE, FALSE, FALSE, TRUE)
)

panel_b <- ggplot(cm, aes(x = Predicted, y = True, fill = n)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(
    aes(label = n),
    size = 6.5, fontface = "bold",
    colour = ifelse(cm$n > 150, "white", col_ink)
  ) +
  scale_fill_gradient(low = "#fef5f0", high = col_accent, guide = "none") +
  labs(x = "Predicted", y = "True") +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x     = element_text(colour = col_ink, size = 11, face = "bold", margin = margin(t = 8)),
    axis.title.y     = element_text(colour = col_ink, size = 11, face = "bold", margin = margin(r = 8)),
    axis.text        = element_text(colour = col_ink, size = 10),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    plot.margin      = margin(12, 12, 12, 12)
  )

# =============================================================================
# Panel C: Stage 2 class weights
# =============================================================================

weights <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  weight = c(0.27, 1.21, 1.29, 1.57, 1.60, 4.93, 4.93)
) %>%
  arrange(weight, Mechanism) %>%
  mutate(
    Mechanism = factor(Mechanism, levels = Mechanism),
    label     = sprintf("%.2f", weight)
  )

panel_c <- ggplot(weights, aes(x = weight, y = Mechanism)) +
  geom_vline(
    xintercept = 1, linetype = "dashed",
    colour = col_muted, linewidth = 0.4, alpha = 0.7
  ) +
  geom_segment(
    aes(x = 0, xend = weight, y = Mechanism, yend = Mechanism),
    colour = col_accent, linewidth = 1.0, alpha = 0.60
  ) +
  geom_point(size = 3.5, colour = col_accent, alpha = 0.90) +
  geom_text(
    aes(x = weight + 0.15, label = label),
    hjust = 0, size = 3.2, colour = col_ink
  ) +
  annotate(
    "text", x = 1.04, y = 0.55, label = "weight = 1",
    hjust = 0, size = 2.6, colour = col_muted, fontface = "italic"
  ) +
  labs(x = "Class Weight", y = NULL) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.20)),
    breaks = c(0, 1, 2, 3, 4, 5)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  base_theme +
  theme(axis.title.x = element_text(colour = col_ink, size = 10, margin = margin(t = 7)))

# =============================================================================
# Panel D: Stage 2 overall test-set metrics
# =============================================================================

stage2_metrics <- data.frame(
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

panel_d <- ggplot(stage2_metrics, aes(x = Score, y = Metric)) +
  geom_segment(
    aes(x = 93, xend = Score, y = Metric, yend = Metric),
    colour = col_accent, linewidth = 1.1, alpha = 0.55
  ) +
  geom_point(size = 4, colour = col_accent, alpha = 0.95) +
  geom_text(
    aes(x = Score + 0.3, label = label),
    hjust = 0, size = 3.2, colour = col_ink
  ) +
  labs(x = "Score (%)", y = NULL) +
  scale_x_continuous(
    limits = c(93, 102),
    breaks = c(93, 94, 95, 96, 97, 98, 99, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  base_theme +
  theme(
    legend.position  = "none",
    axis.title.x     = element_text(colour = col_ink, size = 10, margin = margin(t = 7))
  )

# =============================================================================
# Combine: 2 × 2 patchwork
# =============================================================================

combined <- (
  (panel_a + tag_theme) | (panel_b + tag_theme)
) / (
  (panel_c + tag_theme) | (panel_d + tag_theme)
) +
  plot_layout(widths = c(1.2, 0.9)) +
  plot_annotation(tag_levels = "A")

# --- Save --------------------------------------------------------------------
output_path <- "figures/report/Figure4_model_evaluation.png"

ggsave(
  filename = output_path,
  plot     = combined,
  width    = 11,
  height   = 8,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)