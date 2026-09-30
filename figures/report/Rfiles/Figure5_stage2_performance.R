# Run from the repository root: Rscript figures/report/Rfiles/Figure5_stage2_performance.R
# =============================================================================
# Figure 5 — Stage 2 mechanism classification performance (two panels)
#
#   (A) Per-class precision / recall / F1 for the seven mechanism classes
#   (B) 7x7 confusion matrix on the independent test set (n = 200)
#
# Replaces the previously separate figure_table7.png (panel A, R/ggplot) and
# reports/stage2_confusion_matrix.png (panel B, Python/seaborn). Rebuilding
# panel B in ggplot puts both panels in the same visual language as Figure 4
# and resolves the "merge Figure 6 with 5" review comment.
#
# NOTE: no model is re-run. Every value below is a fixed constant read off the
# Stage 2 test-set confusion matrix.
#
# Output: figures/report/Figure5_stage2_performance.png (300 dpi)
# =============================================================================

library(ggplot2)
library(patchwork)

# --- Colour palette (matches figure_table7.R / figure_stage1.R) ---------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Tag styling shared by both panels ---------------------------------------
tag_style <- theme(
  plot.tag = element_text(size = 16, face = "bold", colour = col_ink)
)

# =============================================================================
# Panel A — per-class metrics
# =============================================================================
# Precision for Autoubiquitination is 85.0, not 85.9. 17 / (17 + 3) = 85.0.
# Confirmed by this panel's own F1 (91.9 requires 85.0; 85.9 gives 92.4) and by
# the macro-precision of 94.6 reported in Figure 4D.

make_panel <- function(mech, n, prec, rec, f1, show_x_title = FALSE) {

  df <- data.frame(
    Metric = factor(c("Precision", "Recall", "F1"),
                    levels = c("Precision", "Recall", "F1")),
    Score  = c(prec, rec, f1),
    label  = sprintf("%.1f", c(prec, rec, f1))
  )

  ggplot(df, aes(x = .data$Score, y = .data$Metric)) +

    geom_segment(
      aes(xend = .data$Score),
      x = 84, colour = col_accent, linewidth = 1.0, alpha = 0.55
    ) +

    geom_point(size = 3.5, colour = col_accent, alpha = 0.95) +

    geom_text(
      aes(x = .data$Score + 1.5, label = .data$label),
      hjust = 0, size = 2.9, colour = col_ink
    ) +

    scale_x_continuous(
      limits = c(84, 110),
      breaks = c(85, 90, 95, 100),
      expand = expansion(mult = c(0, 0))
    ) +

    scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +

    labs(
      x        = if (show_x_title) "Score (%)" else NULL,
      y        = NULL,
      title    = mech,
      subtitle = paste0("n = ", n)
    ) +

    theme_classic(base_size = 11) +
    theme(
      plot.title       = element_text(
        colour = col_ink, face = "bold", size = 9,
        hjust = 0.5, margin = margin(b = 0)
      ),
      plot.subtitle    = element_text(
        colour = col_muted, size = 8,
        hjust = 0.5, margin = margin(b = 4)
      ),
      plot.background  = element_rect(fill = col_bg, colour = NA),
      panel.background = element_rect(fill = col_bg, colour = NA),
      axis.text.y      = element_text(colour = col_ink,   size = 9.5),
      axis.text.x      = element_text(colour = col_muted, size = 8),
      axis.title.x     = element_text(
        colour = col_ink, size = 9.5, margin = margin(t = 5)
      ),
      axis.ticks.y     = element_blank(),
      axis.line.y      = element_blank(),
      axis.line.x      = element_line(colour = "#cccccc", linewidth = 0.4),
      axis.ticks.x     = element_line(colour = "#cccccc", linewidth = 0.4),
      plot.margin      = margin(6, 12, 6, 6)
    )
}

# Laid out 2 columns x 4 rows (see composition block below), so the x-axis
# title belongs on the bottom plot of each column: p6 (col 2) and p7 (col 1).
p1 <- make_panel("Autophosphorylation", 107,  99.0,  92.5,  95.6)
p2 <- make_panel("Autoregulation",       24,  92.3, 100.0,  96.0)
p3 <- make_panel("Autocatalytic",        22,  91.7, 100.0,  95.6)
p4 <- make_panel("Autoinhibition",       18,  94.4,  94.4,  94.4)
p5 <- make_panel("Autoubiquitination",   17,  85.0, 100.0,  91.9)
p6 <- make_panel("Autolysis",             6, 100.0, 100.0, 100.0, show_x_title = TRUE)
p7 <- make_panel("Autoinducer",           6, 100.0, 100.0, 100.0, show_x_title = TRUE)

# Tag "A" sits on the first panel of the block
p1 <- p1 + labs(tag = "A") + tag_style

# =============================================================================
# Panel B — 7x7 confusion matrix
# =============================================================================
# Rows = true class, columns = predicted class. Row sums give the supports
# used in panel A (107, 24, 22, 18, 17, 6, 6); total = 200, correct = 191.

classes <- c(
  "autophosphorylation", "autoregulation", "autocatalytic",
  "autoinhibition", "autoubiquitination", "autolysis", "autoinducer"
)

counts <- c(
  99, 2, 2, 1, 3, 0, 0,   # true autophosphorylation
   0, 24, 0, 0, 0, 0, 0,  # true autoregulation
   0, 0, 22, 0, 0, 0, 0,  # true autocatalytic
   1, 0, 0, 17, 0, 0, 0,  # true autoinhibition
   0, 0, 0, 0, 17, 0, 0,  # true autoubiquitination
   0, 0, 0, 0, 0, 6, 0,   # true autolysis
   0, 0, 0, 0, 0, 0, 6    # true autoinducer
)

cm <- data.frame(
  True      = factor(rep(classes, each = 7), levels = rev(classes)),
  Predicted = factor(rep(classes, times = 7), levels = classes),
  n         = counts
)

panel_b <- ggplot(cm, aes(x = .data$Predicted, y = .data$True, fill = .data$n)) +

  geom_tile(colour = "white", linewidth = 1.2) +

  geom_text(
    aes(label = .data$n),
    size = 3.6, fontface = "bold",
    colour = ifelse(cm$n == 0, "#c2ccd4",
             ifelse(cm$n > 50, "white", col_ink))
  ) +

  # sqrt scaling keeps mid-range counts (17-24) visible against the 99 diagonal
  scale_fill_gradient(
    low   = "#fdf1ea",
    high  = col_accent,
    trans = "sqrt",
    guide = "none"
  ) +

  scale_x_discrete(position = "bottom", expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0)) +

  coord_fixed() +

  labs(x = "Predicted", y = "True", tag = "B") +

  theme_classic(base_size = 12) +
  theme(
    plot.tag         = element_text(size = 16, face = "bold", colour = col_ink),
    axis.title.x     = element_text(
      colour = col_ink, size = 11, face = "bold", margin = margin(t = 8)
    ),
    axis.title.y     = element_text(
      colour = col_ink, size = 11, face = "bold", margin = margin(r = 8)
    ),
    axis.text.x      = element_text(
      colour = col_ink, size = 9, angle = 45, hjust = 1
    ),
    axis.text.y      = element_text(colour = col_ink, size = 9),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    plot.margin      = margin(14, 10, 10, 10)
  )

# =============================================================================
# Compose: panel A as 2 columns x 4 rows, panel B centred beneath
# =============================================================================
# Portrait on purpose. The figure is page-height limited, so two wide columns
# print roughly twice as large as four narrow ones once scaled to a journal
# page; the 9 x 12.5 canvas is ~the 6.5 x 9 in text-block aspect ratio, so it
# fills the page with minimal down-scaling.
#
#   col 1 | col 2
#   ------+------
#     p1  |  p2
#     p3  |  p4
#     p5  |  p6
#     p7  | (empty)

row1 <- p1 | p2
row2 <- p3 | p4
row3 <- p5 | p6
row4 <- p7 | plot_spacer()

# panel_b spans the full width rather than being centred with spacers: coord_fixed
# keeps the matrix square within whatever space it gets, and running full-width
# keeps the "B" tag on the same left margin as the "A" tag above.
combined <- row1 / row2 / row3 / row4 / panel_b +
  plot_layout(heights = c(1, 1, 1, 1, 2.8))

# --- Save --------------------------------------------------------------------
output_path <- "figures/report/Figure5_stage2_performance.png"

ggsave(
  filename = output_path,
  plot     = combined,
  width    = 9,
  height   = 12.5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)
