# Run from the repository root: Rscript figures/other/Rfiles/figure_table7.R
# =============================================================================
# Figure: Table 7 — Per-class Stage 2 test-set classification performance
# Description: 7-panel dot plot (patchwork 3×3), one panel per mechanism,
#              metrics on y-axis, score on x-axis.
# Output: figures/other/figure_table7.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# --- Data --------------------------------------------------------------------
table7 <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  Support   = c(107, 24, 22, 18, 17, 6, 6),
  Precision = c(99.0, 92.3, 91.7, 94.4,  85.0, 100.0, 100.0),
  Recall    = c(92.5, 100.0, 100.0, 94.4, 100.0, 100.0, 100.0),
  F1        = c(95.6, 96.0, 95.6, 94.4,  91.9, 100.0, 100.0)
) %>%
  arrange(desc(Support))

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Helper: one panel per mechanism -----------------------------------------
make_panel <- function(mech, n, prec, rec, f1, show_x_title = FALSE) {

  df <- data.frame(
    Metric = factor(c("Precision", "Recall", "F1"),
                    levels = c("Precision", "Recall", "F1")),
    Score  = c(prec, rec, f1),
    label  = sprintf("%.1f", c(prec, rec, f1))
  )

  ggplot(df, aes(x = .data$Score, y = .data$Metric)) +

    # Stem
    geom_segment(
      aes(xend = .data$Score),
      x = 84, colour = col_accent, linewidth = 1.0, alpha = 0.55
    ) +

    # Dot
    geom_point(size = 3.5, colour = col_accent, alpha = 0.95) +

    # Label — 1.5-unit gap clears the dot cleanly
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

# --- Build 7 panels ----------------------------------------------------------
# Rows 1–2: no x-axis title (avoid repetition); Row 3 / bottom: show it
p1 <- make_panel("Autophosphorylation", 107,  99.0,  92.5, 95.6)
p2 <- make_panel("Autoregulation",       24,  92.3, 100.0, 96.0)
p3 <- make_panel("Autocatalytic",         22,  91.7, 100.0, 95.6)
p4 <- make_panel("Autoinhibition",        18,  94.4,  94.4, 94.4)
p5 <- make_panel("Autoubiquitination",    17,  85.0, 100.0, 91.9)
p6 <- make_panel("Autolysis",              6, 100.0, 100.0, 100.0)
p7 <- make_panel("Autoinducer",            6, 100.0, 100.0, 100.0,
                 show_x_title = TRUE)

# --- Combine: 3×3 grid, p7 centred in the last row --------------------------
row1 <- p1 | p2 | p3
row2 <- p4 | p5 | p6
row3 <- plot_spacer() | p7 | plot_spacer()

combined <- row1 / row2 / row3

# --- Save --------------------------------------------------------------------
output_path <- "figures/other/figure_table7.png"

ggsave(
  filename = output_path,
  plot     = combined,
  width    = 9,
  height   = 7.5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)