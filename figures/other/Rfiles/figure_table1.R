# Run from the repository root: Rscript figures/other/Rfiles/figure_table1.R
# =============================================================================
# Figure: Table 1 — Final distribution of autoregulatory mechanisms
# Description: Lollipop chart — stem length encodes count (n), dot size
#              encodes percentage (%), label shows both. Publication quality.
# Output: figures/other/figure_table1.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)

# --- Data --------------------------------------------------------------------
table1 <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  n   = c(711, 158, 147, 122, 118, 38, 38),
  pct = c(53.4, 11.9, 11.0, 9.2, 8.9, 2.9, 2.9)
)

# Sort ascending by n, then alphabetically for ties (bottom = least frequent)
table1 <- table1 %>%
  arrange(n, Mechanism) %>%
  mutate(
    Mechanism = factor(Mechanism, levels = Mechanism),
    label     = paste0(n, " (", sprintf("%.1f", pct), "%)")
  )

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"  # orange — stems and dots
col_ink    <- "#1a2332"  # navy   — text
col_muted  <- "#6b7a89"  # grey   — axis text
col_bg     <- "#ffffff"  # white  — background

# --- Plot --------------------------------------------------------------------
p <- ggplot(table1, aes(x = n, y = Mechanism)) +

  # Stem — thin, slightly transparent
  geom_segment(
    aes(x = 0, xend = n, y = Mechanism, yend = Mechanism),
    colour = col_accent, linewidth = 1.0, alpha = 0.60
  ) +

  # Dot — size proportional to percentage
  geom_point(
    aes(size = pct),
    colour = col_accent, alpha = 0.90
  ) +

  # Label to the right: fixed x offset for consistent spacing across dot sizes
  geom_text(
    aes(x = n + 30, label = label),
    hjust = 0, size = 3.4, colour = col_ink
  ) +

  # Size scale for dot (percentage)
  scale_size_continuous(
    name   = "Percentage (%)",
    range  = c(2, 6.5),
    breaks = c(2.9, 9.2, 53.4),
    labels = c("2.9%", "9.2%", "53.4%")
  ) +

  # Axis — no title on y, x is count
  labs(x = "Number of Publications", y = NULL) +

  # Expand right for labels, no left padding past 0
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.28)),
    breaks = seq(0, 700, 100)
  ) +

  # Increase vertical spacing between categories
  scale_y_discrete(expand = expansion(add = c(0.8, 0.8))) +

  # Clean manuscript theme — white background, no gridlines
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(
      colour = col_ink, size = 11, margin = margin(t = 8)
    ),
    axis.text.y  = element_text(colour = col_ink,  size = 11),
    axis.text.x  = element_text(colour = col_muted, size = 10),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x  = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks.x = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    legend.position  = "none",
    legend.title     = element_text(
      colour = col_ink, size = 9, face = "bold"
    ),
    legend.text      = element_text(colour = col_muted, size = 9),
    legend.key       = element_rect(fill = NA, colour = NA),
    plot.margin      = margin(15, 20, 15, 15)
  )

# --- Save --------------------------------------------------------------------
output_path <- "figures/other/figure_table1.png"

ggsave(
  filename = output_path,
  plot     = p,
  width    = 8,
  height   = 4.5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)