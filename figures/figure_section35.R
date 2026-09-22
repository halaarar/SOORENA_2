# =============================================================================
# Figure: Section 3.5 — Comparing Source of Autoregulation Data
# Four-panel patchwork:
#   A: Source breakdown (SOORENA AI-assisted curation vs curated databases)
#   B: Publication timeline (1970–2024)
#   C: Top 10 journals
#   D: Prediction confidence — Stage 1 (Mechanism) and Stage 2 (Type), faceted
# Data: shiny_app/data/predictions.db (100,065 autoregulatory entries)
# Output: figures/figure_section35.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

# --- Shared palette -----------------------------------------------------------
col_accent  <- "#d97742"
col_curated <- "#4a6fa5"
col_ink     <- "#1a2332"
col_muted   <- "#6b7a89"
col_bg      <- "#ffffff"

tag_theme <- theme(
  plot.tag        = element_text(size = 13, face = "bold", colour = col_ink),
  plot.background = element_rect(fill = col_bg, colour = NA)
)

base_theme <- theme_classic(base_size = 11) +
  theme(
    axis.text.x      = element_text(colour = col_muted, size = 9),
    axis.line        = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks       = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA)
  )

# =============================================================================
# Panel A: Source distribution
# =============================================================================

source_df <- data.frame(
  Source = c("OmniPath", "TRRUST", "SIGNOR", "UniProt", "AI-assisted\ncuration"),
  n      = c(20, 61, 995, 1332, 97657),
  Type   = c("Curated", "Curated", "Curated", "Curated", "AI-assisted curation")
) %>%
  arrange(n) %>%
  mutate(
    Source = factor(Source, levels = Source),
    label  = formatC(n, format = "d", big.mark = ",")
  )

p_source <- ggplot(source_df, aes(x = n, y = Source, fill = Type)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = label), hjust = -0.12, size = 3.1, colour = col_ink) +
  scale_x_log10(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.3))
  ) +
  scale_fill_manual(
    values = c("AI-assisted curation" = col_accent, "Curated" = col_curated),
    labels = c("Curated" = "Curated databases", "AI-assisted curation" = "SOORENA AI-assisted curation"),
    breaks = c("Curated", "AI-assisted curation")
  ) +
  labs(x = "Number of entries (log scale)", y = NULL, fill = NULL) +
  base_theme +
  theme(
    axis.text.y     = element_text(colour = col_ink, size = 9.5),
    axis.ticks.y    = element_blank(),
    axis.line.y     = element_blank(),
    axis.title.x    = element_text(colour = col_ink, size = 10, margin = margin(t = 7)),
    legend.position = "top",
    legend.text     = element_text(colour = col_ink, size = 9),
    plot.margin     = margin(10, 15, 10, 10)
  )

# =============================================================================
# Panel B: Publication timeline (1970–2024)
# =============================================================================

timeline_df <- data.frame(
  Year = 1970:2024,
  n = c(
     34,  23,  33,  43,  44, 121, 117, 125, 125, 139,
    177, 192, 242, 331, 410, 512, 594, 658, 841, 980,
   1061,1417,1606,1936,2296,2424,2557,2586,2765,2926,
   2993,2995,3218,3270,3402,3419,3261,3493,3519,3462,
   3444,3440,3243,3135,2982,2874,2633,2349,2174,2051,
   1990,1975,1748,1736,1774
  )
)

p_timeline <- ggplot(timeline_df, aes(x = Year, y = n)) +
  geom_area(fill = col_accent, alpha = 0.25) +
  geom_line(colour = col_accent, linewidth = 0.9) +
  scale_x_continuous(
    breaks = seq(1970, 2024, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = "Publication year", y = "Number of papers") +
  base_theme +
  theme(
    axis.title   = element_text(colour = col_ink, size = 10, margin = margin(t = 6)),
    axis.text.y  = element_text(colour = col_muted, size = 9),
    plot.margin  = margin(10, 15, 10, 10)
  )

# =============================================================================
# Panel C: Top 10 journals
# =============================================================================

journals_df <- data.frame(
  Journal = c(
    "The Journal of Biological Chemistry",
    "PNAS",
    "Journal of Bacteriology",
    "Biochemical and Biophysical\nResearch Communications",
    "Oncogene",
    "Molecular and Cellular Biology",
    "The EMBO Journal",
    "Molecular Microbiology",
    "Biochemistry",
    "FEBS Letters"
  ),
  n = c(13091, 5116, 3443, 3005, 2435, 2435, 1975, 1903, 1848, 1835)
) %>%
  arrange(n) %>%
  mutate(
    Journal = factor(Journal, levels = Journal),
    label   = formatC(n, format = "d", big.mark = ",")
  )

p_journals <- ggplot(journals_df, aes(x = n, y = Journal)) +
  geom_col(fill = col_accent, width = 0.6, alpha = 0.9) +
  geom_text(aes(label = label), hjust = -0.1, size = 2.9, colour = col_ink) +
  scale_x_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(x = "Number of entries", y = NULL) +
  base_theme +
  theme(
    axis.text.y  = element_text(colour = col_ink, size = 8.5, lineheight = 0.9),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    axis.title.x = element_text(colour = col_ink, size = 10, margin = margin(t = 7)),
    plot.margin  = margin(10, 15, 10, 10)
  )

# =============================================================================
# Panel D: Confidence distributions — Stage 1 and Stage 2, side by side
# Stage 1 (Mechanism_Probability): binary classifier output, 0.50–0.99
# Stage 2 (Type_Confidence):       7-class classifier output, 0.18–0.95
# =============================================================================

stage1_conf <- data.frame(
  bin = c(
    0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
    0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
    0.70,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,
    0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,
    0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99
  ),
  n = c(
     780,1455,1460,1498,1432,1416,1396,1419,1435,1393,
    1371,1359,1300,1317,1406,1317,1351,1309,1431,1374,
    1358,1373,1356,1301,1416,1435,1454,1424,1469,1516,
    1507,1566,1708,1679,1768,1873,1938,1932,2009,2255,
    2435,2648,2788,3046,3593,3973,4540,5174,6404,3500
  ),
  Stage = "Stage 1"
)

stage2_conf <- data.frame(
  bin = c(
    0.18,0.19,0.20,0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,
    0.30,0.31,0.32,0.33,0.34,0.35,0.36,0.37,0.38,0.39,
    0.40,0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,
    0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
    0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
    0.70,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,
    0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,
    0.90,0.91,0.92,0.93,0.94,0.95
  ),
  n = c(
        1,   3,   5,  10,  18,  30,  34,  76,  89, 129, 132, 153,
      190, 201, 229, 268, 285, 285, 333, 340, 355, 368,
      397, 426, 431, 415, 437, 445, 441, 432, 439, 473,
      465, 458, 454, 436, 432, 450, 457, 465, 481, 507,
      516, 486, 491, 531, 588, 567, 627, 616, 613, 618,
      686, 718, 777, 854, 870, 940,1056,1112,1256,1311,
     1428,1566,1928,2184,2396,2934,3407,4071,4918,6325,
     6920,7653,8379,9389,6273, 178
  ),
  Stage = "Stage 2"
)

conf_df <- rbind(stage1_conf, stage2_conf) %>%
  mutate(Stage = factor(Stage, levels = c("Stage 1", "Stage 2")))

p_conf <- ggplot(conf_df, aes(x = bin, y = n)) +
  geom_col(fill = col_accent, width = 0.019, alpha = 0.85) +
  facet_wrap(~ Stage, scales = "free", nrow = 1) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = seq(0.2, 1.0, by = 0.2), expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Confidence score", y = "Number of entries") +
  theme_classic(base_size = 11) +
  theme(
    strip.text       = element_text(colour = col_ink, face = "bold", size = 9.5),
    strip.background = element_rect(fill = "#fef5f0", colour = NA),
    axis.title       = element_text(colour = col_ink, size = 10, margin = margin(t = 6)),
    axis.text        = element_text(colour = col_muted, size = 8.5),
    axis.line        = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.ticks       = element_line(colour = "#cccccc", linewidth = 0.4),
    panel.background = element_rect(fill = col_bg, colour = NA),
    plot.background  = element_rect(fill = col_bg, colour = NA),
    panel.spacing    = unit(1.2, "lines"),
    plot.margin      = margin(10, 15, 10, 10)
  )

# =============================================================================
# Combine: 2×2 patchwork
# =============================================================================

combined <- (
  (p_source + tag_theme) | (p_timeline + tag_theme)
) / (
  (p_journals + tag_theme) | (p_conf + tag_theme)
) +
  plot_layout(heights = c(1, 1.4)) +
  plot_annotation(tag_levels = "A")

# --- Save --------------------------------------------------------------------
output_path <- "figures/figure_section35.png"

ggsave(
  filename = output_path,
  plot     = combined,
  width    = 14,
  height   = 10,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)