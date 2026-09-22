# =============================================================================
# Figure: Mechanism type and species composition, AI-assisted curation vs
#         curated records
# Description: (A) Faceted horizontal bar chart of mechanism types for the
#              curated dataset and AI-assisted curation, same x-axis scale.
#              (B) Species composition of curated and AI-assisted curation
#              records with an annotated organism, read from the app database.
# Output: figures/figure_predicted_distribution.png (300 dpi)
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(DBI)
library(RSQLite)

ai_label      <- "AI-assisted curation"
curated_label <- "Curated dataset"

# --- Data: mechanism types ---------------------------------------------------
predicted <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoubiquitination", "Autocatalytic",
    "Autoregulation", "Autoinhibition", "Autolysis", "Autoinducer"
  ),
  pct   = c(58.1, 11.9, 11.7, 9.6, 4.0, 2.6, 2.1),
  Group = paste0(ai_label, "\n(n = 97,657 across 3,340,955 abstracts)")
)

actual <- data.frame(
  Mechanism = c(
    "Autophosphorylation", "Autoregulation", "Autocatalytic",
    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"
  ),
  pct   = c(53.4, 11.9, 11.0, 9.2, 8.9, 2.9, 2.9),
  Group = paste0(curated_label, "\n(n = 1,332)")
)

df <- rbind(predicted, actual)

# Mechanism order: ascending by predicted % so largest sits at top of y-axis
mech_order <- predicted %>%
  arrange(pct) %>%
  pull(Mechanism)

df <- df %>%
  mutate(
    Mechanism = factor(Mechanism, levels = mech_order),
    Group     = factor(Group, levels = c(unique(actual$Group), unique(predicted$Group))),
    label     = sprintf("%.1f%%", pct)
  )

# --- Data: species composition -----------------------------------------------
# Records whose organism is annotated; strains are merged into their species
# (first two words of the organism name, e.g. all Escherichia coli strains).
n_species <- 10

con <- dbConnect(SQLite(), "shiny_app/data/predictions.db")
records <- dbGetQuery(con, "SELECT Source, OS FROM predictions")
dbDisconnect(con)

species_df <- records %>%
  mutate(
    OS      = trimws(OS),
    Record  = ifelse(Source == "Predicted", ai_label, "Curated records"),
    Species = sub("^(\\S+\\s+\\S+).*$", "\\1", OS)
  ) %>%
  filter(!is.na(OS), OS != "")

group_n <- species_df %>% count(Record, name = "n_annotated")

# Top species by their average share across the two groups
top_species <- species_df %>%
  count(Record, Species) %>%
  group_by(Record) %>%
  mutate(share = n / sum(n)) %>%
  group_by(Species) %>%
  summarise(share = sum(share) / 2, .groups = "drop") %>%
  arrange(desc(share)) %>%
  slice_head(n = n_species) %>%
  pull(Species)

species_pct <- species_df %>%
  mutate(Species = ifelse(Species %in% top_species, Species, "Other species")) %>%
  count(Record, Species) %>%
  group_by(Record) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  left_join(group_n, by = "Record") %>%
  mutate(
    Group = paste0(Record, "\n(n = ", format(n_annotated, big.mark = ",", trim = TRUE),
                   " records with an annotated organism)"),
    Group   = factor(Group, levels = unique(Group[order(Record != "Curated records")])),
    Species = factor(Species, levels = rev(c(top_species, "Other species"))),
    label   = sprintf("%.1f%%", pct)
  )

# --- Colour palette ----------------------------------------------------------
col_accent <- "#d97742"
col_ink    <- "#1a2332"
col_muted  <- "#6b7a89"
col_bg     <- "#ffffff"

# --- Shared styling ----------------------------------------------------------
bar_theme <- theme_classic(base_size = 12) +
  theme(
    strip.text       = element_text(
      colour = col_ink, face = "bold", size = 10
    ),
    strip.background = element_rect(fill = "#fef5f0", colour = NA),
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
    panel.spacing    = unit(1.5, "lines"),
    plot.margin      = margin(15, 20, 15, 15),
    plot.tag         = element_text(face = "bold", size = 14)
  )

bar_layers <- function(x_max) {
  list(
    # Bar
    geom_col(fill = col_accent, alpha = 0.75, width = 0.65),
    # Label at bar end
    geom_text(
      aes(x = pct + 0.8, label = label),
      hjust = 0, size = 3.1, colour = col_ink
    ),
    # Two facets — fixed shared x-axis for direct comparison
    facet_wrap(~ Group, ncol = 2),
    labs(x = "Percentage (%)", y = NULL),
    scale_x_continuous(
      limits = c(0, x_max),
      breaks = seq(0, x_max, by = 10),
      expand = expansion(mult = c(0.02, 0))
    ),
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))),
    bar_theme
  )
}

# --- Plot --------------------------------------------------------------------
p_mech <- ggplot(df, aes(x = pct, y = Mechanism)) +
  bar_layers(72)

species_max <- ceiling((max(species_pct$pct) + 8) / 10) * 10
p_species <- ggplot(species_pct, aes(x = pct, y = Species)) +
  bar_layers(species_max) +
  theme(axis.text.y = element_text(colour = col_ink, size = 11, face = "italic"))

p <- p_mech / p_species +
  plot_layout(heights = c(7, n_species + 1)) +
  plot_annotation(tag_levels = "A")

# --- Save --------------------------------------------------------------------
output_path <- "figures/figure_predicted_distribution.png"

ggsave(
  filename = output_path,
  plot     = p,
  width    = 10,
  height   = 10.5,
  dpi      = 300,
  bg       = col_bg
)

message("Saved: ", output_path)
print(species_pct %>% select(Record, Species, n, pct) %>% arrange(Record, desc(pct)), n = 30)
