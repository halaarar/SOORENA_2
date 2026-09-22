library(shiny)        # for building the interactive web app
library(DT)          # for rendering interactive data tables
library(dplyr)       # for data manipulation
library(DBI)         # for database interface
library(RSQLite)     # for SQLite database
library(shinyjs)     # for JavaScript integration (e.g., toggle dark mode)
library(htmltools)   # for safe HTML rendering
library(plotly)      # Creates interactive, dynamic, and web-friendly plots from ggplot or standalone
library(ggplot2)
library(shinycssloaders) # for loading spinners
library(rsconnect)  # for deploying the app to shinyapps.io

# Connect to SQLite Database
cat("Connecting to SQLite database...\n")
DB_PATH <- "data/predictions.db"

if (!file.exists(DB_PATH)) {
  stop(paste("Database not found:", DB_PATH, "\n",
             "Build it from the merged CSV using:\n",
             "  python scripts/python/data_processing/create_sqlite_db.py \\\n",
             "    --input shiny_app/data/predictions.csv \\\n",
             "    --output shiny_app/data/predictions.db\n",
             "See docs/README.md for the full workflow."))
}

# Create database connection
conn_info <- dbConnect(RSQLite::SQLite(), DB_PATH)

# Get total row count
total_rows <- dbGetQuery(conn_info, "SELECT COUNT(*) as count FROM predictions")$count
cat(paste("Connected to database with", format(total_rows, big.mark=","), "rows\n"))

cat("Database ready!\n")

# Close the initial info connection; each Shiny session will open its own
dbDisconnect(conn_info)

# How many rows to fetch per page for the main table
DEFAULT_PAGE_SIZE <- 50
PAGE_SIZE_OPTIONS <- c(25, 50, 100, 500)

# Ontology Info Table (authoritative)
ontology_info <- list(
  Autokinase = list(
    Definition = "A protein kinase that phosphorylates itself, typically through autophosphorylation at specific residues (Huang et al., 2017; Mechaly et al., 2017).",
    Synonym = "self-kinase, autophosphorylating kinase, self-phosphorylating enzyme",
    Antonym = "heterophosphorylation, trans-phosphorylation",
    Related = "autophosphorylation"
  ),
  Autophosphorylation = list(
    Definition = "The process by which a protein kinase phosphorylates itself, often leading to activation or conformational changes (Wang, 2002; Bayliss et al., 2023).",
    Synonym = "self-phosphorylation, cis-phosphorylation, intramolecular phosphorylation",
    Antonym = "dephosphorylation, heterophosphorylation",
    Related = "autokinase"
  ),
  Autoubiquitination = list(
    Definition = "The process where an E3 ubiquitin ligase attaches ubiquitin to itself, typically marking itself for degradation (Ciechanover et al., 2011; Amemiya et al., 2008).",
    Synonym = "self-ubiquitination, cis-ubiquitination, auto-ubiquitylation",
    Antonym = "deubiquitination, heteroubiquitination",
    Related = "protein degradation"
  ),
  Autolysis = list(
    Definition = "The process of self-digestion where a protease cleaves itself, often resulting in activation or inactivation (Kapust et al., 2001; Little, 1991).",
    Synonym = "self-cleavage, autocatalytic cleavage, self-proteolysis",
    Antonym = "heterolysis, trans-cleavage",
    Related = "autocatalysis"
  ),
  Autocatalysis = list(
    Definition = "A catalytic process where the product of the reaction catalyzes its own formation or the enzyme catalyzes its own modification (Klemm et al., 2009; Lv et al., 2020).",
    Synonym = "self-catalysis, positive feedback catalysis, autocatalytic reaction",
    Antonym = "heterocatalysis, negative feedback",
    Related = "autolysis, positive feedback"
  ),
  Autoactivation = list(
    Definition = "A regulatory mechanism where a protein positively regulates its own expression or activity (Thomas et al., 2018; García-López et al., 2019).",
    Synonym = "self-activation, positive autoregulation, self-stimulation",
    Antonym = "autoinhibition, negative autoregulation",
    Related = "positive feedback, autoinduction"
  ),
  Autoinhibition = list(
    Definition = "A regulatory mechanism where a protein negatively regulates its own expression or activity (Chen et al., 2020; Rodriguez-Martinez et al., 2021).",
    Synonym = "self-inhibition, negative autoregulation, self-repression",
    Antonym = "autoactivation, positive autoregulation",
    Related = "negative feedback, autorepression"
  ),
  Autoinduction = list(
    Definition = "A process where a protein induces its own expression, typically through positive feedback loops (Miller & Bassler, 2018; Sharma et al., 2022).",
    Synonym = "self-induction, auto-stimulation, positive feedback induction",
    Antonym = "autorepression, negative feedback",
    Related = "autoactivation, positive feedback"
  ),
  Autofeedback = list(
    Definition = "A regulatory loop where a protein's output influences its own activity or expression (Williams et al., 2019; Kumar et al., 2020).",
    Synonym = "self-feedback, autoregulatory loop, feedback regulation",
    Antonym = "feedforward regulation, open-loop control",
    Related = "feedback loop, homeostasis"
  ),
  Autoregulation = list(
    Definition = "The general process by which a biological system regulates its own activity, expression, or function (Johnson & Smith, 2021; Martinez et al., 2019).",
    Synonym = "self-regulation, autonomous regulation, homeostatic control",
    Antonym = "heteroregulation, external regulation",
    Related = "homeostasis, feedback control"
  ),
  Autodephosphorylation = list(
    Definition = "The process by which a protein removes phosphate groups from itself, typically through intrinsic phosphatase activity or conformational changes that reverse autophosphorylation.",
    Synonym = "self-dephosphorylation, autophosphatase activity, intrinsic phosphatase",
    Antonym = "autophosphorylation, heterophosphatase",
    Related = "autophosphorylation, phosphatase activity"
  ),
  Autoacetylation = list(
    Definition = "The process where an acetyltransferase acetylates itself, adding acetyl groups to its own lysine residues, often affecting protein activity or stability.",
    Synonym = "self-acetylation, cis-acetylation, intrinsic acetyltransferase activity",
    Antonym = "deacetylation, heteroacetylation",
    Related = "histone modification, lysine acetylation, protein acetylation"
  ),
  Autodemethylation = list(
    Definition = "The process by which a protein removes methyl groups from itself through intrinsic demethylase activity, reversing the effects of methylation.",
    Synonym = "self-demethylation, intrinsic demethylase activity",
    Antonym = "automethylation, heteromethylation",
    Related = "protein methylation, demethylase activity, epigenetic regulation"
  )
)

# Map dataset labels to ontology keys when names differ
ontology_key_map <- c(
  "Autocatalytic" = "Autocatalysis",
  "Autoinducer"   = "Autoinduction"
  # Add more aliases here if your data contains other variants
)

# Polarity is derived deterministically from mechanism class (no separate model).
# Symbols: "+" positive/self-amplifying, "–" negative/self-limiting, "±" context-dependent.
polarity_symbol_map <- c(
  Autocatalysis = "+",
  Autophosphorylation = "+",
  Autodephosphorylation = "–",
  Autoacetylation = "+",
  Autodemethylation = "±",
  Autoubiquitination = "–",
  Autoregulation = "±",
  Autoinhibition = "–",
  Autolysis = "–",
  Autoinduction = "+"
)

get_polarity_symbol <- function(key) {
  sym <- polarity_symbol_map[[key]]
  if (is.null(sym) || is.na(sym) || sym == "") return("Unknown")
  sym
}

# Ontology path helper for display
get_ontology_path <- function(key) {
  enzymatic <- c("Autokinase","Autophosphorylation","Autodephosphorylation","Autoacetylation","Autodemethylation","Autoubiquitination","Autolysis","Autocatalysis")
  expression <- c("Autoactivation","Autoinhibition","Autoinduction","Autofeedback","Autoregulation")
  if (key %in% enzymatic)   return(paste("Autoregulatory Mechanisms (Root) → Enzymatic Self-Modification →", key))
  if (key %in% expression)  return(paste("Autoregulatory Mechanisms (Root) → Expression Control →", key))
  "Autoregulatory Mechanisms (Root)"
}

# Source display labels.
# The database stores Source = "Predicted"; users see a phrase that better
# describes what the pipeline does. Change PREDICTED_LABEL to reword it
# everywhere (filter, table, CSV export, statistics chart) at once.
PREDICTED_LABEL <- "AI-assisted curation"

source_display_map <- c("Predicted" = PREDICTED_LABEL)

display_source <- function(x) {
  out <- unname(source_display_map[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}


# Define UI
# Header UI reused across all tabs
header_ui <- div(
  class = "header-section",
  div(class = "app-title", img(src = "logo.png"), "SOORENA"),
  div(class = "header-logos",
      img(src = "logoHelsinki.png"),
      img(src = "logoUBC.png"),
      img(src = "Tampere.png", alt = "Tampere University"),
      img(src = "logoJafariLab.png")
  )
)

# Define UI
ui <- navbarPage(
  title = NULL,
  id = "main_nav",

  # Tab: Search and Main App Interface
  # background-image: url('header_img.png');
  tabPanel(
    title = "Search",
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$style(HTML("
          body {
            background-color: #f9f9f9;
          }
          /* Truncated cells reveal their full value on mouseover */
          .truncated-cell {
            border-bottom: 1px dotted #9aa5b1;
            cursor: help;
          }
          .view-btn {
            padding: 0 2px;
            font-size: 0.85em;
            white-space: nowrap;
          }.navbar {
            background-color:  #f2f2f2;
            border-color: #1f4e79;
          }.navbar .navbar-nav > li > a,
           .navbar .navbar-brand {
           color: black;
          }
          :root {
            --stats-bg: #f5f1e8;
            --stats-surface: #ffffff;
            --stats-ink: #1a2332;
            --stats-muted: #6b7a89;
            --stats-accent: #d97742;
            --stats-accent-2: #1a2332;
            --stats-accent-3: #2c3e50;
            --stats-border: #e8dcc8;
            --stats-shadow: 0 8px 24px rgba(26, 35, 50, 0.08);
          }
          .header-section {
            background-size: 50% auto;
            background-position: center;
            background-repeat: no-repeat;
            background-color: #ffffff;
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-top: 20px;
            padding: 20px 30px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .app-title {
            font-size: 48px;
            font-weight: bold;
            color: #2c3e50;
            margin: 0;
            display: flex;
            align-items: center;
          }
          .app-title img {
            height: 120px;
            margin-right: 20px;
          }
          .header-logos {
            display: flex;
            gap: 20px;
          }
          .header-logos img {
            height: 120px;
          }
	          .filter-panel {
	            background-color: #ffffff;
	            padding: 20px;
	            border-radius: 8px;
	            margin: 20px 30px;
	            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
	          }
	          .filter-top-row {
	            display: flex;
	            align-items: flex-end;
	            justify-content: flex-start;
	            gap: 12px 16px;
	            flex-wrap: wrap;
	          }
	          .filter-top-left {
	            display: flex;
	            align-items: flex-end;
	            justify-content: flex-start;
	            gap: 12px 16px;
	            flex-wrap: wrap;
	            flex: 1 1 520px;
	          }
	          .filter-top-search {
	            flex: 1 1 520px;
	            max-width: 720px;
	            min-width: 280px;
	          }
	          .filter-top-search .shiny-input-container {
	            width: 100%;
	          }
	          .filter-top-search .form-group {
	            margin-bottom: 0;
	          }
	          .search-label-row {
	            display: flex;
	            align-items: center;
	            justify-content: space-between;
	            gap: 12px;
	            margin-bottom: 6px;
	          }
	          .filter-top-row .search-label-row .control-label {
	            margin: 0;
	          }
	          .filter-top-right {
	            display: flex;
	            align-items: flex-end;
	            justify-content: flex-start;
	            gap: 12px;
	            flex-wrap: wrap;
	            margin-left: auto;
	          }
	          .filter-top-row .form-group {
	            margin-bottom: 0;
	          }
	          .filter-top-row .control-label {
	            margin-bottom: 6px;
	            font-weight: 600;
	          }
          .match-pill {
            display: inline-flex;
            align-items: center;
            padding: 8px 12px;
            border-radius: 999px;
            background: rgba(217, 119, 66, 0.12);
            color: #1a2332;
            font-weight: 600;
            font-size: 14px;
            white-space: nowrap;
          }
	          .match-exact-toggle .form-group {
	            margin: 0;
	          }
	          .match-exact-toggle .control-label {
	            display: none;
	          }
	          .match-exact-toggle .checkbox {
	            margin: 0;
	          }
	          .match-exact-toggle .checkbox label {
	            position: relative;
	            margin: 0;
	            padding: 8px 12px;
	            border-radius: 999px;
	            border: 1px solid #d1d5db;
	            background: #ffffff;
	            font-weight: 700;
	            color: #1a2332;
	            cursor: pointer;
	            user-select: none;
	            display: inline-flex;
	            align-items: center;
	            justify-content: center;
	            box-shadow: 0 2px 6px rgba(15, 23, 42, 0.06);
	            transition: background 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
	          }
	          .match-exact-toggle .checkbox input[type=checkbox] {
	            position: absolute;
	            opacity: 0;
	            width: 0;
	            height: 0;
	          }
	          .match-exact-toggle .checkbox label:has(input:checked) {
	            background: rgba(217, 119, 66, 0.14);
	            border-color: rgba(217, 119, 66, 0.75);
	            box-shadow: 0 4px 12px rgba(217, 119, 66, 0.12);
	          }
          .btn-clear {
            background: #ffffff !important;
            border: 1px solid rgba(217, 119, 66, 0.7) !important;
            color: #d97742 !important;
            font-weight: 700;
          }
	          .btn-clear:hover {
	            background: rgba(217, 119, 66, 0.08) !important;
	            border-color: rgba(217, 119, 66, 0.95) !important;
	          }
	          .help-icon {
	            text-decoration: none !important;
	          }
          details.more-filters {
            margin-top: 14px;
            border: 1px solid #f0e6d6;
            border-radius: 10px;
            background: #fbf7f0;
            padding: 10px 12px;
          }
          details.more-filters summary {
            cursor: pointer;
            font-weight: 700;
            color: #1a2332;
            list-style: none;
            outline: none;
          }
          details.more-filters summary::-webkit-details-marker {
            display: none;
          }
          details.more-filters summary:after {
            content: \"▾\";
            float: right;
            color: #d97742;
            font-weight: 700;
          }
          details.more-filters[open] summary:after {
            content: \"▴\";
          }
          .filter-card {
            margin-top: 12px;
            background: #ffffff;
            border: 1px solid #f0e6d6;
            border-radius: 10px;
            padding: 14px;
          }
          .filter-card h4 {
            margin-top: 0;
            margin-bottom: 12px;
            color: #1a2332;
            font-weight: 700;
          }
          .help-icon {
            display: inline-flex;
            align-items: center;
            justify-content: center;
            width: 18px;
            height: 18px;
            margin-left: 6px;
            border-radius: 999px;
            border: 1px solid rgba(148, 163, 184, 0.6);
            color: #64748b;
            font-size: 12px;
            cursor: help;
          }
          .polarity-toggle .shiny-options-group {
            display: flex;
            align-items: center;
            gap: 10px;
            flex-wrap: wrap;
          }
          .polarity-toggle label.checkbox-inline {
            position: relative;
            margin: 0;
            padding: 8px 12px;
            border-radius: 999px;
            border: 1px solid #d1d5db;
            background: #ffffff;
            font-weight: 800;
            color: #1a2332;
            cursor: pointer;
            user-select: none;
            display: inline-flex;
            align-items: center;
            justify-content: center;
            min-width: 46px;
            box-shadow: 0 2px 6px rgba(15, 23, 42, 0.06);
            transition: background 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
          }
          .polarity-toggle label.checkbox-inline input[type=checkbox] {
            position: absolute;
            opacity: 0;
            width: 0;
            height: 0;
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"+\"]) {
            border-color: rgba(34, 197, 94, 0.35);
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"–\"]) {
            border-color: rgba(239, 68, 68, 0.35);
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"±\"]) {
            border-color: rgba(99, 102, 241, 0.35);
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"+\"]:checked) {
            background: rgba(34, 197, 94, 0.14);
            border-color: rgba(34, 197, 94, 0.7);
            box-shadow: 0 4px 12px rgba(34, 197, 94, 0.18);
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"–\"]:checked) {
            background: rgba(239, 68, 68, 0.14);
            border-color: rgba(239, 68, 68, 0.7);
            box-shadow: 0 4px 12px rgba(239, 68, 68, 0.14);
          }
          .polarity-toggle label.checkbox-inline:has(input[value=\"±\"]:checked) {
            background: rgba(99, 102, 241, 0.14);
            border-color: rgba(99, 102, 241, 0.7);
            box-shadow: 0 4px 12px rgba(99, 102, 241, 0.14);
          }
          .year-range-group > label {
            font-weight: 600;
            margin-bottom: 6px;
          }
          .year-range-inputs {
            display: flex;
            gap: 10px;
          }
          .year-range-inputs .form-group {
            margin-bottom: 0;
            flex: 1;
          }
          .year-range-inputs .selectize-input {
            border-radius: 6px;
          }
          .source-select .form-group {
            margin-bottom: 0;
          }
          .btn-warning {
            margin-top: 10px;
          }
          .dataTables_wrapper {
            background-color: #ffffff;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            margin: 0 0 20px 0;
          }
          /* Statistics tab styling */
          .filter-panel h2 {
            color: #1a2332;
            border-bottom: 2px solid #d97742;
            padding-bottom: 10px;
            margin-bottom: 20px;
            font-weight: 600;
          }
          .filter-panel h3 {
            color: #34495e;
            margin-top: 30px;
            margin-bottom: 15px;
            font-weight: 600;
          }
          .filter-panel h4 {
            color: #7f8c8d;
            font-weight: 600;
            margin-bottom: 15px;
            font-size: 16px;
          }
          .table {
            font-size: 14px;
            margin-top: 10px;
          }
          .table th {
            background-color: #ecf0f1;
            font-weight: 600;
            color: #2c3e50;
          }
          .table-hover tbody tr:hover {
            background-color: #f5f6f7;
          }
          .stats-panel {
            position: relative;
            background: #f5f1e8;
            border: 1px solid #e8dcc8;
            border-radius: 12px;
            padding: 24px;
            margin: 30px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          }
          .stats-panel--compact {
            max-width: 1100px;
            margin: 30px auto;
          }
          .stats-header {
            display: flex;
            align-items: flex-start;
            justify-content: space-between;
            gap: 16px;
            margin-bottom: 18px;
          }
          .stats-title {
            color: var(--stats-ink);
            font-size: 26px;
            font-weight: 700;
            margin: 0 0 6px 0;
          }
          .stats-subtitle {
            color: var(--stats-muted);
            margin: 0;
            font-size: 14px;
          }
          .stats-pill {
            padding: 6px 12px;
            border-radius: 999px;
            background: rgba(42, 157, 143, 0.12);
            color: var(--stats-accent);
            font-size: 12px;
            font-weight: 600;
            text-transform: uppercase;
            letter-spacing: 0.6px;
            white-space: nowrap;
          }
          .stats-grid {
            display: grid;
            gap: 44px;
          }
          .stats-grid + .stats-grid {
            margin-top: 22px;
          }
          .stats-grid--top {
            grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
          }
          .stats-grid--two {
            grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
          }
          .stats-grid--one {
            grid-template-columns: 1fr;
          }
          .stat-card {
            background: var(--stats-surface);
            border: 1px solid var(--stats-border);
            border-radius: 14px;
            padding: 16px;
            box-shadow: 0 6px 14px rgba(15, 23, 42, 0.08);
            animation: statsFadeUp 0.6s ease;
            transition: box-shadow 0.2s ease, border-color 0.2s ease;
            position: relative;
            z-index: 0;
            min-width: 0;
          }
          .stat-card:hover {
            box-shadow: 0 8px 16px rgba(15, 23, 42, 0.12);
            border-color: rgba(42, 157, 143, 0.35);
            z-index: 2;
          }
          .stat-card__header {
            display: flex;
            align-items: baseline;
            justify-content: space-between;
            gap: 8px;
          }
          .stat-card__title {
            color: var(--stats-ink);
            font-size: 16px;
            font-weight: 600;
            margin: 0 0 10px 0;
          }
          .stat-card__meta {
            color: var(--stats-muted);
            font-size: 12px;
            text-transform: uppercase;
            letter-spacing: 0.4px;
          }
          .stat-value-centered {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            width: 100%;
            text-align: center;
          }
          .stat-value-centered #stat_total_papers {
            font-size: 56px;
            font-weight: 700;
            color: var(--stats-ink);
            line-height: 1;
            font-variant-numeric: tabular-nums;
          }
          .stat-label {
            color: var(--stats-muted);
            font-size: 13px;
            text-transform: uppercase;
            letter-spacing: 0.6px;
            font-weight: 600;
          }
          .stat-value {
            margin-top: 8px;
            margin-bottom: 6px;
            display: flex;
            align-items: center;
            justify-content: center;
          }
          #stat_total_papers {
            font-size: 44px;
            font-weight: 700;
            color: var(--stats-ink);
            line-height: 1;
            font-variant-numeric: tabular-nums;
          }
          .stat-meta {
            color: var(--stats-muted);
            font-size: 12px;
          }
          .chart-note {
            color: var(--stats-muted);
            font-size: 12px;
            margin: -4px 0 8px 0;
          }
          .stats-note {
            color: var(--stats-muted);
            font-size: 13px;
            margin-top: 16px;
            font-style: italic;
          }
          .stats-table-card .table {
            margin-bottom: 0;
          }
          .plotly, .plot-container, .js-plotly-plot, .html-widget {
            max-width: 100%;
          }
          .ontology-panel {
            overflow-x: hidden;
          }
          .ontology-tree {
            overflow-x: auto;
            -webkit-overflow-scrolling: touch;
          }
	          @media (max-width: 768px) {
            .header-section {
              flex-direction: column;
              align-items: center;
              gap: 12px;
              margin: 12px;
              padding: 16px;
              text-align: center;
            }
            .app-title {
              flex-direction: column;
              font-size: 32px;
            }
            .app-title img {
              height: 64px;
              margin: 0 0 8px 0;
            }
            .header-logos {
              flex-wrap: wrap;
              justify-content: center;
              gap: 12px;
            }
            .header-logos img {
              height: 56px;
            }
		            .filter-panel {
		              margin: 12px;
		              padding: 16px;
		            }
		            .filter-top-right {
		              margin-left: 0;
		            }
		            .table-toolbar {
		              flex-direction: column;
		              align-items: flex-start;
		              gap: 12px;
		            }
            .table-toolbar__left {
              flex-wrap: wrap;
            }
            .table-toolbar__download {
              width: 100%;
            }
            .table-toolbar-wrap {
              margin: 0 12px 16px 12px !important;
            }
            .dataTables_wrapper {
              padding: 12px;
            }
            .stats-header {
              flex-direction: column;
              align-items: flex-start;
            }
            .stats-panel {
              margin: 12px;
              padding: 16px;
            }
            .stats-grid {
              gap: 20px;
            }
            .stats-grid--top,
            .stats-grid--two,
            .stats-grid--one {
              grid-template-columns: 1fr;
            }
            .stat-card__title {
              font-size: 14px;
            }
            .stat-card__meta {
              font-size: 10px;
            }
            .stat-value-centered {
              position: static;
              transform: none;
              margin-top: 8px;
            }
            .stat-value-centered #stat_total_papers {
              font-size: 28px;
            }
            .navbar .navbar-nav > li > a {
              padding: 10px 12px;
            }
            .table {
              font-size: 12px;
            }
            .plotly, .plot-container, .js-plotly-plot, .html-widget {
              width: 100% !important;
            }
            #stat_mechanism_plot,
            #stat_source_plot {
              height: 220px !important;
            }
            #stat_type_plot,
            #stat_year_plot,
            #stat_journal_plot,
            #stat_probability_plot,
            #stat_type_confidence_plot {
              height: 260px !important;
            }
            .modebar {
              display: none !important;
            }
            .ontology-panel {
              margin: 12px !important;
              padding: 16px !important;
              font-size: 14px !important;
              line-height: 1.6 !important;
            }
            .ontology-panel h2 {
              font-size: 20px !important;
            }
            .ontology-panel h3 {
              font-size: 16px !important;
            }
            .ontology-panel h4 {
              font-size: 16px !important;
            }
            .ontology-tree {
              margin: 12px 0 !important;
              padding: 16px !important;
            }
            .ontology-tree-lines {
              margin-left: 16px !important;
            }
            .ontology-tree-node {
              margin-left: 14px !important;
              padding: 10px 12px !important;
            }
            .mechanism-box {
              margin: 18px 0 !important;
              padding: 16px !important;
            }
            .stats-table-card {
              overflow-x: auto;
              -webkit-overflow-scrolling: touch;
            }
            .team-grid {
              gap: 16px !important;
            }
            .team-card {
              flex: 1 1 100% !important;
              max-width: 320px;
              width: 100%;
              margin: 0 auto;
            }
            .about-hero {
              margin: 24px 0 32px 0 !important;
            }
            .about-description {
              margin: 0 auto 32px auto !important;
            }
          }
          @media (max-width: 480px) {
            .app-title {
              font-size: 28px;
            }
            .header-logos img {
              height: 48px;
            }
            .stats-title {
              font-size: 20px;
            }
          }
          @keyframes statsFadeUp {
            from {
              opacity: 0;
              transform: translateY(8px);
            }
            to {
              opacity: 1;
              transform: translateY(0);
            }
          }
        "))
      ),

      # Header section
      header_ui,

	      # Search and Filter Controls
				  div(class = "filter-panel",
				    div(
				      class = "filter-top-row",
				      div(
				        class = "filter-top-left",
				        div(
				          class = "filter-top-search",
				          div(
				            class = "search-label-row",
				            tags$label("Title/Abstract search", `for` = "search", class = "control-label"),
				            div(
				              class = "match-exact-toggle",
				              checkboxInput("match_exact", label = "Exact match", value = FALSE)
				            )
				          ),
				          textInput("search", NULL, placeholder = "Search title or abstract...")
				        )
				      ),
				      div(
				        class = "filter-top-right",
				        div(class = "match-pill", textOutput("match_count_badge", inline = TRUE)),
				        actionButton("reset_filters", "Clear all", class = "btn-clear")
				      )
				    ),
				    tags$hr(style = "margin: 14px 0; border-top: 1px solid #eee;"),
				    fluidRow(
				      column(4,
				             selectizeInput("type", "Autoregulatory Type",
			                            choices = NULL,
			                            multiple = TRUE,
			                            options = list(placeholder = "Select type..."))),
			      column(3,
			             div(class = "polarity-toggle",
				                 checkboxGroupInput(
				                   "polarity",
				                   tags$span(
				                     "Polarity",
			                     actionLink(
			                       "polarity_help",
			                       label = "i",
			                       class = "help-icon",
			                       title = "Click for polarity definitions"
			                     )
				                   ),
				                   choices = c("+", "–", "±"),
				                   selected = c("+", "–", "±"),
				                   inline = TRUE
				                 ))),
			      column(3,
			             div(class = "year-range-group",
			                 tags$label("Year range"),
			                 div(class = "year-range-inputs",
			                     selectizeInput("year_from", NULL,
		                                    choices = c(""),
		                                    selected = "",
		                                    multiple = FALSE,
		                                    options = list(placeholder = "From...")),
		                     selectizeInput("year_to", NULL,
		                                    choices = c(""),
		                                    selected = "",
			                                    multiple = FALSE,
			                                    options = list(placeholder = "To..."))
			                 ))),
			      column(2,
			             div(class = "source-select",
			                 selectInput(
			                   "source_mode",
			                   "Source of Autoregulation Data",
			                   choices = setNames(
			                     c("all", "UniProt", "Predicted", "OmniPath", "SIGNOR", "TRRUST"),
			                     c("All", "UniProt", PREDICTED_LABEL, "OmniPath", "SIGNOR", "TRRUST")
			                   ),
			                   selected = "all"
			                 )))
			    ),
		    tags$details(
		      class = "more-filters",
		      tags$summary(textOutput("more_filters_title", container = span)),
		      div(class = "filter-card",
		        h4("Publication & Metadata"),
		        fluidRow(
		          column(3,
		                 selectizeInput("journal", "Journal",
		                                choices = NULL,
		                                multiple = TRUE,
		                                options = list(placeholder = "Select journal..."))),
		          column(3,
		                 selectizeInput("os", "Organism",
		                                choices = NULL,
		                                multiple = TRUE,
		                                options = list(placeholder = "Select Organism..."))),
		          column(3, textInput("author", "Author", placeholder = "Search author...")),
		          column(3,
		                 selectizeInput("month", "Publication Month",
		                                choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
		                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
		                                multiple = TRUE,
		                                options = list(placeholder = "Select month..."))),
		        )
		      ),
		      div(class = "filter-card",
		        h4("Proteins & IDs"),
		        fluidRow(
		          column(4, textInput("protein_name", "Protein Name", placeholder = "Search protein name...")),
		          column(4, textInput("gene_name", "Gene Name", placeholder = "Search gene name...")),
		          column(4, textInput("protein_id", "Protein ID", placeholder = "Search protein ID..."))
		        ),
		        fluidRow(
		          column(4, textInput("pmid", "PMID", placeholder = "Search PMID...")),
		          column(4, textInput("ac", "UniProt AC", placeholder = "Search UniProt accession...")),
		          column(4, textInput("record_ac", "AC (Record ID)", placeholder = "Search record AC..."))
		        )
		      ),
		      div(class = "filter-card",
		        h4("Model Confidence"),
		        fluidRow(
		          column(6,
		                 sliderInput("min_mechanism_prob",
		                            "Minimum Mechanism Probability",
		                            min = 0, max = 1, value = 0, step = 0.05,
		                            width = "100%")),
		          column(6,
		                 sliderInput("min_type_conf",
		                            "Minimum Type Confidence",
		                            min = 0, max = 1, value = 0, step = 0.05,
		                            width = "100%"))
		        ),
		        div(style = "margin-top: -10px; padding: 8px 12px; background: #f9f6f1; border-radius: 6px; font-size: 13px; color: #666;",
		            HTML("💡 <b>Tip:</b> Use the <i>Statistics</i> tab to view probability distribution and choose an appropriate threshold"))
		      )
		    )
		  ),

        # Display Table with loading spinner
        div(class = "table-toolbar-wrap", style = "margin: 0 30px 20px 30px;",
            div(
              class = "table-toolbar",
              style = "display: flex; align-items: center; justify-content: space-between; gap: 10px; margin-bottom: 10px;",
              div(
                class = "table-toolbar__left",
                style = "display: flex; align-items: center; gap: 10px;",
                selectInput("rows_per_page", "Rows per page",
                            choices = PAGE_SIZE_OPTIONS,
                            selected = DEFAULT_PAGE_SIZE,
                            width = "140px"),
                actionButton("prev_page", "Previous", class = "btn-default"),
                actionButton("next_page", "Next", class = "btn-default"),
                textOutput("page_status", inline = TRUE)
              ),
              downloadButton("download_csv", "Download CSV", class = "btn-primary mb-0 table-toolbar__download")
            ),
            withSpinner(DTOutput("result_table"),
                       type = 6,
                       color = "#2c3e50",
                       size = 1.5))
      )
    ),

  # Tab: Statistics
  tabPanel(
    title = "Statistics",
    fluidPage(
      header_ui,

      # Dataset Statistics Section
	      div(class = "stats-panel",
	        div(class = "stats-header",
	          div(
	            h2("Dataset Statistics", class = "stats-title"),
	            p("Based on current filters", class = "stats-subtitle"),
	            div(class = "stats-subtitle", textOutput("stat_filters_summary"))
	          ),
	          div(class = "stats-pill", "Live filtered view")
	        ),
        div(class = "stats-grid stats-grid--top",
          div(class = "stat-card stat-card--chart",
            h4("Total Matching Papers", class = "stat-card__title"),
            div(class = "stat-value-centered", textOutput("stat_total_papers", inline = TRUE))
          ),
          div(class = "stat-card stat-card--chart",
            div(class = "stat-card__header",
              h4("Source Mix", class = "stat-card__title"),
              span(class = "stat-card__meta", "Data Source Distribution")
            ),
            withSpinner(plotlyOutput("stat_source_plot", height = "230px"), type = 6, color = "#2c3e50")
          )
        ),
        div(class = "stats-grid stats-grid--two",
          div(class = "stat-card stat-card--wide",
            h4("Autoregulatory Types", class = "stat-card__title"),
            withSpinner(plotlyOutput("stat_type_plot", height = "340px"), type = 6, color = "#2c3e50")
          ),
          div(class = "stat-card stat-card--wide",
            div(class = "stat-card__header",
              h4("Publication Timeline", class = "stat-card__title"),
              span(class = "stat-card__meta", "Bubble size shows paper count")
            ),
            div(class = "chart-note", "Bubble size reflects paper count; color deepens with volume."),
            withSpinner(plotlyOutput("stat_year_plot", height = "340px"), type = 6, color = "#2c3e50")
          )
        ),
        div(class = "stats-grid stats-grid--one",
          div(class = "stat-card stat-card--wide",
            h4("Top Journals", class = "stat-card__title"),
            withSpinner(plotlyOutput("stat_journal_plot", height = "320px"), type = 6, color = "#2c3e50")
          )
        ),
        div(class = "stats-grid stats-grid--two",
          div(class = "stat-card stat-card--wide",
            div(class = "stat-card__header",
              h4("Mechanism Probability Distribution", class = "stat-card__title"),
              span(class = "stat-card__meta", "Stage 1 model confidence scores")
            ),
            div(class = "chart-note", "Distribution of confidence scores for autoregulatory mechanism detection. Use this to inform filtering thresholds."),
            withSpinner(plotlyOutput("stat_probability_plot", height = "340px"), type = 6, color = "#2c3e50")
          ),
          div(class = "stat-card stat-card--wide",
            div(class = "stat-card__header",
              h4("Type Confidence Distribution", class = "stat-card__title"),
              span(class = "stat-card__meta", "Stage 2 model confidence scores")
            ),
            div(class = "chart-note", "Distribution of confidence scores for mechanism type classification. Use this to inform filtering thresholds."),
            withSpinner(plotlyOutput("stat_type_confidence_plot", height = "340px"), type = 6, color = "#2c3e50")
          )
        )
      ),

      # Model Performance Section
      div(class = "stats-panel stats-panel--compact",
        div(class = "stats-header",
          div(
            h2("Model Training Performance", class = "stats-title"),
            p("Performance metrics from the published SOORENA study", class = "stats-subtitle")
          ),
          div(class = "stats-pill", "Model benchmarks")
        ),
        div(class = "stats-grid stats-grid--two",
          div(class = "stat-card stats-table-card",
            h4("Stage 1: Binary Classification (n = 600 test samples)", class = "stat-card__title"),
            tableOutput("model_stage1_table")
          ),
          div(class = "stat-card stats-table-card",
            h4("Stage 2: Multi-class - Overall Performance", class = "stat-card__title"),
            tableOutput("model_stage2_overall_table")
          )
        ),
        div(class = "stat-card stats-table-card", style = "margin-top: 16px;",
          h4("Stage 2: Per-class Performance", class = "stat-card__title"),
          tableOutput("model_stage2_perclass_table")
        ),
        p("Source: bioRxiv preprint doi: https://doi.org/10.1101/2025.11.03.685842",
          class = "stats-note")
      )
    )
  ),

  # Tab: Ontology
  tabPanel(
    title = "Ontology",
    fluidPage(
      header_ui,

      tags$head(
        tags$style(HTML("
          .tree-link {
            color: #1a2332;
            text-decoration: none;
            cursor: pointer;
            font-weight: 500;
          }
          .tree-link:hover {
            color: #d97742;
            text-decoration: underline;
          }
          .mechanism-box {
            scroll-margin-top: 80px;
          }
        "))
      ),

      div(class = "ontology-panel", style = "
         padding: 30px;
         background: #ffffff;
         border-radius: 12px;
         box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
         margin: 30px;
         font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
         line-height: 1.8;
         font-size: 15px;",

          h2("Autoregulatory Mechanisms Ontology", style = "color: #1a2332; font-weight: 700; margin-bottom: 10px; border-bottom: 3px solid #d97742; padding-bottom: 10px;"),

          p(style = "font-size: 16px; color: #555; margin-top: 20px;",
            "A structured classification of self-directed biochemical processes identified and categorized by the SOORENA pipeline."),

          div(style = "margin: 14px 0 22px 0; padding: 12px 14px; background: #f5f1e8; border: 1px solid #e8dcc8; border-radius: 10px;",
            tags$span(style = "font-weight: 700; color: #1a2332; margin-right: 8px;", "Polarity legend:"),
            tags$span(style = "display: inline-block; margin-right: 10px;", tags$b("+"), " positive / self-amplifying"),
            tags$span(style = "display: inline-block; margin-right: 10px;", tags$b("–"), " negative / self-limiting"),
            tags$span(style = "display: inline-block;", tags$b("±"), " context-dependent")
          ),

          # Ontology Tree
          h3("Hierarchical Structure", style = "color: #1a2332; margin-top: 40px; font-weight: 600;"),

          div(class = "ontology-tree", style = "
            background: #ffffff;
            padding: 30px;
            border-radius: 8px;
            border: 2px solid #e8dcc8;
            margin: 20px 0;
            font-family: 'Segoe UI', Arial, sans-serif;",

            # Root
            div(style = "margin-bottom: 25px;",
              div(style = "
                background: #1a2332;
                color: white;
                padding: 12px 20px;
                border-radius: 6px;
                font-weight: 600;
                font-size: 16px;
                display: inline-block;",
                "Self-directed biochemical processes"
              )
            ),

            # Tree with vertical line
            div(class = "ontology-tree-lines", style = "margin-left: 30px; border-left: 3px solid #d97742; padding-left: 0px;",

              # Branch 1
              div(style = "margin: 15px 0; position: relative;",
                div(style = "position: absolute; left: -3px; top: 12px; width: 25px; height: 3px; background: #d97742;"),
                div(class = "ontology-tree-node", style = "margin-left: 25px; background: #fef5f0; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #d97742;",
                  div(style = "font-weight: 600; color: #1a2332; margin-bottom: 6px; font-size: 15px;", "Self-catalytic chemistry"),
                  div(style = "margin-left: 15px; margin-top: 8px;",
                    tags$a(href = '#autocatalytic', class = 'tree-link', style = "font-size: 14px;", "→ Autocatalytic Reaction (+)")
                  )
                )
              ),

              # Branch 2
              div(style = "margin: 15px 0; position: relative;",
                div(style = "position: absolute; left: -3px; top: 12px; width: 25px; height: 3px; background: #d97742;"),
                div(class = "ontology-tree-node", style = "margin-left: 25px; background: #fef5f0; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #d97742;",
                  div(style = "font-weight: 600; color: #1a2332; margin-bottom: 6px; font-size: 15px;", "Protein self-modification (post-translational)"),
                  div(style = "margin-left: 15px; margin-top: 8px;",
                    div(tags$a(href = '#autophosphorylation', class = 'tree-link', style = "font-size: 14px;", "→ Autophosphorylation (+)")),
                    div(tags$a(href = '#autodephosphorylation', class = 'tree-link', style = "font-size: 14px; margin-top: 4px; display: block;", "→ Autodephosphorylation (–)")),
                    div(tags$a(href = '#autoacetylation', class = 'tree-link', style = "font-size: 14px; margin-top: 4px; display: block;", "→ Autoacetylation (+)")),
                    div(tags$a(href = '#autodemethylation', class = 'tree-link', style = "font-size: 14px; margin-top: 4px; display: block;", "→ Autodemethylation (±)")),
                    div(tags$a(href = '#autoubiquitination', class = 'tree-link', style = "font-size: 14px; margin-top: 4px; display: block;", "→ Autoubiquitination (–)"))
                  )
                )
              ),

              # Branch 3
              div(style = "margin: 15px 0; position: relative;",
                div(style = "position: absolute; left: -3px; top: 12px; width: 25px; height: 3px; background: #d97742;"),
                div(class = "ontology-tree-node", style = "margin-left: 25px; background: #fef5f0; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #d97742;",
                  div(style = "font-weight: 600; color: #1a2332; margin-bottom: 6px; font-size: 15px;", "Intrinsic regulatory control"),
                  div(style = "margin-left: 15px; margin-top: 8px;",
                    div(tags$a(href = '#autoregulation', class = 'tree-link', style = "font-size: 14px;", "→ Autoregulation of Gene Expression (±)")),
                    div(tags$a(href = '#autoinhibition', class = 'tree-link', style = "font-size: 14px; margin-top: 4px; display: block;", "→ Autoinhibition within Proteins (–)"))
                  )
                )
              ),

              # Branch 4
              div(style = "margin: 15px 0; position: relative;",
                div(style = "position: absolute; left: -3px; top: 12px; width: 25px; height: 3px; background: #d97742;"),
                div(class = "ontology-tree-node", style = "margin-left: 25px; background: #fef5f0; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #d97742;",
                  div(style = "font-weight: 600; color: #1a2332; margin-bottom: 6px; font-size: 15px;", "Self-degradation and lysis"),
                  div(style = "margin-left: 15px; margin-top: 8px;",
                    tags$a(href = '#autolysis', class = 'tree-link', style = "font-size: 14px;", "→ Autolysis (–)")
                  )
                )
              ),

              # Branch 5
              div(style = "margin: 15px 0; position: relative;",
                div(style = "position: absolute; left: -3px; top: 12px; width: 25px; height: 3px; background: #d97742;"),
                div(class = "ontology-tree-node", style = "margin-left: 25px; background: #fef5f0; padding: 10px 15px; border-radius: 6px; border-left: 4px solid #d97742;",
                  div(style = "font-weight: 600; color: #1a2332; margin-bottom: 6px; font-size: 15px;", "Population-level self-signaling"),
                  div(style = "margin-left: 15px; margin-top: 8px;",
                    tags$a(href = '#autoinducer', class = 'tree-link', style = "font-size: 14px;", "→ Autoinducer Molecules in Quorum Sensing (+)")
                  )
                )
              )
            )
          ),

          hr(style = "margin: 40px 0; border-top: 2px solid #e8dcc8;"),

          # Detailed Mechanism Descriptions
          h3("Mechanism Definitions with Ontology Relations and Citations", style = "color: #1a2332; font-weight: 600;"),

          # 1. Autocatalytic Reaction
          div(id = "autocatalytic", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("1. Autocatalytic Reaction (+)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "A chemical reaction in which its product or intermediate accelerates the same reaction. Supports nonlinear self-reinforcement and chemical self-organization."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-catalysis, autocatalysis, self-reinforcing reaction"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "heterocatalysis, enzyme-catalyzed"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "chemical oscillation, nonlinear dynamics")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " chemical reaction"),
                  tags$li(tags$b("has-input:"), " reaction substrates"),
                  tags$li(tags$b("has-output:"), " product that acts as catalyst"),
                  tags$li(tags$b("enables:"), " self-amplification in reaction networks"),
                  tags$li(tags$b("occurs-in:"), " biochemical and chemical systems")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Blackmond D. G. (2009). ", tags$i("Angewandte Chemie International Edition"), ", 48, 6092-6101."),
                tags$li("Plasson R., Brandenburg A., Jullien L., Bersini H. (2011). ", tags$i("Chemical Society Reviews"), ", 40, 2005-2018.")
              )
          ),

          # 2. Autophosphorylation
          div(id = "autophosphorylation", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #f0f2f5; border-radius: 8px; border-left: 5px solid #1a2332; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("2. Autophosphorylation (+)", style = "color: #1a2332; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "A protein kinase phosphorylates its own amino acid residues (cis or trans), tuning its conformation, catalytic activity, localization, and signaling dynamics."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-phosphorylation, cis-phosphorylation, intramolecular phosphorylation"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "dephosphorylation, heterophosphorylation"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "autokinase")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " protein self-modification event"),
                  tags$li(tags$b("part-of:"), " post-translational regulation"),
                  tags$li(tags$b("regulates:"), " kinase activity and activation state"),
                  tags$li(tags$b("has-input:"), " kinase + ATP"),
                  tags$li(tags$b("has-output:"), " phosphorylated kinase + ADP"),
                  tags$li(tags$b("occurs-in:"), " bacteria, plants, animals")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Reinhardt R. et al. (2023). ", tags$i("eLife"), ", 12, e88210."),
                tags$li("Xu Q. et al. (2015). ", tags$i("PNAS"), ", 112, 11753-11758.")
              )
          ),

          # 3. Autodephosphorylation
          div(id = "autodephosphorylation", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("3. Autodephosphorylation (–)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "The process by which a protein removes phosphate groups from itself, typically through intrinsic phosphatase activity or conformational changes that reverse autophosphorylation."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-dephosphorylation, autophosphatase activity, intrinsic phosphatase"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "autophosphorylation, heterophosphatase"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "autophosphorylation, phosphatase activity")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " protein self-modification event"),
                  tags$li(tags$b("part-of:"), " post-translational regulation"),
                  tags$li(tags$b("regulates:"), " phosphatase activity and deactivation"),
                  tags$li(tags$b("has-input:"), " phosphorylated protein"),
                  tags$li(tags$b("has-output:"), " dephosphorylated protein + Pi"),
                  tags$li(tags$b("related-to:"), " autophosphorylation (reverse process)")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Related to intrinsic phosphatase activity in dual-function kinases")
              )
          ),

          # 4. Autoacetylation
          div(id = "autoacetylation", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #f0f2f5; border-radius: 8px; border-left: 5px solid #1a2332; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("4. Autoacetylation (+)", style = "color: #1a2332; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "The process where an acetyltransferase acetylates itself, adding acetyl groups to its own lysine residues, often affecting protein activity or stability."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-acetylation, cis-acetylation, intrinsic acetyltransferase activity"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "deacetylation, heteroacetylation"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "histone modification, lysine acetylation, protein acetylation")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " protein self-modification event"),
                  tags$li(tags$b("part-of:"), " post-translational regulation"),
                  tags$li(tags$b("regulates:"), " acetyltransferase activity and protein function"),
                  tags$li(tags$b("has-input:"), " acetyltransferase + acetyl-CoA"),
                  tags$li(tags$b("has-output:"), " acetylated protein + CoA"),
                  tags$li(tags$b("related-to:"), " histone modification, lysine acetylation")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Related to intrinsic acetyltransferase activity")
              )
          ),

          # 5. Autodemethylation
          div(id = "autodemethylation", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("5. Autodemethylation (±)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "The process by which a protein removes methyl groups from itself through intrinsic demethylase activity, reversing the effects of methylation."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-demethylation, intrinsic demethylase activity"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "automethylation, heteromethylation"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "protein methylation, demethylase activity, epigenetic regulation")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " protein self-modification event"),
                  tags$li(tags$b("part-of:"), " post-translational regulation"),
                  tags$li(tags$b("regulates:"), " demethylase activity and epigenetic state"),
                  tags$li(tags$b("has-input:"), " methylated protein"),
                  tags$li(tags$b("has-output:"), " demethylated protein"),
                  tags$li(tags$b("related-to:"), " protein methylation, epigenetic regulation")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Related to intrinsic demethylase activity")
              )
          ),

          # 6. Autoubiquitination
          div(id = "autoubiquitination", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("6. Autoubiquitination (–)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "An E3 ubiquitin ligase attaches ubiquitin to itself, altering its stability, proteasomal targeting, and signaling functions depending on chain type and site."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-ubiquitination, cis-ubiquitination, auto-ubiquitylation"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "deubiquitination, heteroubiquitination"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "protein degradation")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " protein self-ubiquitylation"),
                  tags$li(tags$b("part-of:"), " ubiquitin–proteasome regulatory system"),
                  tags$li(tags$b("regulates:"), " E3 ligase abundance and pathway output"),
                  tags$li(tags$b("has-input:"), " E3 ligase + ubiquitin + E2 enzyme"),
                  tags$li(tags$b("has-output:"), " ubiquitylated E3 ligase"),
                  tags$li(tags$b("occurs-in:"), " eukaryotic cells")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Buetow L., Huang D. T. (2021). ", tags$i("International Journal of Molecular Sciences"), ", 22, 6057."),
                tags$li("Nityanandam R. et al. (2011). ", tags$i("BMC Biochemistry"), ", 12, 33.")
              )
          ),

          # 4. Autoregulation
          div(id = "autoregulation", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #f0f2f5; border-radius: 8px; border-left: 5px solid #1a2332; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("4. Autoregulation of Gene Expression (±)", style = "color: #1a2332; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "A gene product regulates transcription of the same gene through positive or negative feedback, tuning gene expression dynamics, noise, and homeostasis."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-regulation, autonomous regulation, homeostatic control"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "heteroregulation, external regulation"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "homeostasis, feedback control")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " gene regulatory feedback motif"),
                  tags$li(tags$b("part-of:"), " transcriptional regulatory network"),
                  tags$li(tags$b("regulates:"), " mRNA abundance and response time"),
                  tags$li(tags$b("has-input:"), " gene product (protein or RNA)"),
                  tags$li(tags$b("has-output:"), " altered transcription rate"),
                  tags$li(tags$b("occurs-in:"), " bacteria, yeast, multicellular organisms")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Alon U. (2007). ", tags$i("Nature Reviews Genetics"), ", 8, 450-461."),
                tags$li("Becskei A., Serrano L. (2000). ", tags$i("Nature"), ", 405, 590-593.")
              )
          ),

          # 5. Autoinducer
          div(id = "autoinducer", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("5. Autoinducer Molecules in Quorum Sensing (+)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Small diffusible molecules synthesized and detected by bacteria. Their accumulation with increasing cell density triggers coordinated community-wide transcriptional changes."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-induction, auto-stimulation, positive feedback induction"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "autorepression, negative feedback"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "autoactivation, positive feedback")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " extracellular signaling molecule"),
                  tags$li(tags$b("part-of:"), " quorum-sensing signal-response system"),
                  tags$li(tags$b("regulates:"), " group-level behavior and virulence"),
                  tags$li(tags$b("has-input:"), " bacterial metabolic pathways"),
                  tags$li(tags$b("has-output:"), " receptor activation and transcriptional changes"),
                  tags$li(tags$b("occurs-in:"), " bacterial populations")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Papenfort K., Bassler B. L. (2016). ", tags$i("Nature Reviews Microbiology"), ", 14, 576-588."),
                tags$li("Mukherjee S., Bassler B. L. (2019). ", tags$i("Nature Reviews Microbiology"), ", 17, 371-382.")
              )
          ),

          # 6. Autoinhibition
          div(id = "autoinhibition", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #f0f2f5; border-radius: 8px; border-left: 5px solid #1a2332; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("6. Autoinhibition within Proteins (–)", style = "color: #1a2332; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Intrinsic structural interactions prevent inappropriate activation of a protein, maintaining it in an inactive state until relieved by ligand binding, structural rearrangement, or post-translational modification."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-inhibition, negative autoregulation, self-repression"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "autoactivation, positive autoregulation"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "negative feedback, autorepression")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " intrinsic negative regulatory process"),
                  tags$li(tags$b("part-of:"), " protein activity control"),
                  tags$li(tags$b("regulates:"), " activation threshold and specificity"),
                  tags$li(tags$b("has-input:"), " inactive protein structure"),
                  tags$li(tags$b("has-output:"), " relieved inhibition and activation"),
                  tags$li(tags$b("occurs-in:"), " multi-domain signaling proteins")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Fenton M. et al. (2023). ", tags$i("Cell Reports"), ", 42, 112739."),
                tags$li("Khan R. B. et al. (2019). ", tags$i("Frontiers in Molecular Biosciences"), ", 6, 144.")
              )
          ),

          # 7. Autolysis
          div(id = "autolysis", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef5f0; border-radius: 8px; border-left: 5px solid #d97742; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("7. Autolysis (–)", style = "color: #d97742; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Self-degradation mediated by endogenous lytic enzymes, occurring during programmed cell death, post-mortem breakdown, or engineered microbial lysis systems."),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Synonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "self-cleavage, autocatalytic cleavage, self-proteolysis"),
                h5("Antonyms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0 0 12px 0; padding-left: 10px; color: #666; font-size: 14px;", "heterolysis, trans-cleavage"),
                h5("Related Terms", style = "color: #2c3e50; font-size: 14px; margin-bottom: 8px;"),
                p(style = "margin: 0; padding-left: 10px; color: #666; font-size: 14px;", "autocatalysis")
              ),

              div(style = "background: #ffffff; padding: 15px; border-radius: 6px; margin: 15px 0; border: 1px solid #f0f0f0;",
                h5("Core Ontology Relations", style = "color: #2c3e50; font-size: 14px; margin-bottom: 10px;"),
                tags$ul(style = "margin: 0; padding-left: 20px; color: #555;",
                  tags$li(tags$b("is-a:"), " lytic cell death mechanism"),
                  tags$li(tags$b("part-of:"), " cellular degradation process"),
                  tags$li(tags$b("regulates:"), " release of intracellular contents"),
                  tags$li(tags$b("has-input:"), " endogenous hydrolases"),
                  tags$li(tags$b("has-output:"), " membrane rupture and component release"),
                  tags$li(tags$b("occurs-in:"), " microbes, tissues, postmortem environments")
                )
              ),

              p(tags$b("Key References:"), style = "color: #2c3e50; margin-top: 15px;"),
              tags$ul(style = "margin: 5px 0; padding-left: 20px; color: #666; font-size: 14px;",
                tags$li("Yamaguchi Y., Nariya H., Inouye M. (2013). ", tags$i("Applied and Environmental Microbiology"), ", 79, 3120-3126."),
                tags$li("Pérez-Torrado R. et al. (2015). ", tags$i("Comprehensive Reviews in Food Science and Food Safety"), ", 14, 726-743.")
              )
          ),

          hr(style = "margin: 40px 0; border-top: 2px solid #e0e0e0;"),

          # Classification Pipeline - professional colors
      )
    )
  ),



  # Tab: Updates
  tabPanel(
    title = "Updates",
    fluidPage(
      header_ui,
      h2("Updates"),
      withSpinner(
        DT::dataTableOutput("patch_notes_table"),
        type = 6,
        color = "#2c3e50",
        size = 1.5
      )
    )
  ),

  # Tab: About Us
  tabPanel(
    title = "About Us",
    fluidPage(class = "about-page",
      header_ui,

      # Page Title
      div(class = "about-hero",
        style = "text-align: center; margin: 40px 0 60px 0;",
        h1("About SOORENA", style = "font-size: 2.5em; font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
        p("Self-lOOp containing or autoREgulatory Nodes in biological network Analysis",
          style = "font-size: 1.2em; color: #7f8c8d; font-style: italic;")
      ),

      # Project Description
	      div(class = "about-description",
	        style = "max-width: 900px; margin: 0 auto 60px auto; padding: 0 20px;",
	        p("SOORENA is a comprehensive database for exploring autoregulatory mechanisms in proteins.
	          Our machine learning-powered platform analyzes millions of scientific publications to identify
	          and classify autoregulatory protein mechanisms, providing researchers with unprecedented access
	          to this critical biological information.",
	          style = "font-size: 1.1em; line-height: 1.8; color: #34495e; text-align: center;")
	        ,
	        p(
	          tags$a(
	            href = "https://www.biorxiv.org/content/10.1101/2025.11.03.685842v1",
	            target = "_blank",
	            "Read the BioRxiv manuscript"
	          ),
	          style = "font-size: 1.05em; line-height: 1.6; color: #34495e; text-align: center;"
	        )
	      ),

      # Team Section Header
      div(class = "about-team-header",
        style = "text-align: center; margin: 60px 0 40px 0;",
        h2("Project Contributors", style = "font-size: 2em; font-weight: 600; color: #2c3e50; margin-bottom: 10px;"),
        div(style = "width: 80px; height: 4px; background: #3498db; margin: 0 auto;")
      ),

      # Team Members Grid
      div(class = "about-team-grid",
        style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",

        # Row 1: Hala and Mohieddin
        div(class = "team-grid",
          style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 40px; margin-bottom: 40px;",

          # Hala Arar
          div(class = "team-card",
            style = "flex: 0 1 280px; text-align: center; background: white; padding: 30px 20px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); transition: transform 0.3s;",
            div(
              style = "width: 180px; height: 180px; margin: 0 auto 20px auto; border-radius: 50%; overflow: hidden; border: 4px solid #3498db;",
              tags$img(src = "images/team/hala_arar.jpg",
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      alt = "Hala Arar")
            ),
            h3("Hala Arar", style = "font-size: 1.4em; font-weight: 600; color: #2c3e50; margin: 15px 0 10px 0;"),
            p("Department of Statistics", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("University of British Columbia", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("Vancouver, BC, Canada", style = "font-size: 0.9em; color: #95a5a6; margin: 5px 0;")
          ),

          # Mohieddin Jafari
          div(class = "team-card",
            style = "flex: 0 1 280px; text-align: center; background: white; padding: 30px 20px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); transition: transform 0.3s;",
            div(
              style = "width: 180px; height: 180px; margin: 0 auto 20px auto; border-radius: 50%; overflow: hidden; border: 4px solid #3498db;",
              tags$img(src = "images/team/mohieddin_jafari.jpg",
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      alt = "Mohieddin Jafari")
            ),
            h3("Mohieddin Jafari", style = "font-size: 1.4em; font-weight: 600; color: #2c3e50; margin: 15px 0 10px 0;"),
            p("Department of Biochemistry and Developmental Biology", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("University of Helsinki, Finland", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("Faculty of Medicine and Health Technology, Tampere University, Finland", style = "font-size: 0.9em; color: #95a5a6; margin: 5px 0;"),
            p("Tampere Institute for Advanced Study, Finland", style = "font-size: 0.9em; color: #95a5a6; margin: 5px 0;")
          )
        ),

        # Row 2: Payman and Jehad
        div(class = "team-grid",
          style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 40px;",

          # Payman Nickchi
          div(class = "team-card",
            style = "flex: 0 1 280px; text-align: center; background: white; padding: 30px 20px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); transition: transform 0.3s;",
            div(
              style = "width: 180px; height: 180px; margin: 0 auto 20px auto; border-radius: 50%; overflow: hidden; border: 4px solid #3498db;",
              tags$img(src = "images/team/payman_nickchi.jpg",
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      alt = "Payman Nickchi")
            ),
            h3("Payman Nickchi", style = "font-size: 1.4em; font-weight: 600; color: #2c3e50; margin: 15px 0 10px 0;"),
            p("Department of Statistics", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("University of British Columbia", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("Vancouver, BC, Canada", style = "font-size: 0.9em; color: #95a5a6; margin: 5px 0;")
          ),

          # Jehad Aldahdooh
          div(class = "team-card",
            style = "flex: 0 1 280px; text-align: center; background: white; padding: 30px 20px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); transition: transform 0.3s;",
            div(
              style = "width: 180px; height: 180px; margin: 0 auto 20px auto; border-radius: 50%; overflow: hidden; border: 4px solid #3498db;",
              tags$img(src = "images/team/jehad_aldahdooh.jpg",
                      style = "width: 100%; height: 100%; object-fit: cover;",
                      alt = "Jehad Aldahdooh")
            ),
            h3("Jehad Aldahdooh", style = "font-size: 1.4em; font-weight: 600; color: #2c3e50; margin: 15px 0 10px 0;"),
            p("Research Programs Unit", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("University of Helsinki", style = "font-size: 0.95em; color: #7f8c8d; margin: 5px 0;"),
            p("Helsinki, Finland", style = "font-size: 0.9em; color: #95a5a6; margin: 5px 0;")
          )
        )
      ),

      # Acknowledgements Section
      div(
        style = "max-width: 900px; margin: 80px auto 40px auto; padding: 30px; background: #f8f9fa; border-radius: 10px;",
        h3("Acknowledgements", style = "font-size: 1.6em; font-weight: 600; color: #2c3e50; margin-bottom: 20px; text-align: center;"),
        p("We would like to thank the following individuals for their valuable contributions to this project:",
          style = "font-size: 1.05em; color: #34495e; text-align: center; margin-bottom: 15px;"),
        div(
          style = "text-align: center; font-size: 1.05em; color: #7f8c8d; line-height: 1.8;",
          p("Zheng He • Yining Zhou • Mingyang Zhang", style = "margin: 10px 0;")
        )
      ),

      # Footer spacing
      div(style = "height: 60px;")
    )
  )
)


# Define Server Logic
server <- function(input, output, session) {

  # Open a per-session database connection
  conn <- dbConnect(RSQLite::SQLite(), DB_PATH)
  db_columns <- dbGetQuery(conn, "PRAGMA table_info(predictions)")$name
  db_has_polarity <- "Polarity" %in% db_columns

  # Track current page for server-side pagination
  current_page <- reactiveVal(1)
  table_sort <- reactiveVal(NULL)
  page_size <- reactive({
    size <- as.numeric(input$rows_per_page)
    if (is.null(size) || is.na(size)) DEFAULT_PAGE_SIZE else size
  })

  # Store minimum probability values for filter comparison
  min_mechanism_probability <- reactiveVal(0)
  min_type_confidence <- reactiveVal(0)

  is_mobile_output <- function(output_id) {
    width <- session$clientData[[paste0("output_", output_id, "_width")]]
    if (is.null(width)) {
      width <- session$clientData$client_width
    }
    !is.null(width) && width < 520
  }

  truncate_label <- function(x, max_chars = 20) {
    x <- as.character(x)
    ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
  }

  apply_plotly_config <- function(p, is_mobile) {
    plotly::config(p, displayModeBar = !is_mobile, displaylogo = FALSE, responsive = TRUE)
  }

  # Build WHERE clause and parameter list based on current filters
		  build_filter_query <- function() {
		    query <- "FROM predictions WHERE 1=1"
		    params <- list()
		    match_mode <- if (isTRUE(input$match_exact)) "exact" else "contains"

    # Journal filter
    if (!is.null(input$journal) && length(input$journal) > 0) {
      placeholders <- paste(rep("?", length(input$journal)), collapse = ",")
      query <- paste(query, "AND Journal IN (", placeholders, ")")
      params <- c(params, as.list(input$journal))
    }

    # Type filter
    if (!is.null(input$type) && length(input$type) > 0) {
      placeholders <- paste(rep("?", length(input$type)), collapse = ",")
      query <- paste(query, "AND Autoregulatory_Type IN (", placeholders, ")")
      params <- c(params, as.list(input$type))
    }

    # Polarity filter (optional).
    # When all polarity options are selected, treat it as "no filter" for clarity/performance.
    if (!is.null(input$polarity) && length(input$polarity) > 0) {
      selected <- as.character(input$polarity)
      selected <- selected[!is.na(selected) & trimws(selected) != ""]

      # Normalize "-" to the en-dash used in the UI.
      selected[selected == "-"] <- "–"

      all_polarities <- c("+", "–", "±")
      if (length(selected) > 0 && !setequal(selected, all_polarities)) {
        if (db_has_polarity) {
          placeholders <- paste(rep("?", length(selected)), collapse = ",")
          query <- paste(query, "AND Polarity IN (", placeholders, ")")
          params <- c(params, as.list(selected))
        } else {
          # Backwards-compatible fallback for older DBs without a Polarity column.
          pol_conditions <- c()
          if ("+" %in% selected) {
            pol_conditions <- c(pol_conditions, "lower(Autoregulatory_Type) IN ('autocatalytic','autophosphorylation','autoinducer')")
          }
          if ("–" %in% selected) {
            pol_conditions <- c(pol_conditions, "lower(Autoregulatory_Type) IN ('autoinhibition','autoubiquitination','autolysis')")
          }
          if ("±" %in% selected) {
            pol_conditions <- c(pol_conditions, "lower(Autoregulatory_Type) IN ('autoregulation')")
          }
          if (length(pol_conditions) > 0) {
            query <- paste(query, "AND (", paste(pol_conditions, collapse = " OR "), ")")
          }
        }
      }
    }

    # OS filter
    if (!is.null(input$os) && length(input$os) > 0) {
      placeholders <- paste(rep("?", length(input$os)), collapse = ",")
      query <- paste(query, "AND OS IN (", placeholders, ")")
      params <- c(params, as.list(input$os))
    }

	    # UniProtKB accessions search
	    if (!is.null(input$ac) && nzchar(input$ac)) {
	      value <- trimws(input$ac)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND (',' || REPLACE(UPPER(UniProtKB_accessions), ' ', '') || ',') LIKE ?")
	        params <- c(params, paste0("%,", toupper(gsub(" ", "", value)), ",%"))
	      } else {
	        query <- paste(query, "AND UPPER(UniProtKB_accessions) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # Record AC (unique per-row ID) search
	    if (!is.null(input$record_ac) && nzchar(input$record_ac)) {
	      value <- trimws(input$record_ac)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND AC = ?")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND UPPER(AC) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # Protein ID search
	    if (!is.null(input$protein_id) && nzchar(input$protein_id)) {
	      value <- trimws(input$protein_id)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND Protein_ID = ? COLLATE NOCASE")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND UPPER(Protein_ID) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # Protein Name search
	    if (!is.null(input$protein_name) && nzchar(input$protein_name)) {
	      value <- trimws(input$protein_name)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND Protein_Name = ? COLLATE NOCASE")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND UPPER(Protein_Name) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # Gene Name search
	    if (!is.null(input$gene_name) && nzchar(input$gene_name)) {
	      value <- trimws(input$gene_name)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND Gene_Name = ? COLLATE NOCASE")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND UPPER(Gene_Name) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # PMID search
	    if (!is.null(input$pmid) && nzchar(input$pmid)) {
	      value <- trimws(input$pmid)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND PMID = ?")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND PMID LIKE ?")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

	    # Author search
	    if (!is.null(input$author) && nzchar(input$author)) {
	      value <- trimws(input$author)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND Authors = ? COLLATE NOCASE")
	        params <- c(params, value)
	      } else {
	        query <- paste(query, "AND UPPER(Authors) LIKE UPPER(?)")
	        params <- c(params, paste0("%", value, "%"))
	      }
	    }

    # Source filter (segmented control)
	    if (!is.null(input$source_mode) && nzchar(input$source_mode) && input$source_mode != "all") {
	      query <- paste(query, "AND Source = ? COLLATE NOCASE")
	      params <- c(params, input$source_mode)
	    }

    # Year range filter (optional)
    year_from <- NA_integer_
    year_to <- NA_integer_
    if (!is.null(input$year_from) && nzchar(input$year_from)) {
      year_from <- suppressWarnings(as.integer(trimws(input$year_from)))
    }
    if (!is.null(input$year_to) && nzchar(input$year_to)) {
      year_to <- suppressWarnings(as.integer(trimws(input$year_to)))
    }
    if (!is.na(year_from) && !is.na(year_to) && year_from > year_to) {
      tmp <- year_from
      year_from <- year_to
      year_to <- tmp
    }
	    if (!is.na(year_from)) {
	      query <- paste(query, "AND CAST(Year AS INTEGER) >= ?")
	      params <- c(params, year_from)
	    }
	    if (!is.na(year_to)) {
	      query <- paste(query, "AND CAST(Year AS INTEGER) <= ?")
	      params <- c(params, year_to)
	    }

    # Month filter
    if (!is.null(input$month) && length(input$month) > 0) {
      placeholders <- paste(rep("?", length(input$month)), collapse = ",")
      query <- paste(query, "AND Month IN (", placeholders, ")")
      params <- c(params, as.list(input$month))
    }

	    # Title / Abstract search
	    if (!is.null(input$search) && nzchar(input$search)) {
	      value <- trimws(input$search)
	      if (match_mode == "exact") {
	        query <- paste(query, "AND (Title = ? COLLATE NOCASE OR Abstract = ? COLLATE NOCASE)")
	        params <- c(params, value, value)
	      } else {
	        query <- paste(query, "AND (UPPER(Title) LIKE UPPER(?) OR UPPER(Abstract) LIKE UPPER(?))")
	        search_pattern <- paste0("%", value, "%")
	        params <- c(params, search_pattern, search_pattern)
	      }
	    }

	    # Mechanism Probability threshold (apply only if above minimum)
	    if (!is.null(input$min_mechanism_prob) && input$min_mechanism_prob > min_mechanism_probability()) {
	      query <- paste(query, "AND Mechanism_Probability >= ?")
	      params <- c(params, input$min_mechanism_prob)
	    }

	    # Type Confidence threshold (apply only if above minimum)
	    if (!is.null(input$min_type_conf) && input$min_type_conf > min_type_confidence()) {
	      query <- paste(query, "AND Type_Confidence >= ?")
	      params <- c(params, input$min_type_conf)
	    }

    list(where = query, params = params)
  }

  # Full value, escaped, never truncated. Used for fields short enough to
  # display in their entirety (see field-length survey in the commit message).
  plain_cell <- function(text) {
    vapply(text, function(val) {
      if (is.na(val) || trimws(val) == "" || trimws(val) == "Unknown") return("")
      htmltools::htmlEscape(trimws(val))
    }, character(1), USE.NAMES = FALSE)
  }

  # Truncated, with the full value revealed on mouseover via the native title
  # tooltip. No click, no pop-up. Used for the multi-valued fields, which can
  # be thousands of characters long when several values are joined.
  tooltip_cell <- function(text, max_chars) {
    vapply(text, function(val) {
      if (is.na(val) || trimws(val) == "" || trimws(val) == "Unknown") return("")
      trimmed <- trimws(val)
      if (nchar(trimmed) <= max_chars) return(htmltools::htmlEscape(trimmed))
      paste0(
        '<span class="truncated-cell" title="',
        htmltools::htmlEscape(trimmed, attribute = TRUE), '">',
        htmltools::htmlEscape(substr(trimmed, 1, max_chars)), "\u2026</span>"
      )
    }, character(1), USE.NAMES = FALSE)
  }

  # Simple helper to safely truncate and escape text while keeping the magnifier button
  # Now stores AC identifier instead of full text to avoid HTML attribute length limits
  safe_cell <- function(text, max_chars, field, row_ids = NULL, label = "Show more") {
    vapply(seq_along(text), function(i) {
      val <- text[i]
      if (is.na(val) || trimws(val) == "" || trimws(val) == "Unknown") return("")
      trimmed <- trimws(val)
      escaped_full <- htmltools::htmlEscape(trimmed)
      if (nchar(trimmed) > max_chars) {
        truncated <- htmltools::htmlEscape(substr(trimmed, 1, max_chars))
        # Store row AC identifier instead of full text to avoid browser attribute length limits
        row_id <- if (!is.null(row_ids)) row_ids[i] else ""
        paste0(
          truncated, "... ",
          '<button class=\"btn btn-link btn-sm view-btn\" data-field=\"', field,
          '\" data-row-id=\"', row_id, '\">', label, '</button>'
        )
      } else {
        escaped_full
      }
    }, character(1))
  }

  # Populate filter dropdowns once at startup with top N most common values
  # This prevents loading thousands of options which causes app freeze
  observeEvent(session$clientData, once = TRUE, {
    # Load top 100 most common journals
    top_journals <- dbGetQuery(conn,
      "SELECT Journal, COUNT(*) as n FROM predictions
       WHERE Journal IS NOT NULL
       GROUP BY Journal
       ORDER BY n DESC
       LIMIT 100")$Journal

    updateSelectizeInput(session, "journal",
      choices = top_journals,
      server = FALSE)

    # Load top 100 most common organisms (OS has 9000+ unique values!)
    top_os <- dbGetQuery(conn,
      "SELECT OS, COUNT(*) as n FROM predictions
       WHERE OS IS NOT NULL
       GROUP BY OS
       ORDER BY n DESC
       LIMIT 100")$OS

    updateSelectizeInput(session, "os",
      choices = top_os,
      server = FALSE)

    # Load all autoregulatory types (only ~10 values)
    all_types <- dbGetQuery(conn,
      "SELECT DISTINCT Autoregulatory_Type FROM predictions
       WHERE Autoregulatory_Type IS NOT NULL
       ORDER BY Autoregulatory_Type")$Autoregulatory_Type

    updateSelectizeInput(session, "type",
      choices = all_types,
      server = FALSE)

		    # Load all years (only ~50 values)
		    all_years <- dbGetQuery(conn,
		      "SELECT DISTINCT Year FROM predictions
		       WHERE Year IS NOT NULL
		       ORDER BY Year DESC")$Year

		    year_nums <- suppressWarnings(as.integer(as.character(all_years)))
			    year_nums <- year_nums[!is.na(year_nums) & year_nums >= 1800 & year_nums <= (as.integer(format(Sys.Date(), "%Y")) + 1)]
		    year_nums <- sort(unique(year_nums), decreasing = TRUE)
		    year_choices <- c("", as.character(year_nums))
		    updateSelectizeInput(session, "year_from",
		      choices = year_choices,
		      selected = "",
		      server = FALSE)
    updateSelectizeInput(session, "year_to",
      choices = year_choices,
      selected = "",
      server = FALSE)

    # Get min/max values for probability sliders
    prob_range <- dbGetQuery(conn,
      "SELECT
         MIN(Mechanism_Probability) as min_mech,
         MAX(Mechanism_Probability) as max_mech,
         MIN(Type_Confidence) as min_type,
         MAX(Type_Confidence) as max_type
       FROM predictions
       WHERE Mechanism_Probability IS NOT NULL
         AND Type_Confidence IS NOT NULL")

    if (nrow(prob_range) > 0) {
      # Round to 2 decimal places for cleaner display
      min_mech <- floor(prob_range$min_mech * 100) / 100
      max_mech <- ceiling(prob_range$max_mech * 100) / 100
      min_type <- floor(prob_range$min_type * 100) / 100
      max_type <- ceiling(prob_range$max_type * 100) / 100

      # Store minimum values for filter comparison
      min_mechanism_probability(min_mech)
      min_type_confidence(min_type)

      # Update slider ranges with actual data bounds
      updateSliderInput(session, "min_mechanism_prob",
                       min = min_mech,
                       max = max_mech,
                       value = min_mech)
      updateSliderInput(session, "min_type_conf",
                       min = min_type,
                       max = max_type,
                       value = min_type)
    }
  })

  observeEvent(input$result_table_order, {
    ord <- input$result_table_order
    if (is.null(ord) || length(ord) == 0) {
      if (!is.null(table_sort())) {
        table_sort(NULL)
        current_page(1)
      }
      return()
    }

    first <- ord[[1]]
    if (is.null(first) || length(first) < 2) return()

    idx <- suppressWarnings(as.integer(first[[1]]))
    dir <- as.character(first[[2]])
    if (is.na(idx) || is.null(dir) || !nzchar(dir)) {
      if (!is.null(table_sort())) {
        table_sort(NULL)
        current_page(1)
      }
      return()
    }
    if (!(dir %in% c("asc", "desc"))) return()

    prev <- table_sort()
    if (!is.null(prev) && isTRUE(prev$idx == idx) && isTRUE(prev$dir == dir)) return()

    table_sort(list(idx = idx, dir = dir))
    current_page(1)
  }, ignoreInit = TRUE)

  # Reset pagination whenever filters change
  observeEvent(list(input$journal, input$type, input$polarity, input$os, input$ac, input$record_ac, input$protein_id,
                    input$protein_name, input$gene_name, input$pmid, input$author,
                    input$source_mode, input$year_from, input$year_to, input$month, input$search, input$match_exact, input$rows_per_page), {
    current_page(1)
  })

  # Download csv button
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("filtered_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_data()
      # Keep the download consistent with the visible table (no Has Mechanism column).
      keep <- c(
        "AC",
        "PMID",
        "UniProt AC",
        "Autoregulatory Type",
        "Polarity",
        "Mechanism Probability",
        "Type Confidence",
        "Title",
        "Abstract",
        "Journal",
        "Authors",
        "Year",
        "Month",
        "Source",
        "Protein Name",
        "Gene Name",
        "Protein ID",
        "Organism"
      )
      keep <- keep[keep %in% colnames(data)]
      data <- data[, keep, drop = FALSE]
      # Convert underscores back to spaces for column names
      colnames(data) <- gsub("_", " ", colnames(data))
      write.csv(data, file, row.names = FALSE)
    }
  )


  # Show Full text - query database by AC to avoid HTML attribute length limits
    observeEvent(input$show_full_text, {
      req(input$show_full_text$field)

      field_display <- input$show_full_text$field

      # Special handling for Autoregulatory Type with pre-rendered ontology text
      if (field_display == "Autoregulatory Type" && !is.null(input$show_full_text$text) && input$show_full_text$text != "") {
        showModal(modalDialog(
          title = "Ontology Details",
          HTML(paste0("<div style='font-family: sans-serif; line-height: 1.6;'>",
                      input$show_full_text$text, "</div>")),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        ))
        return()
      }

      # Regular handling for other fields that need database lookup
      req(input$show_full_text$row_id)

      # Map display field names to database column names
      field_map <- c(
        "AC" = "AC",
        "Title" = "Title",
        "Abstract" = "Abstract",
        "Journal" = "Journal",
        "Authors" = "Authors",
        "Protein Name" = "Protein_Name",
        "Gene Name" = "Gene_Name",
        "Protein ID" = "Protein_ID",
        "Organism" = "OS",
        "UniProt AC" = "UniProtKB_accessions"
      )

      field_db <- field_map[[field_display]]

      if (is.null(field_db)) {
        field_db <- field_display  # Fallback if not in map
      }

      # Query database for full text by AC
      query <- sprintf("SELECT %s FROM predictions WHERE AC = ?", field_db)
      result <- dbGetQuery(conn, query, params = list(input$show_full_text$row_id))

      full_text <- if (nrow(result) > 0 && !is.na(result[[1]][1])) {
        as.character(result[[1]][1])
      } else {
        "(No data available)"
      }

      showModal(modalDialog(
        title = paste("Full", field_display),
        HTML(paste0("<div style='white-space: pre-wrap; font-family: sans-serif;'>",
                    htmltools::htmlEscape(full_text), "</div>")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m"
      ))
    })

		  # Reset all filters to default state
			  observeEvent(input$reset_filters, {
			    updateTextInput(session, "protein_id", value = "")
			    updateTextInput(session, "protein_name", value = "")
			    updateTextInput(session, "gene_name", value = "")
			    updateTextInput(session, "ac", value = "")
			    updateTextInput(session, "record_ac", value = "")
			    updateTextInput(session, "pmid", value = "")
			    updateTextInput(session, "author", value = "")
			    updateSelectizeInput(session, "journal", selected = character(0))
			    updateSelectizeInput(session, "os", selected = character(0))
			    updateSelectizeInput(session, "type", selected = character(0))
			    updateCheckboxGroupInput(session, "polarity", selected = c("+", "–", "±"))
			    updateSelectInput(session, "source_mode", selected = "all")
			    updateSelectizeInput(session, "year_from", selected = "")
			    updateSelectizeInput(session, "year_to", selected = "")
			    updateSelectizeInput(session, "month", selected = character(0))
			    updateCheckboxInput(session, "match_exact", value = FALSE)
			    updateTextInput(session, "search", value = "")
			    updateSliderInput(session, "min_mechanism_prob", value = min_mechanism_probability())
			    updateSliderInput(session, "min_type_conf", value = min_type_confidence())
			    table_sort(NULL)
			    current_page(1)
			  })

	  observeEvent(input$polarity, {
	    selected <- input$polarity
	    if (is.null(selected) || length(selected) == 0) {
	      updateCheckboxGroupInput(session, "polarity", selected = c("+", "–", "±"))
	    }
	  }, ignoreInit = TRUE)

	  observeEvent(input$polarity_help, {
	    showModal(modalDialog(
	      title = "Polarity",
	      div(
	        p("Polarity is derived from the inferred autoregulatory mechanism:"),
	        tags$ul(
	          tags$li(tags$b("+"), " positive / self-amplifying (e.g., autocatalysis, autophosphorylation, autoinduction)"),
	          tags$li(tags$b("–"), " negative / self-limiting (e.g., autoinhibition, autoubiquitination, autolysis)"),
	          tags$li(tags$b("±"), " context-dependent (e.g., autoregulation)")
	        ),
	        p(style = "margin-top: 10px;",
	          "Tip: If you want to see everything, leave all three selected (default)."
	        )
	      ),
	      easyClose = TRUE,
	      footer = modalButton("Close"),
	      size = "m"
	    ))
	  })

	  # Get total count of matching rows (for "Showing X of Y" display)
	  total_count <- reactive({
	    filters <- build_filter_query()
	    query <- paste("SELECT COUNT(*) as count", filters$where)

    if (length(filters$params) > 0) {
      count_result <- dbGetQuery(conn, query, params = filters$params)
    } else {
      count_result <- dbGetQuery(conn, query)
    }

    return(count_result$count)
  })

  output$match_count_badge <- renderText({
    total <- as.numeric(total_count()[1])
    if (is.na(total)) total <- 0
    paste0(format(total, big.mark = ","), " matches")
  })

  output$more_filters_title <- renderText({
    active <- 0

    if (!is.null(input$journal) && length(input$journal) > 0) active <- active + 1
    if (!is.null(input$os) && length(input$os) > 0) active <- active + 1
    if (!is.null(input$month) && length(input$month) > 0) active <- active + 1

    if (!is.null(input$author) && nzchar(trimws(input$author))) active <- active + 1
    if (!is.null(input$protein_name) && nzchar(trimws(input$protein_name))) active <- active + 1
    if (!is.null(input$gene_name) && nzchar(trimws(input$gene_name))) active <- active + 1
    if (!is.null(input$protein_id) && nzchar(trimws(input$protein_id))) active <- active + 1
    if (!is.null(input$pmid) && nzchar(trimws(input$pmid))) active <- active + 1
    if (!is.null(input$ac) && nzchar(trimws(input$ac))) active <- active + 1
    if (!is.null(input$record_ac) && nzchar(trimws(input$record_ac))) active <- active + 1

    if (!is.null(input$min_mechanism_prob) && input$min_mechanism_prob > min_mechanism_probability()) active <- active + 1
    if (!is.null(input$min_type_conf) && input$min_type_conf > min_type_confidence()) active <- active + 1

    if (active > 0) {
      paste0("More filters (", active, ")")
    } else {
      "More filters"
    }
  })

	  # Filtering Logic - Build SQL query dynamically with LIMIT
		  filtered_data <- reactive({
		    filters <- build_filter_query()
		    offset <- (current_page() - 1) * page_size()

		    dt_cols <- c(
		      "AC",
		      "PMID",
		      "UniProt AC",
		      "Autoregulatory Type",
		      "Polarity",
		      "Mechanism Probability",
		      "Type Confidence",
		      "Title",
		      "Abstract",
		      "Journal",
		      "Authors",
		      "Year",
		      "Month",
		      "Source",
		      "Protein Name",
		      "Gene Name",
		      "Protein ID",
		      "Organism"
		    )

		    sort_map <- list(
		      "AC" = list(expr = "AC", missing = "AC IS NULL OR TRIM(AC) = ''"),
		      "PMID" = list(expr = "CAST(PMID AS INTEGER)", missing = "PMID IS NULL OR TRIM(PMID) = ''"),
		      "UniProt AC" = list(expr = "UniProtKB_accessions", missing = "UniProtKB_accessions IS NULL OR TRIM(UniProtKB_accessions) = ''"),
		      "Autoregulatory Type" = list(expr = "Autoregulatory_Type", missing = "Autoregulatory_Type IS NULL OR TRIM(Autoregulatory_Type) = ''"),
		      "Polarity" = if (db_has_polarity) list(expr = "Polarity", missing = "Polarity IS NULL OR TRIM(Polarity) = ''") else NULL,
		      "Mechanism Probability" = list(expr = "Mechanism_Probability", missing = "Mechanism_Probability IS NULL"),
		      "Type Confidence" = list(expr = "Type_Confidence", missing = "Type_Confidence IS NULL"),
		      "Title" = list(expr = "Title", missing = "Title IS NULL OR TRIM(Title) = ''"),
		      "Abstract" = list(expr = "Abstract", missing = "Abstract IS NULL OR TRIM(Abstract) = ''"),
		      "Journal" = list(expr = "Journal", missing = "Journal IS NULL OR TRIM(Journal) = ''"),
		      "Authors" = list(expr = "Authors", missing = "Authors IS NULL OR TRIM(Authors) = ''"),
		      "Year" = list(expr = "CAST(Year AS INTEGER)", missing = "Year IS NULL"),
		      "Month" = list(expr = "Month", missing = "Month IS NULL OR TRIM(Month) = ''"),
		      "Source" = list(expr = "Source", missing = "Source IS NULL OR TRIM(Source) = ''"),
		      "Protein Name" = list(expr = "Protein_Name", missing = "Protein_Name IS NULL OR TRIM(Protein_Name) = ''"),
		      "Gene Name" = list(expr = "Gene_Name", missing = "Gene_Name IS NULL OR TRIM(Gene_Name) = ''"),
		      "Protein ID" = list(expr = "Protein_ID", missing = "Protein_ID IS NULL OR TRIM(Protein_ID) = ''"),
		      "Organism" = list(expr = "OS", missing = "OS IS NULL OR TRIM(OS) = ''")
		    )

		    order_clause <- "ORDER BY Title IS NULL, CAST(PMID AS INTEGER)"
		    sort <- table_sort()
		    if (!is.null(sort) && !is.null(sort$idx) && !is.null(sort$dir)) {
		      idx <- suppressWarnings(as.integer(sort$idx))
		      dir <- as.character(sort$dir)
		      if (!is.na(idx) && idx >= 0 && idx < length(dt_cols) && dir %in% c("asc", "desc")) {
		        col <- dt_cols[[idx + 1]]
		        spec <- sort_map[[col]]
		        if (!is.null(spec) && !is.null(spec$expr) && nzchar(spec$expr)) {
		          tie_break <- if (col == "PMID") "" else ", CAST(PMID AS INTEGER)"
		          order_clause <- paste0("ORDER BY (", spec$missing, ") ASC, ", spec$expr, " ", toupper(dir), tie_break)
		        }
		      }
		    }

		    query <- paste(
		      "SELECT *",
		      filters$where,
		      order_clause,
		      "LIMIT ? OFFSET ?"
		    )
		    params <- c(filters$params, page_size(), offset)

	    result <- dbGetQuery(conn, query, params = params)

	    # Convert column names back to spaces for display
	    colnames(result) <- gsub("_", " ", colnames(result))
	    if ("UniProtKB accessions" %in% colnames(result)) {
	      colnames(result)[colnames(result) == "UniProtKB accessions"] <- "UniProt AC"
	    }
	    # The database column is OS; it is shown to users as "Organism"
	    if ("OS" %in% colnames(result)) {
	      colnames(result)[colnames(result) == "OS"] <- "Organism"
	    }
	    # Source is stored as "Predicted"; show the fuller description instead
	    if ("Source" %in% colnames(result)) {
	      result$Source <- display_source(result$Source)
	    }

	    # Replace Autoregulatory Type 'NA' and 'none' values with 'non-autoregulatory'
	    if ("Autoregulatory Type" %in% colnames(result)) {
	      result$`Autoregulatory Type` <- ifelse(
	        is.na(result$`Autoregulatory Type`) |
          trimws(result$`Autoregulatory Type`) == "" |
          tolower(trimws(result$`Autoregulatory Type`)) == "none",
        "non-autoregulatory",
        result$`Autoregulatory Type`
      )
    }

    # Ensure Polarity exists (some older DB builds may not have the column).
    if (!("Polarity" %in% colnames(result))) {
      result$Polarity <- NA_character_
    }

    # If Polarity is missing/blank, derive it deterministically from Autoregulatory Type.
    pol_empty <- is.na(result$Polarity) | trimws(as.character(result$Polarity)) == ""
    if (any(pol_empty) && ("Autoregulatory Type" %in% colnames(result))) {
      types <- result$`Autoregulatory Type`

      to_key <- function(type) {
        if (is.na(type) || trimws(type) == "" || tolower(trimws(type)) == "non-autoregulatory") return(NA_character_)
        raw  <- trimws(type)
        norm <- gsub("[^A-Za-z]", "", raw)
        norm <- paste0(toupper(substr(norm, 1, 1)), tolower(substr(norm, 2, nchar(norm))))
        mapped <- if (norm %in% names(ontology_key_map)) ontology_key_map[[norm]] else NA_character_
        if (!is.na(mapped)) return(mapped)
        norm
      }

      keys <- vapply(types, to_key, character(1))
      symbols <- vapply(keys, function(k) {
        if (is.na(k) || k == "") return(NA_character_)
        sym <- get_polarity_symbol(k)
        if (is.na(sym) || sym == "" || sym == "Unknown") return(NA_character_)
        if (sym == "-") return("–")
        sym
      }, character(1))

      result$Polarity[pol_empty] <- symbols[pol_empty]
    }

    return(result)
  })

  observeEvent(input$next_page, {
    req(total_count())
    max_page <- max(ceiling(total_count() / page_size()), 1)
    if (current_page() < max_page) {
      current_page(current_page() + 1)
    }
  })

  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })

  observe({
    total <- total_count()
    max_page <- max(ceiling(total / page_size()), 1)
    if (current_page() > max_page) {
      current_page(max_page)
    }
  })

  output$page_status <- renderText({
    total <- as.numeric(total_count()[1])

    if (is.na(total) || total <= 0) {
      return("No results to display")
    }

    max_page <- ceiling(total / page_size())
    page <- min(current_page(), max_page)
    paste0("Page ", page, " of ", max_page)
  })

	  # Statistics tab outputs
	  # Dataset statistics (reactive based on filtered data)
	  output$stat_total_papers <- renderText({
	    total <- as.numeric(total_count()[1])
	    if (is.na(total)) total <- 0
	    format(total, big.mark = ",")
	  })

	  output$stat_filters_summary <- renderText({
	    summarize_values <- function(label, values, max_items = 3) {
	      if (is.null(values) || length(values) == 0) return(NULL)
	      values <- as.character(values)
	      values <- values[!is.na(values) & trimws(values) != ""]
	      if (length(values) == 0) return(NULL)
	      if (length(values) > max_items) {
	        shown <- paste(values[seq_len(max_items)], collapse = ", ")
	        return(paste0(label, ": ", shown, " +", length(values) - max_items))
	      }
	      paste0(label, ": ", paste(values, collapse = ", "))
	    }

	    parts <- c()

	    # Only show non-default / user-provided filters (keeps it screenshot-friendly).
	    if (!is.null(input$type) && length(input$type) > 0) {
	      parts <- c(parts, summarize_values("Type", input$type, max_items = 2))
	    }

		    if (!is.null(input$polarity) && length(input$polarity) > 0) {
		      selected <- as.character(input$polarity)
		      selected <- selected[!is.na(selected) & trimws(selected) != ""]
		      selected[selected == "-"] <- "–"
		      if (length(selected) > 0 && !setequal(selected, c("+", "–", "±"))) {
		        parts <- c(parts, summarize_values("Polarity", selected, max_items = 3))
		      }
		    }

	    from <- suppressWarnings(as.integer(trimws(as.character(input$year_from))))
	    to <- suppressWarnings(as.integer(trimws(as.character(input$year_to))))
	    if (!is.na(from) && !is.na(to)) {
	      if (from > to) {
	        tmp <- from
	        from <- to
	        to <- tmp
	      }
	      parts <- c(parts, paste0("Year: ", from, "–", to))
	    } else if (!is.na(from)) {
	      parts <- c(parts, paste0("Year: ≥", from))
	    } else if (!is.na(to)) {
	      parts <- c(parts, paste0("Year: ≤", to))
	    }

	    if (!is.null(input$month) && length(input$month) > 0) {
	      parts <- c(parts, summarize_values("Month", input$month, max_items = 4))
	    }

	    if (!is.null(input$source_mode) && nzchar(input$source_mode) && input$source_mode != "all") {
	      parts <- c(parts, paste0("Source: ", input$source_mode))
	    }

	    if (!is.null(input$journal) && length(input$journal) > 0) {
	      parts <- c(parts, summarize_values("Journal", input$journal, max_items = 2))
	    }

	    if (!is.null(input$os) && length(input$os) > 0) {
	      parts <- c(parts, summarize_values("Organism", input$os, max_items = 1))
	    }

	    if (!is.null(input$pmid) && nzchar(input$pmid)) {
	      parts <- c(parts, paste0("PMID: ", trimws(input$pmid)))
	    }
	    if (!is.null(input$ac) && nzchar(input$ac)) {
	      parts <- c(parts, paste0("UniProt AC: ", trimws(input$ac)))
	    }
	    if (!is.null(input$record_ac) && nzchar(input$record_ac)) {
	      parts <- c(parts, paste0("AC: ", trimws(input$record_ac)))
	    }
	    if (!is.null(input$protein_id) && nzchar(input$protein_id)) {
	      parts <- c(parts, paste0("Protein ID: ", trimws(input$protein_id)))
	    }
	    if (!is.null(input$protein_name) && nzchar(input$protein_name)) {
	      parts <- c(parts, paste0("Protein: ", trimws(input$protein_name)))
	    }
	    if (!is.null(input$gene_name) && nzchar(input$gene_name)) {
	      parts <- c(parts, paste0("Gene: ", trimws(input$gene_name)))
	    }
	    if (!is.null(input$author) && nzchar(input$author)) {
	      parts <- c(parts, paste0("Author: ", trimws(input$author)))
	    }
	    if (!is.null(input$search) && nzchar(input$search)) {
	      q <- trimws(input$search)
	      if (nchar(q) > 70) q <- paste0(substr(q, 1, 67), "...")
	      parts <- c(parts, paste0("Text: “", q, "”"))
	    }

	    parts <- parts[!is.na(parts) & parts != ""]
	    if (length(parts) == 0) return("No filters applied")
	    paste(parts, collapse = " • ")
	  })

		  output$stat_source_plot <- renderPlotly({
		    is_mobile <- is_mobile_output("stat_source_plot")
		    filters <- build_filter_query()
		    query <- paste(
	      "SELECT Source AS label, COUNT(*) as n",
	      filters$where,
	      "GROUP BY Source"
	    )
	    res <- if (length(filters$params) > 0) {
	      dbGetQuery(conn, query, params = filters$params)
	    } else {
	      dbGetQuery(conn, query)
	    }
	    if (nrow(res) == 0) {
	      res <- data.frame(label = character(0), n = numeric(0))
	    }
		    res$label <- ifelse(is.na(res$label) | res$label == "", "Unknown", res$label)
		    res$label <- display_source(res$label)
		    res$label <- factor(res$label, levels = c("UniProt", PREDICTED_LABEL, "OmniPath", "SIGNOR", "TRRUST", "Unknown"))
		    res <- res[order(res$label), ]
		    color_map <- setNames(
		      c("#d97742", "#1a2332", "#3498db", "#27ae60", "#9b59b6", "#94a3b8"),
		      c("UniProt", PREDICTED_LABEL, "OmniPath", "SIGNOR", "TRRUST", "Unknown")
		    )
		    text_size <- if (is_mobile) 10 else 12
		    text_info <- if (is_mobile) "percent" else "label+percent"

		    p <- plot_ly(
		      labels = res$label,
		      values = res$n,
		      text = format(res$n, big.mark = ","),
		      type = 'pie',
		      hole = 0.55,
		      sort = FALSE,
		      marker = list(colors = unname(color_map[as.character(res$label)])),
		      textinfo = text_info,
		      textposition = 'inside',
		      hovertemplate = "<b>%{label}</b><br>Papers: %{text}<extra></extra>",
		      textfont = list(size = text_size)
		    ) %>%
		      layout(
		        showlegend = FALSE,
		        margin = list(l = 10, r = 10, t = 10, b = 10)
		      )
		    apply_plotly_config(p, is_mobile)
		  })

  output$stat_type_plot <- renderPlotly({
    is_mobile <- is_mobile_output("stat_type_plot")
    filters <- build_filter_query()
    query <- paste(
      "SELECT Autoregulatory_Type AS type, COUNT(*) as n",
      filters$where,
      "GROUP BY Autoregulatory_Type"
    )
    res <- if (length(filters$params) > 0) {
      dbGetQuery(conn, query, params = filters$params)
    } else {
      dbGetQuery(conn, query)
    }
    res <- res %>% filter(!is.na(type) & trimws(type) != "" & tolower(trimws(type)) != "non-autoregulatory")
    if (nrow(res) == 0) {
      res <- data.frame(type = character(0), n = numeric(0))
    }
    colnames(res) <- c("Type", "Count")
    type_counts <- res[order(res$Count, decreasing = TRUE), ]
    type_counts$TypeLabel <- if (is_mobile) truncate_label(type_counts$Type, 18) else type_counts$Type
    text_position <- if (is_mobile) "none" else "outside"
    left_margin <- if (is_mobile) 90 else 180
    right_margin <- if (is_mobile) 16 else 40
    bottom_margin <- if (is_mobile) 40 else 50
    tick_size <- if (is_mobile) 10 else 12
    x_title <- if (is_mobile) "" else "Number of Papers"

    p <- plot_ly(
      data = type_counts,
      x = ~Count,
      y = ~reorder(TypeLabel, Count),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#d97742'),
      text = ~format(Count, big.mark = ","),
      textposition = text_position,
      hovertemplate = "<b>%{customdata}</b><br>Papers: %{text}<extra></extra>",
      customdata = ~Type
    ) %>%
      layout(
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = tick_size)),
        xaxis = list(title = x_title, tickfont = list(size = tick_size)),
        margin = list(l = left_margin, r = right_margin, t = 10, b = bottom_margin)
      )
    apply_plotly_config(p, is_mobile)
  })

  output$stat_year_plot <- renderPlotly({
    is_mobile <- is_mobile_output("stat_year_plot")
    filters <- build_filter_query()
    query <- paste(
      "SELECT Year AS year, COUNT(*) as n",
      filters$where,
      "GROUP BY Year"
    )
    res <- if (length(filters$params) > 0) {
      dbGetQuery(conn, query, params = filters$params)
    } else {
      dbGetQuery(conn, query)
    }
    res <- res %>% filter(!is.na(year) & year != "Unknown")
    colnames(res) <- c("Year", "Count")
    year_counts <- res

    if (nrow(year_counts) > 0) {
      year_counts$Year <- suppressWarnings(as.numeric(as.character(year_counts$Year)))
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      year_counts <- year_counts %>%
        filter(!is.na(Year) & Year >= 1800 & Year <= current_year) %>%
        arrange(Year)

      if (nrow(year_counts) == 0) {
        p <- plot_ly() %>%
          layout(
            annotations = list(
              text = "No year data available",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5
            )
          )
      } else {
        max_count <- max(year_counts$Count)
        year_counts$Size <- if (max_count > 0) {
          10 + (sqrt(year_counts$Count) / sqrt(max_count)) * 30
        } else {
          10
        }

        p <- plot_ly(
          data = year_counts,
          x = ~Year,
          y = 0,
          type = 'scatter',
          mode = 'markers',
          text = ~format(Count, big.mark = ","),
          marker = list(
            size = ~Size,
            color = ~Count,
            colorscale = list(c(0, "#fef5f0"), c(1, "#d97742")),
            showscale = !is_mobile,
            line = list(color = "rgba(0, 0, 0, 0.15)", width = 1)
          ),
          hovertemplate = "<b>Year:</b> %{x}<br><b>Papers:</b> %{text}<extra></extra>"
        ) %>%
          layout(
            xaxis = list(
              title = if (is_mobile) "" else "Publication Year",
              tickangle = if (is_mobile) -30 else -45,
              tickfont = list(size = if (is_mobile) 10 else 12)
            ),
            yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
            margin = list(l = if (is_mobile) 20 else 40, r = if (is_mobile) 10 else 20, t = 10, b = if (is_mobile) 60 else 80),
            showlegend = FALSE
          )
      }
      return(apply_plotly_config(p, is_mobile))
    } else {
      p <- plot_ly() %>%
        layout(
          annotations = list(
            text = "No year data available",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          )
        )
      return(apply_plotly_config(p, is_mobile))
    }
  })

  output$stat_journal_plot <- renderPlotly({
    is_mobile <- is_mobile_output("stat_journal_plot")
    limit_n <- if (is_mobile) 6 else 10
    filters <- build_filter_query()
    query <- paste(
      "SELECT Journal AS label, COUNT(*) as n",
      filters$where,
      "AND Journal IS NOT NULL AND trim(Journal) != ''",
      "GROUP BY Journal",
      "ORDER BY n DESC",
      "LIMIT", limit_n
    )
    res <- if (length(filters$params) > 0) {
      dbGetQuery(conn, query, params = filters$params)
    } else {
      dbGetQuery(conn, query)
    }
    if (nrow(res) == 0) {
      res <- data.frame(label = character(0), n = numeric(0))
    }
    res$label <- ifelse(is.na(res$label) | res$label == "", "Unknown", res$label)
    res <- res[order(res$n, decreasing = TRUE), ]
    res$label_short <- if (is_mobile) truncate_label(res$label, 22) else res$label
    text_position <- if (is_mobile) "none" else "outside"
    left_margin <- if (is_mobile) 110 else 220
    right_margin <- if (is_mobile) 16 else 40
    bottom_margin <- if (is_mobile) 40 else 50
    tick_size <- if (is_mobile) 10 else 12
    x_title <- if (is_mobile) "" else "Number of Papers"

    p <- plot_ly(
      data = res,
      x = ~n,
      y = ~reorder(label_short, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#d97742'),
      text = ~format(n, big.mark = ","),
      textposition = text_position,
      hovertemplate = "<b>%{customdata}</b><br>Papers: %{text}<extra></extra>",
      customdata = ~label
    ) %>%
      layout(
        xaxis = list(title = x_title, tickfont = list(size = tick_size)),
        yaxis = list(title = "", automargin = TRUE, tickfont = list(size = tick_size)),
        margin = list(l = left_margin, r = right_margin, t = 10, b = bottom_margin)
      )
    apply_plotly_config(p, is_mobile)
  })

  output$stat_probability_plot <- renderPlotly({
    is_mobile <- is_mobile_output("stat_probability_plot")
    filters <- build_filter_query()

    # Query to get all Mechanism_Probability values
    query <- paste(
      "SELECT Mechanism_Probability",
      filters$where,
      "AND Mechanism_Probability IS NOT NULL"
    )

    res <- if (length(filters$params) > 0) {
      dbGetQuery(conn, query, params = filters$params)
    } else {
      dbGetQuery(conn, query)
    }

    if (nrow(res) == 0 || all(is.na(res$Mechanism_Probability))) {
      p <- plot_ly() %>%
        layout(
          annotations = list(
            text = "No probability data available",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          )
        )
      return(apply_plotly_config(p, is_mobile))
    }

    # Create bins for histogram using actual data range
    probs <- res$Mechanism_Probability
    probs <- probs[!is.na(probs)]

    if (length(probs) == 0) {
      p <- plot_ly() %>%
        layout(
          annotations = list(
            text = "No valid probability data",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          )
        )
      return(apply_plotly_config(p, is_mobile))
    }

    # Calculate statistics and range for annotation
    mean_prob <- mean(probs)
    median_prob <- median(probs)
    min_prob <- min(probs)
    max_prob <- max(probs)

    # Get current threshold from filter
    threshold <- if (!is.null(input$min_mechanism_prob)) input$min_mechanism_prob else min_prob

    # Check if threshold is actually active (above minimum, with small tolerance for floating point)
    threshold_active <- (threshold - min_prob) > 0.001

    # Calculate counts above/below threshold
    total_count <- length(probs)
    above_threshold <- sum(probs >= threshold)
    below_threshold <- sum(probs < threshold)
    pct_above <- (above_threshold / total_count) * 100

    # Create histogram
    p <- plot_ly(
      x = probs,
      type = 'histogram',
      nbinsx = 20,
      marker = list(
        color = '#d97742',
        line = list(color = 'white', width = 1)
      ),
      hovertemplate = "Probability: %{x:.2f}<br>Count: %{y}<extra></extra>"
    )

    # Add vertical line for threshold if active
    if (threshold_active) {
      p <- p %>%
        add_trace(
          x = c(threshold, threshold),
          y = c(0, max(hist(probs, breaks = 20, plot = FALSE)$counts) * 1.1),
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#e63946', width = 3, dash = 'dash'),
          name = sprintf("Threshold: %.2f", threshold),
          hovertemplate = sprintf("Threshold: %.2f<extra></extra>", threshold),
          showlegend = FALSE
        )
    }

    # Build annotation text
    if (threshold_active) {
      anno_text <- sprintf(
        "Mean: %.3f | Median: %.3f<br>Threshold: %.2f<br>≥ Threshold: %s (%s%%)",
        mean_prob, median_prob, threshold,
        format(above_threshold, big.mark = ","),
        format(round(pct_above, 1), nsmall = 1)
      )
    } else {
      anno_text <- sprintf("Mean: %.3f<br>Median: %.3f", mean_prob, median_prob)
    }

    p <- p %>%
      layout(
        xaxis = list(
          title = if (is_mobile) "Probability" else "Mechanism Probability",
          range = c(min_prob - 0.01, max_prob + 0.01),  # Add small padding
          tickfont = list(size = if (is_mobile) 10 else 12)
        ),
        yaxis = list(
          title = if (is_mobile) "Count" else "Number of Predictions",
          tickfont = list(size = if (is_mobile) 10 else 12)
        ),
        margin = list(
          l = if (is_mobile) 40 else 60,
          r = if (is_mobile) 20 else 40,
          t = if (is_mobile) 30 else 40,
          b = if (is_mobile) 40 else 50
        ),
        bargap = 0.05,
        annotations = list(
          list(
            x = 0.98,
            y = 0.98,
            xref = "paper",
            yref = "paper",
            text = anno_text,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = if (threshold_active) "#e63946" else "#d97742",
            borderwidth = if (threshold_active) 2 else 1,
            borderpad = 4,
            font = list(size = if (is_mobile) 9 else 10)
          )
        )
      )

    apply_plotly_config(p, is_mobile)
  })

  output$stat_type_confidence_plot <- renderPlotly({
    is_mobile <- is_mobile_output("stat_type_confidence_plot")
    filters <- build_filter_query()

    # Query to get all Type_Confidence values
    query <- paste(
      "SELECT Type_Confidence",
      filters$where,
      "AND Type_Confidence IS NOT NULL"
    )

    res <- if (length(filters$params) > 0) {
      dbGetQuery(conn, query, params = filters$params)
    } else {
      dbGetQuery(conn, query)
    }

    if (nrow(res) == 0 || all(is.na(res$Type_Confidence))) {
      p <- plot_ly() %>%
        layout(
          annotations = list(
            text = "No confidence data available",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          )
        )
      return(apply_plotly_config(p, is_mobile))
    }

    # Create bins for histogram using actual data range
    confs <- res$Type_Confidence
    confs <- confs[!is.na(confs)]

    if (length(confs) == 0) {
      p <- plot_ly() %>%
        layout(
          annotations = list(
            text = "No valid confidence data",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          )
        )
      return(apply_plotly_config(p, is_mobile))
    }

    # Calculate statistics and range for annotation
    mean_conf <- mean(confs)
    median_conf <- median(confs)
    min_conf <- min(confs)
    max_conf <- max(confs)

    # Get current threshold from filter
    threshold <- if (!is.null(input$min_type_conf)) input$min_type_conf else min_conf

    # Check if threshold is actually active (above minimum, with small tolerance for floating point)
    threshold_active <- (threshold - min_conf) > 0.001

    # Calculate counts above/below threshold
    total_count <- length(confs)
    above_threshold <- sum(confs >= threshold)
    below_threshold <- sum(confs < threshold)
    pct_above <- (above_threshold / total_count) * 100

    # Create histogram
    p <- plot_ly(
      x = confs,
      type = 'histogram',
      nbinsx = 20,
      marker = list(
        color = '#d97742',
        line = list(color = 'white', width = 1)
      ),
      hovertemplate = "Confidence: %{x:.2f}<br>Count: %{y}<extra></extra>"
    )

    # Add vertical line for threshold if active
    if (threshold_active) {
      p <- p %>%
        add_trace(
          x = c(threshold, threshold),
          y = c(0, max(hist(confs, breaks = 20, plot = FALSE)$counts) * 1.1),
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#e63946', width = 3, dash = 'dash'),
          name = sprintf("Threshold: %.2f", threshold),
          hovertemplate = sprintf("Threshold: %.2f<extra></extra>", threshold),
          showlegend = FALSE
        )
    }

    # Build annotation text
    if (threshold_active) {
      anno_text <- sprintf(
        "Mean: %.3f | Median: %.3f<br>Threshold: %.2f<br>≥ Threshold: %s (%s%%)",
        mean_conf, median_conf, threshold,
        format(above_threshold, big.mark = ","),
        format(round(pct_above, 1), nsmall = 1)
      )
    } else {
      anno_text <- sprintf("Mean: %.3f<br>Median: %.3f", mean_conf, median_conf)
    }

    p <- p %>%
      layout(
        xaxis = list(
          title = if (is_mobile) "Confidence" else "Type Confidence",
          range = c(min_conf - 0.01, max_conf + 0.01),  # Add small padding
          tickfont = list(size = if (is_mobile) 10 else 12)
        ),
        yaxis = list(
          title = if (is_mobile) "Count" else "Number of Predictions",
          tickfont = list(size = if (is_mobile) 10 else 12)
        ),
        margin = list(
          l = if (is_mobile) 40 else 60,
          r = if (is_mobile) 20 else 40,
          t = if (is_mobile) 30 else 40,
          b = if (is_mobile) 40 else 50
        ),
        bargap = 0.05,
        annotations = list(
          list(
            x = 0.98,
            y = 0.98,
            xref = "paper",
            yref = "paper",
            text = anno_text,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = if (threshold_active) "#e63946" else "#d97742",
            borderwidth = if (threshold_active) 2 else 1,
            borderpad = 4,
            font = list(size = if (is_mobile) 9 else 10)
          )
        )
      )

    apply_plotly_config(p, is_mobile)
  })

  # Model performance tables (static)
  output$model_stage1_table <- renderTable({
    data.frame(
      Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
      Score = c("96.0%", "97.8%", "90.0%", "93.8%")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = 'lc')

  output$model_stage2_overall_table <- renderTable({
    data.frame(
      Metric = c("Accuracy", "Macro Precision", "Macro Recall", "Macro F1",
                 "Weighted Precision", "Weighted Recall", "Weighted F1"),
      Value = c("95.5%", "94.6%", "98.1%", "96.2%", "95.9%", "95.5%", "95.5%")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = 'lc')

  output$model_stage2_perclass_table <- renderTable({
    data.frame(
      Mechanism = c("Autophosphorylation", "Autoregulation", "Autocatalytic",
                    "Autoinhibition", "Autoubiquitination", "Autolysis", "Autoinducer"),
      Precision = c("99.0%", "92.3%", "91.7%", "94.4%", "85.0%", "100.0%", "100.0%"),
      Recall = c("92.5%", "100.0%", "100.0%", "94.4%", "100.0%", "100.0%", "100.0%"),
      F1_Score = c("95.6%", "96.0%", "95.6%", "94.4%", "91.9%", "100.0%", "100.0%"),
      Support = c("107", "24", "22", "18", "17", "6", "6")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = 'lcccc')


 	output$result_table <- renderDT({
 	  data <- filtered_data()

	  sort <- table_sort()
	  dt_order <- list()
	  if (!is.null(sort) && !is.null(sort$idx) && !is.null(sort$dir)) {
	    idx <- suppressWarnings(as.integer(sort$idx))
	    dir <- as.character(sort$dir)
	    if (!is.na(idx) && idx >= 0 && idx < ncol(data) && dir %in% c("asc", "desc")) {
	      dt_order <- list(list(idx, dir))
	    }
	  }
 	 
 	  data <- data %>% select(
	    AC,
	    PMID,
	    `UniProt AC`,
	    `Autoregulatory Type`,
	    Polarity,
	    `Mechanism Probability`,
	    `Type Confidence`,
	    Title,
	    Abstract,
	    Journal,
	    Authors,
	    Year,
	    Month,
	    Source,
	    `Protein Name`,
	    `Gene Name`,
	    `Protein ID`,
	    Organism
 	  )

	  # Store original AC identifiers for magnifier button lookups
	  row_acs <- data$AC

	  # Make PMID a clickable link to PubMed
	  data$PMID <- ifelse(
	    !is.na(data$PMID) & data$PMID != "",
    paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', data$PMID,
           '" target="_blank" style="color: #0366d6; text-decoration: none;">',
           data$PMID, '</a>'),
    data$PMID
	  )

	  # Make UniProtKB accessions clickable to UniProt (first accession if multiple)
	  uniprot_col <- "UniProt AC"
	  data[[uniprot_col]] <- ifelse(
	    !is.na(data[[uniprot_col]]) & data[[uniprot_col]] != "",
	    sapply(seq_along(data[[uniprot_col]]), function(i) {
	      ac <- data[[uniprot_col]][i]
	      first_ac <- trimws(strsplit(ac, ",")[[1]][1])
      full_ac <- tooltip_cell(ac, 40)
      paste0('<a href="https://www.uniprot.org/uniprotkb/', first_ac,
             '" target="_blank" style="color: #0366d6; text-decoration: none;">',
             full_ac, '</a>')
    }),
    tooltip_cell(data[[uniprot_col]], 40)
	  )

	  data$AC <- plain_cell(data$AC)
	  data$`Protein Name` <- tooltip_cell(data$`Protein Name`, 60)
	  data$`Gene Name` <- plain_cell(data$`Gene Name`)

  # Clean up Protein ID: hide "NA_####" entries (show blank instead)
  data$`Protein ID` <- ifelse(
    grepl("^NA_", data$`Protein ID`),
    "",  # Show blank for NA_#### entries
    tooltip_cell(data$`Protein ID`, 40)
	  )

	  data$Organism <- plain_cell(data$Organism)
	  data$Title <- plain_cell(data$Title)
	  data$Abstract <- safe_cell(data$Abstract, 250, "Abstract", row_acs, "Show abstract")
	  data$Journal <- plain_cell(data$Journal)
	  data$Authors <- tooltip_cell(data$Authors, 80)

  getOntologyDetails <- function(type) {
    if (is.na(type) || type == "non-autoregulatory" || trimws(type) == "") {
      return("Not classified as autoregulatory")
    }
    raw  <- trimws(type)
    norm <- gsub("[^A-Za-z]", "", raw)
    norm <- paste0(toupper(substr(norm,1,1)), tolower(substr(norm,2,nchar(norm))))
    mapped <- if (norm %in% names(ontology_key_map)) ontology_key_map[[norm]] else NA_character_
    key    <- if (!is.na(mapped)) mapped else norm
	    info <- ontology_info[[key]]
	    if (is.null(info)) return(paste0("Ontology information not found for: ", htmltools::htmlEscape(raw)))
	    path <- get_ontology_path(key)
	    pol <- get_polarity_symbol(key)
	    paste0(
	      "<b>Ontology Path</b><br>", htmltools::htmlEscape(path), "<br><br>",
	      "<b>Polarity</b><br>", htmltools::htmlEscape(pol), "<br><br>",
	      "<b>Definition</b><br>", htmltools::htmlEscape(info$Definition), "<br><br>",
	      "<b>Synonym:</b> ", htmltools::htmlEscape(info$Synonym), "<br><br>",
	      "<b>Antonym:</b> ", htmltools::htmlEscape(info$Antonym), "<br><br>",
	      "<b>Related:</b> ", htmltools::htmlEscape(info$Related)
	    )
	  }

  type_values <- data$`Autoregulatory Type`
  safe_types <- vapply(type_values, function(val) {
    if (is.na(val)) return("")
    htmltools::htmlEscape(val)
  }, character(1))

  data$`Autoregulatory Type` <- ifelse(
    is.na(type_values) | type_values == "non-autoregulatory",
    safe_types,
    paste0(
      safe_types,
      '<br><button class="btn btn-link btn-sm view-btn" data-field="Autoregulatory Type" data-text="',
      htmltools::htmlEscape(vapply(type_values, getOntologyDetails, character(1))),
      '">Ontology</button>'
    )
  )

	  datatable(
	    data,
	    escape = FALSE,
	    rownames = FALSE,
		    options = list(
		      pageLength = page_size(),  # Show all loaded rows for current page size
		      lengthMenu = PAGE_SIZE_OPTIONS,  # Allow selectable page sizes
		      scrollX = TRUE,
		      dom = 't',  # Only show table (no info text at bottom)
		      order = dt_order,
		      ordering = TRUE,  # Enable column header sorting
		      columnDefs = list(
		        list(
		          targets = 1,  # PMID column (rendered as HTML link)
		          render = JS(
	            "function(data, type, row, meta) {",
	            "  if (type === 'sort' || type === 'type') {",
	            "    var m = String(data).match(/\\d+/);",
	            "    return m ? parseInt(m[0], 10) : 0;",
	            "  }",
	            "  return data;",
	            "}"
	          )
	        ),
	        list(targets = c(3, 4, 5, 6, 11, 12, 13), className = "dt-center"),  # Type, Polarity, probabilities, date, source
	        list(targets = "_all", orderSequence = c("asc","desc",""), className = "dt-left")  # All other columns - left aligned
	      ),
	      # Server-side processing: only load current page, not all rows
	      serverSide = FALSE,  # Client-side mode, but we prevent default sorting in callback
      deferRender = TRUE,
      scroller = FALSE,
      # Add info icons with tooltips to column headers
      headerCallback = JS(
        "function(thead, data, start, end, display) {",
        "  var tooltips = {",
        "    'AC': 'SOORENA accession ID. Format: SOORENA-{Source}-{PMID}-{Counter}. Source codes: U=UniProt, P=AI-assisted curation, O=OmniPath, S=SIGNOR, T=TRRUST',",
        "    'PMID': 'PubMed identifier. Click to view the publication on PubMed',",
        "    'UniProt AC': 'UniProtKB accession number(s). Click to view protein entry on UniProt. May contain multiple comma-separated accessions',",
        "    'Autoregulatory Type': 'Classification of autoregulatory mechanism (e.g., Autophosphorylation, Autoubiquitination). Click the Ontology button for detailed ontology information',",
        "    'Polarity': 'Direction of regulation: + (positive/activation), – (negative/inhibition), ± (context-dependent)',",
        "    'Mechanism Probability': 'ML model confidence that this entry describes an autoregulatory mechanism (0-1 scale, where 1.0 = curated or highest confidence)',",
        "    'Type Confidence': 'ML model confidence in the specific autoregulatory type classification (0-1 scale)',",
        "    'Title': 'Title of the publication from PubMed',",
        "    'Abstract': 'Abstract text from the publication',",
        "    'Journal': 'Journal name where the publication appeared',",
        "    'Authors': 'List of publication authors',",
        "    'Year': 'Publication year',",
        "    'Month': 'Publication month',",
        "    'Source': 'Origin of the record: AI-assisted curation (identified from the literature by the SOORENA two-stage model), UniProt (expert-curated), OmniPath/SIGNOR/TRRUST (external curated databases)',",
        "    'Protein Name': 'Full name of the protein from UniProt',",
        "    'Gene Name': 'Gene symbol for the protein',",
        "    'Protein ID': 'UniProt protein identifier',",
        "    'Organism': 'Organism/species (e.g., Homo sapiens, Mus musculus)'",
        "  };",
        "  ",
        "  // Add CSS for info icon (only once)",
        "  if (!$('#column-info-icon-style').length) {",
        "    $('<style id=\"column-info-icon-style\">' +",
        "      '.col-info-icon {' +",
        "      '  display: inline-block;' +",
        "      '  width: 16px;' +",
        "      '  height: 16px;' +",
        "      '  line-height: 16px;' +",
        "      '  border-radius: 50%;' +",
        "      '  border: 1.5px solid #6c757d;' +",
        "      '  color: #6c757d;' +",
        "      '  font-size: 11px;' +",
        "      '  font-weight: bold;' +",
        "      '  font-style: italic;' +",
        "      '  text-align: center;' +",
        "      '  margin-left: 5px;' +",
        "      '  cursor: help;' +",
        "      '  vertical-align: middle;' +",
        "      '  font-family: Georgia, serif;' +",
        "      '}' +",
        "      '.col-info-icon:hover {' +",
        "      '  background-color: #6c757d;' +",
        "      '  color: white;' +",
        "      '  border-color: #6c757d;' +",
        "      '}' +",
        "      '.col-info-tip.in { opacity: 1; }' +",
        "      '.col-info-tip .tooltip-inner {' +",
        "      '  max-width: 320px;' +",
        "      '  font-size: 14px;' +",
        "      '  line-height: 1.4;' +",
        "      '  text-align: left;' +",
        "      '  padding: 8px 12px;' +",
        "      '  background-color: #ffffff;' +",
        "      '  color: #212529;' +",
        "      '  border: 1px solid #1f4e79;' +",
        "      '  border-radius: 4px;' +",
        "      '  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);' +",
        "      '}' +",
        "      '.col-info-tip.top .tooltip-arrow { border-top-color: #1f4e79; }' +",
        "      '.col-info-tip.bottom .tooltip-arrow { border-bottom-color: #1f4e79; }' +",
        "      '</style>').appendTo('head');",
        "  }",
        "  ",
        "  // Remove tooltips left over from the previous draw",
        "  $('.tooltip.col-info-tip').remove();",
        "  ",
        "  $(thead).find('th').each(function() {",
        "    var $th = $(this);",
        "    var colName = $th.text().trim().replace(/\\s*i$/, '').trim();",
        "    ",
        "    // Remove any existing info icon",
        "    $th.find('.col-info-icon').tooltip('destroy').remove();",
        "    ",
        "    if (tooltips[colName]) {",
        "      var icon = $('<span class=\"col-info-icon\" title=\"' + ",
        "        tooltips[colName].replace(/\"/g, '&quot;') + '\">i</span>');",
        "      $th.append(' ').append(icon);",
        "      // Bootstrap tooltip: appears immediately and can be styled, unlike the native title tooltip",
        "      icon.tooltip({",
        "        container: 'body',",
        "        placement: 'top',",
        "        delay: { show: 0, hide: 0 },",
        "        template: '<div class=\"tooltip col-info-tip\" role=\"tooltip\"><div class=\"tooltip-arrow\"></div><div class=\"tooltip-inner\"></div></div>'",
        "      });",
        "    }",
        "  });",
        "}"
      ),
      # Center column headers
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).find('th').css('text-align', 'center');",
        "}"
      )
	    ),
	    callback = JS("
	      // Prevent DataTables from sorting client-side; handle server-side instead
	      table.off('click', 'th');
	      $(table.table().header()).on('click', 'th', function(e) {
	        var colIdx = table.column(this).index();
	        if (colIdx === null || colIdx === undefined) return;

	        // Get current order state for this column
	        var currentOrder = table.order();
	        var currentDir = null;
	        if (currentOrder.length > 0 && currentOrder[0][0] === colIdx) {
	          currentDir = currentOrder[0][1];
	        }

	        // Determine next direction: asc -> desc -> none -> asc
	        var nextDir = 'asc';
	        if (currentDir === 'asc') {
	          nextDir = 'desc';
	        } else if (currentDir === 'desc') {
	          nextDir = '';
	        }

	        // Send to Shiny for server-side processing
	        if (nextDir === '') {
	          Shiny.setInputValue('result_table_order', [], {priority: 'event'});
	        } else {
	          Shiny.setInputValue('result_table_order', [[colIdx, nextDir]], {priority: 'event'});
	        }

	        // Prevent event bubbling
	        e.stopImmediatePropagation();
	        return false;
	      });

	      table.off('click', '.view-btn');
	      table.on('click', '.view-btn', function() {
	        var row_id = $(this).data('row-id');
	        var field = $(this).data('field');
	        var text = $(this).data('text');
	        Shiny.setInputValue('show_full_text', { field: field, row_id: row_id, text: text }, {priority: 'event'});
	      });
	    ")
	  ) %>%
	  formatRound(columns = c("Mechanism Probability", "Type Confidence"), digits = 3)
	})



  # Updates Table Data
  patch_notes_data <- data.frame(
    Version = c("0.0.1", "0.0.2", "0.0.3", "0.0.4", "0.0.5", "0.0.6", "0.0.7", "0.0.8", "0.0.9", "0.0.10", "0.0.11", "0.0.12", "0.0.13", "0.0.14", "0.0.15"),
    Description = c(
      paste(
        "<ul>",
        "<li>Shiny App Prototype</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>App Nickname & Logo</li>",
        "<li>Search Functionality Enhancement</li>",
        "<li>UI Cleanup</li>",
        "<li>Paper Source</li>",
        "<li>Protein Accession Handling</li>",
        "<li>Additional Tabs</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>New columns: Polarity score & Autoregulatory term probability</li>",
        "<li>Added placeholder content to Statistics tab</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Replaced placeholder data with sample data</li>",
        "<li>Added expand button to Autoregulatory Type for full ontology view</li>",
        "<li>Statistics Tab: Label distribution (positive / negative / neutral)</li>",
        "<li>Statistics Tab: Feature frequency (Journal, Species, Data Source, Year)</li>",
        "<li>Statistics Tab: Model evaluation metrics (e.g., accuracy, F1-score)</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Updated app logo</li>",
        "<li>Statistics Tab: Added Proportion (%) display in hover tooltips for frequency plots</li>",
        "<li>Statistics Tab: Updated evaluation metrics</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Statistics Tab: Updated Species Frequency plot to show Top 9 species</li>",
        "<li>Statistics Tab: Added Dot Plot (Species vs Autoregulatory Type)</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Ontology Tab: Added new Ontology tab with structured tree and detailed descriptions</li>",
        "<li>Enhanced ontology pop-up content in Search tab (definitions, synonyms, antonyms, related terms)</li>",
        "<li>Hid OntologyFullText column in final search table to keep interface clean</li>",
        "<li>Improved overall UI and typography for Ontology content</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Major update aligned with the SOORENA preprint (bioRxiv, November 2025)</li>",
        "<li>Integrated 252,880 model-inferred PubMed abstracts into searchable database</li>",
        "<li>Removed Polarity and Polarity Score columns</li>",
        "<li>Updated column schema: Has Mechanism, Mechanism Probability, Autoregulatory Type, Type Confidence</li>",
        "<li>Improved table UX: sortable columns, truncation with magnifier pop-up modal, CSV export</li>",
        "<li>Updated Ontology tab with hierarchical mechanism tree and detailed definition pages for all seven mechanism classes</li>",
        "<li>Ontology pop-ups now show standardized definitions, relations, and references</li>",
        "<li>Removed old Statistics tab and placeholders</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>New Publication Year and Publication Month filter dropdowns</li>",
        "<li>Table now displays Year, and Month for all papers</li>",
        "<li>Existing data labeled with 'No Date' / 'Unknown' until new data is added</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li>Data refresh: loaded ~3.37M predictions into SQLite for full searchability</li>",
        "<li>Pagination: added rows-per-page selector (25/50/100/500) wired to SQL LIMIT/OFFSET and DataTable page length</li>",
        "<li>Pagination messaging: page status now clamps to available pages; removed table info banner</li>",
        "<li>UI layout: consolidated pagination controls with Download CSV aligned on the right for cleaner toolbar</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li><strong>Data Quality:</strong> Database now contains only autoregulatory mechanisms — all non-autoregulatory and mechanism-less entries removed for consistency</li>",
        "<li><strong>UniProt Accessions (AC):</strong> Added unique row identifier (AC) with standardized UniProtKB_accessions field throughout database and UI</li>",
        "<li><strong>Clickable Links:</strong> PMID and UniProt AC columns now link directly to PubMed and UniProt databases for easy reference access</li>",
        "<li><strong>Polarity Column:</strong> Reintroduced Polarity with deterministic assignment (+/−/±) based on mechanism type, with visual legend and help modal explaining positive/negative feedback loops</li>",
        "<li><strong>Column Reordering:</strong> Optimized column order for better workflow — AC, PMID, Title, Abstract, Mechanism Type, Polarity, Confidence, Year, Month, Journal, Source</li>",
        "<li><strong>Advanced Filtering:</strong> Complete filter UI overhaul with organized panels, polarity filter, year range slider, source toggles, and collapsible advanced options</li>",
        "<li><strong>Global Column Filters:</strong> Search filters now apply to entire dataset across all pages, not just current page — with dynamic match count display</li>",
        "<li><strong>Exact Match Mode:</strong> Added exact/partial match toggle for precise text searching in Title, Abstract, and other fields</li>",
        "<li><strong>Server-Side Sorting:</strong> Implemented efficient server-side sorting with automatic pagination reset when sort order changes</li>",
        "<li><strong>Filter Summary Panel:</strong> Real-time summary showing active filters and result counts for better query transparency</li>",
        "<li><strong>Enhanced Statistics:</strong> Updated all statistics visualizations to reflect autoregulatory-only dataset; added pie chart for source distribution with improved mobile layout</li>",
        "<li><strong>Ontology Updates:</strong> Ontology tab now includes AC examples and improved mechanism definitions with polarity context</li>",
        "<li><strong>Date Enrichment:</strong> Publication Year and Month now populated from PubMed metadata for all records (previously 'Unknown')</li>",
        "<li><strong>Improved Robustness:</strong> Better error handling for PubMed API rate limits and transient HTTP errors during enrichment</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li><strong>External Resources Integration:</strong> Added curated autoregulatory data from external biological databases: OmniPath (protein-protein interactions), SIGNOR (signaling/phosphorylation), and TRRUST (transcriptional regulation). These appear as separate Source entries alongside UniProt and Predicted data</li>",
        "<li><strong>External Resources Enrichment:</strong> External resource entries automatically enriched with publication metadata (Title, Abstract, Journal, Authors, Date, Protein Name) using cached PubMed/UniProt lookups for faster subsequent runs</li>",
        "<li><strong>Column Help Icons:</strong> Added small circled 'i' icons next to column headers. Hover over any icon to see a tooltip explaining what the column contains (AC format, data sources, confidence scores, etc.)</li>",
        "<li><strong>AC Format Standardization:</strong> Updated SOORENA accession IDs to consistent dash format (SOORENA-{Source}-{PMID}-{Counter}) with clear source codes (U=UniProt, P=Predicted, O=OmniPath, S=SIGNOR, T=TRRUST)</li>",
        "<li><strong>AC Documentation:</strong> Added comprehensive AC format documentation explaining the ID structure, source codes, and counter meaning</li>",
        "<li><strong>Invalid PMID Handling:</strong> External resource entries with missing PMIDs now use 'UNKNOWN' for clarity instead of invalid placeholders</li>",
        "<li><strong>Abstract Display Fix:</strong> Fixed issue where long abstracts were truncated in modal popups. Now uses database queries instead of HTML attributes to ensure complete text displays for all fields (Abstract, Title, Journal, Authors, etc.)</li>",
        "<li><strong>Probability Distribution Visualizations:</strong> Added interactive histograms in Statistics tab showing Mechanism Probability and Type Confidence distributions with mean/median statistics</li>",
        "<li><strong>Interactive Threshold Filters:</strong> Added Model Confidence section in Advanced Filters with sliders for Minimum Mechanism Probability and Minimum Type Confidence</li>",
        "<li><strong>Visual Threshold Feedback:</strong> Distribution plots now display red dashed threshold lines when filters are active, showing real-time count and percentage of entries above threshold</li>",
        "<li><strong>Dynamic Filter Ranges:</strong> Threshold sliders automatically adjust to actual data ranges instead of hardcoded 0-1, maximizing usable slider space</li>",
        "<li><strong>Side-by-Side Statistics:</strong> Mechanism Probability and Type Confidence histograms displayed side-by-side for easy comparison</li>",
        "<li><strong>Smart Filter Detection:</strong> Threshold filters only activate when slider moves above minimum data value, preventing unnecessary filtering at baseline</li>",
        "<li><strong>Reset Improvements:</strong> Reset Filters button now correctly returns thresholds to data minimums rather than zero</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li><strong>Label Clarity Improvement:</strong> Changed 'Data Source' filter label to 'Source of Autoregulation Data' for better clarity and specificity</li>",
        "<li><strong>Probability Precision:</strong> Rounded Mechanism Probability and Type Confidence values to 3 decimal places (e.g., 0.644 instead of 0.6438683271408081) for improved readability</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li><strong>External Resource Deduplication Fix:</strong> Updated deduplication strategy to preserve species and mechanism variants. SIGNOR entries increased from 394 to 995 (+601 entries), OmniPath from 10 to 20 (+10 entries). Previous aggressive deduplication by PMID+Source+Gene incorrectly removed valid biological variants (e.g., human vs mouse, phosphorylation vs dephosphorylation)</li>",
        "<li><strong>New Ontology Terms:</strong> Added three new autoregulatory mechanism types to ontology: Autodephosphorylation (9 entries from SIGNOR), Autoacetylation (1 entry from SIGNOR), and Autodemethylation (2 entries from SIGNOR) under 'Enzymatic Self-Modification' category</li>",
        "<li><strong>External Mechanism Mapping:</strong> Implemented systematic mapping of external database mechanism types to SOORENA ontology (e.g., phosphorylation → Autophosphorylation, cleavage → Autocatalytic, transcriptional → Autoregulation) for consistent classification across all data sources</li>",
        "<li><strong>Ontology Tab Updates:</strong> Added Autodephosphorylation (–), Autoacetylation (+), and Autodemethylation (±) to ontology hierarchy visualization with full ontology relations, definitions, synonyms, antonyms, and references</li>",
        "</ul>"
      ),
      paste(
        "<ul>",
        "<li><strong>Organism Column:</strong> The species column is now labelled &quot;Organism&quot; instead of &quot;OS&quot; in the results table, search filter, CSV export and column tooltips. Also fixes the magnifier pop-up for that column, which previously failed to look up the full value</li>",
        "<li><strong>Evaluation Metrics Correction:</strong> Corrected the Stage 2 per-class precision for Autoubiquitination in the Statistics tab from 85.9% to 85.0% (17/20), consistent with the published confusion matrix and macro-precision of 94.6%</li>",
        "<li><strong>Table Readability:</strong> Fields short enough to display in full (AC, Title, Journal, Organism, Gene Name) are no longer truncated. Longer multi-valued fields now reveal their full value on mouseover instead of requiring a click, and the abstract control is labelled &quot;Show abstract&quot; rather than a magnifier icon. The Ontology button now always appears on its own line below the autoregulatory type</li>",
        "<li><strong>Column Help Icons:</strong> The &quot;i&quot; icons next to the column headers now show their description immediately on mouseover, in a larger, easier-to-read box, instead of after a delay of about two seconds</li>",
        "<li><strong>Source Terminology:</strong> Records identified by the SOORENA model are now labelled &quot;AI-assisted curation&quot; instead of &quot;Predicted&quot; across the search filter, results table, CSV export and statistics chart, to describe the method more accurately. The underlying data is unchanged and accession IDs are unaffected</li>",
        "</ul>"
      )
    ),
    Date = c("2025-05-29", "2025-06-01", "2025-06-04", "2025-06-19", "2025-06-24", "2025-07-02", "2025-07-10", "2025-11-04", "2025-12-07", "2025-12-08", "2025-12-27", "2026-01-12", "2026-01-13", "2026-01-15", "2026-09-14"),
    stringsAsFactors = FALSE
  )

  # Render Updates Table
  output$patch_notes_table <- DT::renderDataTable({
    DT::datatable(
      patch_notes_data[nrow(patch_notes_data):1, ],  # Reverse order: latest first
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        scrollX = TRUE
      ),
      escape = FALSE,   # <-- allow HTML rendering
      rownames = FALSE
    )
  })

  # Cleanup: Disconnect database when app stops
  session$onSessionEnded(function() {
    if (dbIsValid(conn)) dbDisconnect(conn)
    cat("Database connection closed\n")
  })
}

# Run App
shinyApp(ui, server)
