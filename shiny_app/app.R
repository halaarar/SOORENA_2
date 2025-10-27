library(shiny)       # for building the interactive web app
library(DT)          # for rendering interactive data tables
library(dplyr)       # for data manipulation
library(readr)       # for reading CSV files
library(shinyjs)     # for JavaScript integration (e.g., toggle dark mode)
library(htmltools)   # for safe HTML rendering
library(plotly)      # Creates interactive, dynamic, and web-friendly plots from ggplot or standalone
library(ggplot2)

# Load CSV Data
# Read preprocessed CSV file with PubMed preview data
preview_df <- read.csv("data/predictions_for_app.csv", stringsAsFactors = FALSE)
colnames(preview_df) <- gsub("\\.", " ", colnames(preview_df))



# Ensure required columns exist; fill missing ones with NA
required_cols <- c(
  # Unique ID
  "Protein ID",
  
  # Protein metadata
  "AC",
  "OS",
  
  # Publication metadata
  "PMID",
  "Title",
  "Abstract",
  "Journal",
  "Authors",
  "Source",
  
  # Mechanism info
  "Has Mechanism",
  "Mechanism Probability",
  "Autoregulatory Type",
  "Type Confidence"
)

for (col in required_cols) {
  if (!(col %in% colnames(preview_df))) {
    preview_df[[col]] <- NA
  }
}

# Final data frame to be used
df <- preview_df[, required_cols]

# Convert probability/confidence to percent strings
num_or_na <- function(x) suppressWarnings(as.numeric(x))
safe_percent_column <- function(x) {
  num <- num_or_na(x)
  ifelse(!is.na(num), paste0(round(num * 100, 2), "%"), NA)
}
df$`Mechanism Probability` <- safe_percent_column(df$`Mechanism Probability`)
df$`Type Confidence`      <- safe_percent_column(df$`Type Confidence`)


# replace Autoregulatory Type 'NA' value with 'non-autoregulatory'
df$`Autoregulatory Type` <- ifelse(
  is.na(df$`Autoregulatory Type`) | 
    trimws(df$`Autoregulatory Type`) == "" |
    df$`Autoregulatory Type` == "none",
  "non-autoregulatory",
  df$`Autoregulatory Type`
)

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
  )
)

# Map dataset labels to ontology keys when names differ
ontology_key_map <- c(
  "Autocatalytic" = "Autocatalysis",
  "Autoinducer"   = "Autoinduction"
  # Add more aliases here if your data contains other variants
)

# Ontology path helper for display
get_ontology_path <- function(key) {
  enzymatic <- c("Autokinase","Autophosphorylation","Autoubiquitination","Autolysis","Autocatalysis")
  expression <- c("Autoactivation","Autoinhibition","Autoinduction","Autofeedback","Autoregulation")
  if (key %in% enzymatic)   return(paste("Autoregulatory Mechanisms (Root) → Enzymatic Self-Modification →", key))
  if (key %in% expression)  return(paste("Autoregulatory Mechanisms (Root) → Expression Control →", key))
  "Autoregulatory Mechanisms (Root)"
}


# Define UI
# Header UI reused across all tabs
header_ui <- div(
  class = "header-section",
  div(class = "app-title", img(src = "logo.png"), "SOORENA"),
  div(class = "header-logos",
      img(src = "logoHelsinki.png"),
      img(src = "logoUBC.png"),
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
        tags$style(HTML("
          body {
            background-color: #f9f9f9;
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
          .btn-warning {
            margin-top: 10px;
          }
          .dataTables_wrapper {
            background-color: #ffffff;
            padding: 10px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            margin: 0 30px 20px 30px;
          }
        "))
      ),
      
      # Header section
      header_ui,
      
      # Search and Filter Controls
  # Search and Filter Controls
  div(class = "filter-panel",
    fluidRow(
      column(
        width = 8,
        fluidRow(
          column(3, textInput("protein_id", "Protein ID", placeholder = "Search protein ID...")),
          column(3, textInput("ac", "AC", placeholder = "Search AC...")),
          column(3, selectInput("has_mechanism", "Has Mechanism", choices = c("All", "Yes", "No"))),
          column(3, selectInput("os", "OS",
                                choices = c("All", sort(unique(na.omit(df$OS)))),
                                multiple = TRUE))
        ),
        fluidRow(
          column(3, textInput("pmid", "PMID", placeholder = "Search PMID...")),
          column(3, textInput("author", "Author", placeholder = "Search author...")),
          column(3, selectInput("journal", "Journal",
                                choices = c("All", sort(unique(na.omit(df$Journal)))),
                                multiple = TRUE)),
          column(3, selectInput("source", "Data Source", choices = c("All", "UniProt", "Non-UniProt")))
        ),
        fluidRow(
          column(3, selectInput("type", "Autoregulatory Type",
                                choices = c("All", sort(unique(na.omit(df$`Autoregulatory Type`)))),
                                multiple = TRUE))
        )
      ),
      column(
        width = 4,
        textAreaInput("search", "Search Title / Abstract", placeholder = "Type or paste any text...", height = "120px"),
        actionButton("reset_filters", "Reset Filters", class = "btn-warning")
      )
    )
  ),
        
        # Download Button
        div(style = "margin: 0 30px;",
            downloadButton("download_csv", "Download CSV", class = "btn-primary mb-3")
        ),
        
        # Display Table
        div(style = "margin: 0 30px;",
            DTOutput("result_table"))
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
            color: #2c3e50;
            text-decoration: none;
            cursor: pointer;
            font-weight: 500;
          }
          .tree-link:hover {
            color: #3498db;
            text-decoration: underline;
          }
          .mechanism-box {
            scroll-margin-top: 80px;
          }
        "))
      ),
      
      div(style = "
         padding: 30px;
         background: #ffffff;
         border-radius: 12px;
         box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
         margin: 30px;
         font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
         line-height: 1.8;
         font-size: 15px;",
          
          h2("Autoregulatory Mechanisms Ontology", style = "color: #2c3e50; font-weight: 700; margin-bottom: 20px;"),
          
          p(style = "font-size: 16px; color: #555;", 
            "A structured classification of self-directed biochemical processes identified and categorized by the SOORENA pipeline."),
          
          # Ontology Tree - FIXED with better contrast and clickable links
          h3("Hierarchical Structure", style = "color: #2c3e50; margin-top: 30px;"),
          div(style = "
            background: #f8f9fa;
            padding: 25px;
            border-radius: 8px;
            border: 2px solid #dee2e6;
            margin: 20px 0;",
            tags$pre(style = "
              color: #2c3e50;
              font-size: 15px;
              line-height: 2.2;
              font-family: 'Menlo', 'Monaco', 'Courier New', monospace;
              margin: 0;
              font-weight: 500;",
              HTML("Self-directed biochemical processes
│
├─── Self-catalytic chemistry
│    └─── <a href='#autocatalytic' class='tree-link'>Autocatalytic Reaction</a>
│
├─── Protein self-modification (post-translational)
│    ├─── <a href='#autophosphorylation' class='tree-link'>Autophosphorylation</a>
│    └─── <a href='#autoubiquitination' class='tree-link'>Autoubiquitination</a>
│
├─── Intrinsic regulatory control
│    ├─── <a href='#autoregulation' class='tree-link'>Autoregulation of Gene Expression</a>
│    └─── <a href='#autoinhibition' class='tree-link'>Autoinhibition within Proteins</a>
│
├─── Self-degradation and lysis
│    └─── <a href='#autolysis' class='tree-link'>Autolysis</a>
│
└─── Population-level self-signaling
     └─── <a href='#autoinducer' class='tree-link'>Autoinducer Molecules in Quorum Sensing</a>")
            )
          ),
          
          hr(style = "margin: 40px 0; border-top: 2px solid #e0e0e0;"),
          
          # Detailed Mechanism Descriptions with MUTED PROFESSIONAL COLORS
          h3("Mechanism Definitions with Ontology Relations and Citations", style = "color: #2c3e50;"),
          
          # 1. Autocatalytic Reaction
          div(id = "autocatalytic", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #fef9e7; border-radius: 8px; border-left: 5px solid #d4af37; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("1. Autocatalytic Reaction", style = "color: #7d6608; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;", 
                "A chemical reaction in which its product or intermediate accelerates the same reaction. Supports nonlinear self-reinforcement and chemical self-organization."),
              
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
              style = "margin: 30px 0; padding: 25px; background: #edf4f7; border-radius: 8px; border-left: 5px solid #2c5aa0; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("2. Autophosphorylation", style = "color: #1e3a5f; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "A protein kinase phosphorylates its own amino acid residues (cis or trans), tuning its conformation, catalytic activity, localization, and signaling dynamics."),
              
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
          
          # 3. Autoubiquitination
          div(id = "autoubiquitination", class = "mechanism-box",
              style = "margin: 30px 0; padding: 25px; background: #f4f0f7; border-radius: 8px; border-left: 5px solid #6c4a8e; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("3. Autoubiquitination", style = "color: #4a2c5f; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "An E3 ubiquitin ligase attaches ubiquitin to itself, altering its stability, proteasomal targeting, and signaling functions depending on chain type and site."),
              
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
              style = "margin: 30px 0; padding: 25px; background: #eef7ee; border-radius: 8px; border-left: 5px solid #4a7c59; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("4. Autoregulation of Gene Expression", style = "color: #2d5f3a; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "A gene product regulates transcription of the same gene through positive or negative feedback, tuning gene expression dynamics, noise, and homeostasis."),
              
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
              style = "margin: 30px 0; padding: 25px; background: #eef7f5; border-radius: 8px; border-left: 5px solid #2a7a6b; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("5. Autoinducer Molecules in Quorum Sensing", style = "color: #1a5447; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Small diffusible molecules synthesized and detected by bacteria. Their accumulation with increasing cell density triggers coordinated community-wide transcriptional changes."),
              
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
              style = "margin: 30px 0; padding: 25px; background: #fdf1f1; border-radius: 8px; border-left: 5px solid #a94442; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("6. Autoinhibition within Proteins", style = "color: #6f2c2c; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Intrinsic structural interactions prevent inappropriate activation of a protein, maintaining it in an inactive state until relieved by ligand binding, structural rearrangement, or post-translational modification."),
              
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
              style = "margin: 30px 0; padding: 25px; background: #f5f6f7; border-radius: 8px; border-left: 5px solid #5a6c7d; box-shadow: 0 2px 4px rgba(0,0,0,0.08);",
              h4("7. Autolysis", style = "color: #34495e; margin-bottom: 15px; font-weight: 600;"),
              p(style = "color: #555; margin-bottom: 15px;",
                "Self-degradation mediated by endogenous lytic enzymes, occurring during programmed cell death, post-mortem breakdown, or engineered microbial lysis systems."),
              
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
          h3("SOORENA Classification Pipeline", style = "color: #2c3e50;"),
          div(style = "background: #34495e; padding: 25px; border-radius: 8px; color: #ffffff; box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
              p(style = "font-size: 16px; margin-bottom: 15px;", "This ontology is implemented through a two-stage deep learning classification system:"),
              tags$ol(style = "line-height: 2.2; font-size: 15px;",
                tags$li(tags$b("Stage 1 - Binary Classification:"), " Identifies papers describing autoregulatory mechanisms (96.0% accuracy, 93.8% F1)"),
                tags$li(tags$b("Stage 2 - Multi-class Classification:"), " Categorizes mechanisms into 7 types using weighted cross-entropy loss to handle class imbalance (95.5% accuracy, 96.2% macro F1)")
              ),
              div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 6px; margin-top: 20px; border: 1px solid rgba(255,255,255,0.2);",
                p(style = "margin: 5px 0;", tags$b("Base Model:"), " PubMedBERT"),
                p(style = "margin: 5px 0;", tags$b("Training Data:"), " 1,332 manually curated papers"),
                p(style = "margin: 5px 0;", tags$b("Total Database:"), " 254,212 analyzed papers")
              )
          )
      )
    )
  ),
  
  # Tab: Statistics
  tabPanel(
    title = "Statistics",
    fluidPage(
      header_ui,
      h2("Dataset Statistics"),
      
      # Section 1: Label Proportion
      h3("Autoregulatory Type Distribution"),
      plotlyOutput("label_distribution"),
      
      h3("Species vs Autoregulatory Type (Top 9 Species)"),
      plotlyOutput("species_type_dotplot", height = "700px"),
      
      
      # Section 2: Frequency Metrics
      h3("Feature Frequency"),
      fluidRow(
        column(6, plotlyOutput("journal_plot")),
        column(6, plotlyOutput("species_plot"))
      ),
      fluidRow(
        column(6, plotlyOutput("source_plot"))
      ),
      
      # Section 3: Model Evaluation Metrics
      h3("Model Evaluation Metrics"),
      tableOutput("model_metrics")
    )
  ),
  
  # Tab: Patch Notes
  tabPanel(
    title = "Patch Notes",
    fluidPage(
      header_ui,
      h2("Patch Notes"),
      DT::dataTableOutput("patch_notes_table")
    )
  ),
  
  # Tab: About Us
  tabPanel(
    title = "About Us",
    fluidPage(
      header_ui,
      h2("Project Contributors"),
      tags$ul(
        tags$li("Alexandra Zhou – University of British Columbia"),
        tags$li("Hala Arar – University of British Columbia"),
        tags$li("Mingyang Zhang – University of British Columbia"),
        tags$li("Zheng He – University of British Columbia")
      ),
      h2("Mentor & Partner"),
      tags$ul(
        tags$li("Mohieddin Jafari – University of Helsinki (Partner)"), 
        tags$li("Payman Nickchi – University of British Columbia (Mentor)")
      )
    )
  ),
)


# Define Server Logic
server <- function(input, output, session) {
  


  # Download csv button
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("filtered_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  

  # Show Full text
    observeEvent(input$show_full_text, {
      showModal(modalDialog(
        title = paste("Full", input$show_full_text$field),
        HTML(paste0("<div style='white-space: pre-wrap; font-family: sans-serif;'>", 
                    input$show_full_text$text, "</div>")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m"
      ))
    })
  
  # Reset all filters to default state
  observeEvent(input$reset_filters, {
    updateTextInput(session, "protein_id", value = "")
    updateTextInput(session, "ac", value = "")
    updateTextInput(session, "pmid", value = "")
    updateTextInput(session, "author", value = "")
    updateSelectInput(session, "journal", selected = "All")
    updateSelectInput(session, "os", selected = "All")
    updateSelectInput(session, "type", selected = "All")
    updateSelectInput(session, "has_mechanism", selected = "All")
    updateSelectInput(session, "source", selected = "All")
    updateTextAreaInput(session, "search", value = "")
  })
  
  # Filtering Logic
  filtered_data <- reactive({
    result <- df
    print(paste("Initial rows:", nrow(result)))
    
    # Journal filter
    if (!is.null(input$journal) && !"All" %in% input$journal && length(input$journal) > 0) {
      result <- result %>% filter(Journal %in% input$journal)
    }
    print(paste("Rows after Journal filter:", nrow(result)))
    
    # Type filter
    if (!is.null(input$type) && !"All" %in% input$type && length(input$type) > 0) {
      result <- result %>% filter(`Autoregulatory Type` %in% input$type)
    }
    print(paste("Rows after Type filter:", nrow(result)))
    

    
    # OS filter
    if (!is.null(input$os) && !"All" %in% input$os && length(input$os) > 0) {
      result <- result %>% filter(OS %in% input$os)
    }
    print(paste("Rows after OS filter:", nrow(result)))
    
    
    # AC search
    if (!is.null(input$ac) && nzchar(input$ac)) {
      terms <- trimws(unlist(strsplit(input$ac, ",")))
      pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
      result <- result %>% filter(grepl(pattern, AC, ignore.case = TRUE))
    }
    print(paste("Rows after AC search:", nrow(result)))
  
    # Protein ID search
    if (!is.null(input$protein_id) && nzchar(input$protein_id)) {
      terms <- trimws(unlist(strsplit(input$protein_id, ",")))
      pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
      result <- result %>% filter(grepl(pattern, `Protein ID`, ignore.case = TRUE))
    }
  print(paste("Rows after Protein ID search:", nrow(result)))

    # PMID search
    if (!is.null(input$pmid) && nzchar(input$pmid)) {
      terms <- trimws(unlist(strsplit(input$pmid, ",")))
      pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
      result <- result %>% filter(grepl(pattern, PMID, ignore.case = TRUE))
    }
    print(paste("Rows after PMID search:", nrow(result)))
    
    # Author search
    if (!is.null(input$author) && nzchar(input$author)) {
      terms <- trimws(unlist(strsplit(input$author, ",")))
      pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
      result <- result %>% filter(grepl(pattern, Authors, ignore.case = TRUE))
    }
    print(paste("Rows after Author search:", nrow(result)))

    # Has Mechanism filter
    if (!is.null(input$has_mechanism) && input$has_mechanism != "All") {
      result <- result %>% filter(`Has Mechanism` == input$has_mechanism)
    }
    print(paste("Rows after Has Mechanism filter:", nrow(result)))

    # Source filter  
    if (!is.null(input$source) && input$source != "All") {
      result <- result %>% filter(Source == input$source)
    }
    print(paste("Rows after Source filter:", nrow(result)))
        
    # Title / Abstract search
    if (!is.null(input$search) && nzchar(input$search)) {
      terms <- trimws(unlist(strsplit(input$search, ",")))
      pattern <- paste(terms, collapse = "|")
      result <- result %>%
        filter(grepl(pattern, Title, ignore.case = TRUE) |
                 grepl(pattern, Abstract, ignore.case = TRUE))
    }
    print(paste("Rows after Title/Abstract search:", nrow(result)))
    
    return(result)
  })

  
output$result_table <- renderDT({
  data <- filtered_data()

  data <- data %>% select(
    AC, `Protein ID`, OS, PMID, Title, Abstract, Journal, Authors, Source,
    `Has Mechanism`, `Mechanism Probability`, `Autoregulatory Type`, `Type Confidence`
  )

  data$AC <- ifelse(!is.na(data$AC) & nchar(data$AC) > 30,
    paste0(substr(data$AC, 1, 30),
           '... <button class="btn btn-link btn-sm view-btn" data-field="AC" data-text="',
           htmltools::htmlEscape(data$AC),'">🔍</button>'),
    data$AC
  )

  data$`Protein ID` <- ifelse(!is.na(data$`Protein ID`) & nchar(data$`Protein ID`) > 25,
    paste0(substr(data$`Protein ID`, 1, 25),
           '... <button class="btn btn-link btn-sm view-btn" data-field="Protein ID" data-text="',
           htmltools::htmlEscape(data$`Protein ID`),'">🔍</button>'),
    data$`Protein ID`
  )

  data$OS <- ifelse(!is.na(data$OS) & nchar(data$OS) > 40,
    paste0(substr(data$OS, 1, 40),
           '... <button class="btn btn-link btn-sm view-btn" data-field="OS" data-text="',
           htmltools::htmlEscape(data$OS),'">🔍</button>'),
    data$OS
  )

  data$Title <- ifelse(!is.na(data$Title) & nchar(data$Title) > 50,
    paste0(substr(data$Title, 1, 50),
           '... <button class="btn btn-link btn-sm view-btn" data-field="Title" data-text="',
           htmltools::htmlEscape(data$Title),'">🔍</button>'),
    data$Title
  )

  data$Abstract <- ifelse(!is.na(data$Abstract) & nchar(data$Abstract) > 50,
    paste0(substr(data$Abstract, 1, 50),
           '... <button class="btn btn-link btn-sm view-btn" data-field="Abstract" data-text="',
           htmltools::htmlEscape(data$Abstract),'">🔍</button>'),
    data$Abstract
  )

  data$Journal <- ifelse(!is.na(data$Journal) & nchar(data$Journal) > 40,
    paste0(substr(data$Journal, 1, 40),
           '... <button class="btn btn-link btn-sm view-btn" data-field="Journal" data-text="',
           htmltools::htmlEscape(data$Journal),'">🔍</button>'),
    data$Journal
  )

  data$Authors <- ifelse(!is.na(data$Authors) & nchar(data$Authors) > 50,
    paste0(substr(data$Authors, 1, 50),
           '... <button class="btn btn-link btn-sm view-btn" data-field="Authors" data-text="',
           htmltools::htmlEscape(data$Authors),'">🔍</button>'),
    data$Authors
  )

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
    paste0(
      "<b>Ontology Path</b><br>", htmltools::htmlEscape(path), "<br><br>",
      "<b>Definition</b><br>", htmltools::htmlEscape(info$Definition), "<br><br>",
      "<b>Synonym:</b> ", htmltools::htmlEscape(info$Synonym), "<br><br>",
      "<b>Antonym:</b> ", htmltools::htmlEscape(info$Antonym), "<br><br>",
      "<b>Related:</b> ", htmltools::htmlEscape(info$Related)
    )
  }

  data$`Autoregulatory Type` <- ifelse(
    is.na(data$`Autoregulatory Type`) | data$`Autoregulatory Type` == "non-autoregulatory",
    data$`Autoregulatory Type`,
    paste0(
      data$`Autoregulatory Type`,
      ' <button class="btn btn-link btn-sm view-btn" data-field="Autoregulatory Type" data-text="',
      htmltools::htmlEscape(vapply(data$`Autoregulatory Type`, getOntologyDetails, character(1))),
      '"><span style="font-size:14px;">🔍</span></button>'
    )
  )

  datatable(
    data,
    escape = FALSE,
    options = list(
      pageLength = 10,
      lengthMenu = c(10, 25, 50),
      scrollX = TRUE,
      dom = 'tip',
      order = list(),
      columnDefs = list(list(targets = "_all", orderSequence = c("asc","desc","")))
    ),
    callback = JS("
      table.on('click', '.view-btn', function() {
        var text = $(this).data('text');
        var field = $(this).data('field');
        Shiny.setInputValue('show_full_text', { field: field, text: text }, {priority: 'event'});
      });
    ")
  )
})

  
  # Statistics Logic
  # Section 1: Label distribution lollipop chart
  output$label_distribution <- renderPlotly({
    label_counts <- table(df$`Autoregulatory Type`)
    total_count <- sum(label_counts)
    df_plot <- data.frame(
      Type = names(label_counts), 
      Count = as.numeric(label_counts),
      Proportion = round(as.numeric(label_counts) / total_count * 100, 2)
    )
    
    p <- ggplot(df_plot, aes(x = reorder(Type, Count), y = Count,
                             text = paste0(
                               "Type: ", Type,
                               "<br>Count: ", Count,
                               "<br>Proportion: ", Proportion, "%"
                             ))) +
      geom_segment(aes(x = reorder(Type, Count), xend = reorder(Type, Count), y = 0, yend = Count),
                   color = "#2980b9", size = 1.5) + 
      geom_point(color = "#2980b9", size = 5) +
      coord_flip() +
      labs(x = "Autoregulatory Type", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  # Species vs Autoregulatory Type Dot Plot
  output$species_type_dotplot <- renderPlotly({
    # Top 9 species
    top_species <- df %>%
      filter(!is.na(OS) & OS != "") %>%
      count(OS, sort = TRUE) %>%
      slice_head(n = 9) %>%
      pull(OS)
    
    combo_counts <- df %>%
      filter(!is.na(OS) & OS %in% top_species,
             !is.na(`Autoregulatory Type`) & `Autoregulatory Type` != "") %>%
      group_by(OS, `Autoregulatory Type`) %>%
      summarise(Count = n(), .groups = "drop")
    
    p <- ggplot(combo_counts, aes(x = `Autoregulatory Type`, y = OS, size = Count,
                                  text = paste0(
                                    "Species: ", OS,
                                    "<br>Autoregulatory Type: ", `Autoregulatory Type`,
                                    "<br>Count: ", Count
                                  ))) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(4, 18)) +
      labs(x = "Autoregulatory Type", y = "Species (OS)", size = "Count") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })

  
  # Section 2: 
  # Journal frequency - Lollipop Chart
  output$journal_plot <- renderPlotly({
    top_journals <- df %>%
      group_by(Journal) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      slice_head(n = 10) %>%
      mutate(Proportion = round(Count / sum(Count) * 100, 2))
    
    p <- ggplot(top_journals, aes(x = reorder(Journal, Count), y = Count,
                                  text = paste0(
                                    "Journal: ", Journal,
                                    "<br>Count: ", Count,
                                    "<br>Proportion: ", Proportion, "%"
                                  ))) +
      geom_segment(aes(x = reorder(Journal, Count), xend = reorder(Journal, Count), y = 0, yend = Count),
                   color = "#3498db", size = 1.5) +
      geom_point(color = "#3498db", size = 5) +
      coord_flip() +
      labs(x = "Journal", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  
  # Species frequency - Lollipop Chart
  output$species_plot <- renderPlotly({
    top_species <- df %>%
      group_by(OS) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      slice_head(n = 9) %>%
      mutate(Proportion = round(Count / sum(Count) * 100, 2))
    
    p <- ggplot(top_species, aes(x = reorder(OS, Count), y = Count,
                                 text = paste0(
                                   "Species: ", OS,
                                   "<br>Count: ", Count,
                                   "<br>Proportion: ", Proportion, "%"
                                 ))) +
      geom_segment(aes(x = reorder(OS, Count), xend = reorder(OS, Count), y = 0, yend = Count),
                   color = "#27ae60", size = 1.5) +
      geom_point(color = "#27ae60", size = 5) +
      coord_flip() +
      labs(x = "Top 9 Species (OS)", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  # Source frequency - Lollipop Chart
  output$source_plot <- renderPlotly({
    source_counts <- df %>%
      group_by(Source) %>%
      summarise(Count = n()) %>%
      mutate(Proportion = round(Count / sum(Count) * 100, 2))
    
    p <- ggplot(source_counts, aes(x = reorder(Source, Count), y = Count,
                                   text = paste0(
                                     "Source: ", Source,
                                     "<br>Count: ", Count,
                                     "<br>Proportion: ", Proportion, "%"
                                   ))) +
      geom_segment(aes(x = reorder(Source, Count), xend = reorder(Source, Count), y = 0, yend = Count),
                   color = "#8e44ad", size = 1.5) +
      geom_point(color = "#8e44ad", size = 5) +
      coord_flip() +
      labs(x = "Source", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  # Section 3: Model evaluation metrics (mock values)
  output$model_metrics <- renderTable({
    data.frame(
      Metric = c("Micro F1", "Macro F1", "Weighted F1", "Precision", "Recall"),
      Value  = c("62%", "57%", "68%", "65%", "61%")
    )
  })
  
  # Patch Notes Table Data
  patch_notes_data <- data.frame(
    Version = c("0.0.1", "0.0.2", "0.0.3", "0.0.4", "0.0.5", "0.0.6", "0.0.7"),
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
      )
    ),
    Date = c("2025-05-29", "2025-06-01", "2025-06-04", "2025-06-19", "2025-06-24", "2025-07-02", "2025-07-10"),
    stringsAsFactors = FALSE
  )
  
  # Render Patch Notes Table
  output$patch_notes_table <- DT::renderDataTable({
    DT::datatable(
      patch_notes_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      ),
      escape = FALSE,   # <-- allow HTML rendering
      rownames = FALSE
    )
  })
  
}

# Run App
shinyApp(ui, server)