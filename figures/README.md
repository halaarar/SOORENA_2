# Figures

## `report/` — the eight figures in the manuscript

| Figure | File | Script | Previous file name |
|---|---|---|---|
| 1 | `Figure1_boolean_networks.png` | — | (not previously in the repository) |
| 2 | `Figure2_workflow.png` | — (BioRender) | (not previously in the repository) |
| 3 | `Figure3_class_distribution.png` | `Rfiles/Figure3_class_distribution.R` | `figure_table3.*` |
| 4 | `Figure4_model_evaluation.png` | `Rfiles/Figure4_model_evaluation.R` | `figure_combined_456.*` |
| 5 | `Figure5_stage2_performance.png` | `Rfiles/Figure5_stage2_performance.R` | `figure5_combined.*` |
| 6 | `Figure6_web_interface.png` | — (screenshot) | `SOORENA_Dashboard.png` |
| 7 | `Figure7_mechanism_and_species.png` | `Rfiles/Figure7_mechanism_and_species.R` | `figure_predicted_distribution.*` |
| 8 | `Figure8_dataset_overview.png` | `Rfiles/Figure8_dataset_overview.R` | `figure_section35.*` |

Regenerate a figure by running its script from the repository root, for example:

```bash
Rscript figures/report/Rfiles/Figure8_dataset_overview.R
```

Figures 7 and 8 read `shiny_app/data/predictions.db` (fetch it with `git lfs pull`).
Figures 1, 2 and 6 have no script: Figure 1 and Figure 2 were drawn in BioRender, and
Figure 6 is a screenshot of the web application.

## `other/` — earlier and unused figures, kept for reference

- `figure_35_confidence.png`, `figure_35_journals.png`, `figure_35_source.png`,
  `figure_35_timeline.png`, `figure_35_types.png` — the separate panels that became Figure 8
- `figure_stage1.png`, `figure_table1.png`, `figure_table4.png`, `figure_table6.png`,
  `figure_table7.png` — earlier versions of the evaluation figures, superseded by Figures 4 and 5
- `ModelArchitecture.png` — early architecture sketch, replaced by Figure 2
- `figure_predicted_distribution_v1.png` — Figure 7 before the species panels were added
- `figure_section35_v1.png` — Figure 8 before the larger fonts and the AI-assisted curation labels

Their scripts are in `other/Rfiles/` and write back into `other/`.
