# porositAI

`porositAI` is an R-based, user-friendly tool for rapid, reproducible quantification of pore space in soil thin section imagery. It supports using **three image types in tandem**:

- **Translucent / transmitted light** (recommended baseline)
- **Polarized light** (optional, improves classification)
- **Refracted light** (optional, improves classification)

The workflow is designed so users do **not** need to pre-create masks. Instead, the user uses a user-guided, point-based labeling approach (~20 clicks) to train an image-specific classifier.

In addition to porosity, the tool can:
- separate touching pores (includes an **erosion/opening** step),
- segment individual pores,
- group pores into categories,
- export a **publication-ready grouped mask** with a **legend** and **user-defined group names**.

---

## What this tool outputs

### 1) Porosity outputs
- `pore_mask.png`  
  Binary pore mask (1 = pore, 0 = solid/background).
- `overlay.png`  
  The pore mask overlaid on the base image for quick QA/QC.
- Porosity (%)  
  Computed as:

  **Porosity (%) = 100 × (pore pixels within ROI) / (ROI pixels)**

### 2) Pore segmentation + grouping outputs
After pore separation + labeling:
- `pore_features.csv`  
  One row per pore (area, perimeter, equivalent diameter, circularity, etc.)
- `group_summary.csv`  
  Per-group summary:
  - `% of pore area`
  - `% of pore count`

### 3) Publication-ready grouped mask
- `pore_mask_grouped.png`  
  Pore pixels are color-coded by group.
- `pore_mask_grouped_legend.png`  
  Same grouped mask plus a legend/key (group name + % area + % count).

Users can rename group labels (e.g., “Small”, “Medium”, “Large”) before exporting so the figure is close to publication-ready.

---

## Requirements

### R
- R 4.x recommended (older versions may work, but newer is safer for dependencies)

### Packages
CRAN:
- `shiny`
- `png`
- `ranger`

Bioconductor:
- `EBImage` (used for morphology, erosion/opening, and connected-component labeling)

Install:

```r
install.packages(c("shiny", "png", "ranger"))
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("EBImage")
