# swiss-cbd-survey

R code and analysis for **Figure 1** of the manuscript:

**"Genetic and chemical survey of Swiss CBD hemp flowers"**

The repository contains the script used to generate the 4-panel figure (CBD, THC, CBD:THC ratio, and THC cut-off distribution).

---

## 🧬 Figure 1 – Overview

The script produces a **2×2 multi-panel figure**:

**Panel A — Total CBD (%)**
- Violin + boxplot + jittered white data points  
- Four reference lines:
  - EU anchor (6% CBD)
  - Empirical P80 (top 20%)
  - Empirical P90 (top 10%)
  - Swiss anchor (20% CBD)
- Colored threshold labels (no legend)

**Panel B — Total THC (%)**
- Violin + boxplot + jitter  
- Dotted line at 1.0%  
- Annotation showing number and percentage of samples ≥ 1% THC

**Panel C — CBD:THC Ratio (log10 scale)**
- Violin + jitter  
- Dotted reference lines at 20:1 and 30:1  
- Annotation reporting % of samples ≥ 20:1

**Panel D — THC Distribution vs Swiss Cut-offs**
- Barplot of samples falling into:
  - ≤0.3%
  - 0.3–<0.5%
  - 0.5–<1.0%
- Percent labels above bars

All four panels are combined using **patchwork** into a single figure and saved as PNG and PDF.
