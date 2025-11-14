# Figures 1–3: Analysis and Visualization Scripts

This repository includes the R scripts used to generate Figures 1–3 for the manuscript:

**"Genetic and chemical survey of Swiss CBD hemp flowers"**

---

# Figure 1 — CBD / THC Multi-Panel Summary (A–D)
**Script:** `R/Fig1_CBD_THC_2x2.R`

A single script builds a **2×2 composite figure** summarizing CBD, THC, CBD:THC ratios, and THC legal categories for the Swiss CBD market.

### **Panel A — Total CBD (%)**
- Violin + boxplot + jittered *white* points  
- Four reference thresholds:
  - **EU anchor:** 6%
  - **Empirical P80:** ~15.4%
  - **Empirical P90:** ~17.4%
  - **Swiss anchor:** 20%
- Threshold labels printed *above* the plot (no legend)

### **Panel B — Total THC (%)**
- Violin + boxplot + jitter (filled red)  
- **Dotted line at 1.0% THC**  
- Annotation: `n = X/Y (Z%) ≥ 1%`

### **Panel C — CBD:THC ratio (log10 scale)**
- Violin + jitter  
- Dotted reference lines at **20:1** and **30:1**  
- Annotation reporting % of samples ≥20:1

### **Panel D — THC distribution vs Swiss legal cutoffs**
- Binned THC categories:
  - ≤0.3%
  - 0.3–<0.5%
  - 0.5–<1.0%  
- % labels above bars

**Outputs:**  
`Fig01_CBD_THC_Ratio_THCgroups.png`  
`Fig01_CBD_THC_Ratio_THCgroups.pdf`

---

# Figure 2 — Vendor CBD Claims vs Measured CBD
**Script:** `R/Fig2_CBD_Vendor_vs_Measured.R`

This script compares **vendor-declared CBD%** vs **lab-measured CBD%**:

- Green bars = vendor claim  
- Red-outlined bars = measured CBD  
- `*` marks samples missing vendor information  
- Automatically extracts appropriate columns from the Excel sheet  
- Computes:
  - Mean difference
  - Standard deviation
  - Paired t-test
  - Wilcoxon signed-rank test

**Optional output (if activated):**  
`Fig2_CBD_only_vendor_vs_measured.png`

---

# Figure 3 — THC vs CBD Correlation + Decarboxylation Ratio
**Script:** `R/Fig3_THC_vs_CBD_and_Decarb.R`

Generates two panels:

### **(A) THC vs CBD correlation**
- Pearson regression (R², p-value)
- Spearman correlation (ρ, p-value)
- Outlier labeling for extreme combinations
- Regression line with 95% confidence interval

### **(B) Histogram of CBD decarboxylation ratios**
- Automatically detects whether ratios are % or decimals  
- Clean histogram + density overlay showing distribution of CBD decarb ratios

**Outputs saved beneath:**  
`outputs_fig02b/`

---



