# ============================
# Fig. 1 — 2×2 panels (A–D)
# A: Total CBD (all points white) + four colored reference lines (no legend, no header text)
# B: Total THC
# C: CBD:THC ratio (log10) — violin + jitter, dotted 20:1 & 30:1
# D: THC distribution vs Swiss cut-offs (≤0.3, 0.3–<0.5, 0.5–<1.0)
# ============================

rm(list = ls())

# ---- Packages ----
need <- function(pkgs){
        for (p in pkgs) {
                if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
                suppressPackageStartupMessages(library(p, character.only = TRUE))
        }
}
need(c("readxl","dplyr","tidyr","ggplot2","scales","readr","stringr","patchwork"))

# ---- Path to your Excel file ----
data_file <- "/Users/gonzalovillarino/Documents_Local/Swizterland/Puregene/1.Research/2025/1_Projects/40_Swiss_Market/02_R_Codes_Fig_1/01_Cannabinoids_20250113_Analytics_Swiss_Market_Study_Summary_Table.xlsx"

# ---- Load ----
df_raw <- readxl::read_excel(data_file, col_names = TRUE)

# ---- Helpers ----
numize <- function(x) suppressWarnings(readr::parse_number(as.character(x)))

pick_measure_col <- function(df, target = c("CBD","THC")){
        nm <- names(df); target <- match.arg(target)
        incl <- if (target == "CBD") grepl("(?i)total\\s*cbd", nm) else grepl("(?i)total\\s*thc", nm)
        excl <- grepl("(?i)decarb|ratio|label|claim|cbda|thca", nm)
        idx <- which(incl & !excl)
        if (length(idx) == 0) {
                incl2 <- if (target == "CBD") grepl("(?i)\\bcbd\\b", nm) else grepl("(?i)\\bthc\\b", nm)
                idx <- which(incl2 & !excl)
        }
        if (length(idx) == 0) stop("Could not find a clean column for ", target, ".")
        nm[idx[1]]
}

cbd_col <- pick_measure_col(df_raw, "CBD")
thc_col <- pick_measure_col(df_raw, "THC")

# ---- Build tidy numeric data ----
dat <- df_raw %>%
        transmute(
                CBD = numize(.data[[cbd_col]]),
                THC = numize(.data[[thc_col]])
        ) %>%
        filter(!is.na(CBD) | !is.na(THC)) %>%
        mutate(
                CBD_cat = dplyr::case_when(
                        !is.na(CBD) & CBD >= 15 ~ "CBD ≥ 15%",
                        !is.na(CBD) & CBD <  15 ~ "CBD < 15%",
                        TRUE ~ NA_character_
                )
        )

# ==================
# Panel A — Total CBD (all points white, 4 colored threshold lines, no legend, no header text)
# ==================

# ==================
# Panel A — Total CBD (all points white, colored threshold lines, no legend)
# + top labels: "EU 6%", "P80 15.4%", "P90 17.4%", "CH 20%"
# ==================

# Empirical cutoffs
p80_val <- as.numeric(quantile(dat$CBD, probs = 0.80, type = 7, na.rm = TRUE))
p90_val <- as.numeric(quantile(dat$CBD, probs = 0.90, type = 7, na.rm = TRUE))

# Lines
thresh_df <- data.frame(
        y    = c(6.0, p80_val, p90_val, 20.0),
        type = factor(c("EU anchor (0.3% THC → 6% CBD)",
                        "Empirical P80 (top 20%)",
                        "Empirical P90 (top 10%)",
                        "Swiss anchor (1.0% THC → 20% CBD)"),
                      levels = c("EU anchor (0.3% THC → 6% CBD)",
                                 "Empirical P80 (top 20%)",
                                 "Empirical P90 (top 10%)",
                                 "Swiss anchor (1.0% THC → 20% CBD)"))
)

pA <- ggplot(dat, aes(x = "", y = CBD)) +
        geom_violin(width = 0.6, fill = "grey92", color = "black", alpha = 0.18, trim = FALSE) +
        geom_boxplot(width = 0.22, fill = "white", color = "black", outlier.shape = NA) +
        # all points white
        geom_jitter(
                width = 0.08, height = 0,
                shape = 21, size = 2.6, stroke = 0.25,
                color = "black", fill = "white", alpha = 0.95,
                na.rm = TRUE, show.legend = FALSE
        ) +
        # four reference lines (colored + distinct linetypes)
        geom_hline(data = thresh_df, aes(yintercept = y, linetype = type, color = type), linewidth = 1.0) +
        scale_linetype_manual(values = c(
                "EU anchor (0.3% THC → 6% CBD)"      = "dashed",   # 6%
                "Empirical P80 (top 20%)"            = "dotted",   # ~15.4%
                "Empirical P90 (top 10%)"            = "dotdash",  # ~17.4%
                "Swiss anchor (1.0% THC → 20% CBD)"  = "longdash"  # 20%
        )) +
        scale_color_manual(values = c(
                "EU anchor (0.3% THC → 6% CBD)"      = "darkgreen",
                "Empirical P80 (top 20%)"            = "steelblue4",
                "Empirical P90 (top 10%)"            = "orange3",
                "Swiss anchor (1.0% THC → 20% CBD)"  = "firebrick3"
        )) +
        guides(linetype = "none", color = "none") +
        labs(title = "Total CBD", x = "Samples", y = "Concentration (%)") +
        # allow text above plotting area + add headroom on top
        coord_cartesian(ylim = c(0, NA), clip = "off") +
        theme_classic(base_size = 14) +
        theme(
                axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title  = element_text(size = 15, face = "bold"),
                plot.title  = element_text(size = 16, face = "bold"),
                legend.position = "none",
                plot.margin = margin(18, 12, 5.5, 5.5)  # extra top margin for labels
        ) +
        # ---- top labels (color-matched) ----
annotate("text", x = 0.72, y = Inf, label = "EU 6%",  vjust = 1.3, size = 4.0, color = "darkgreen") +
        annotate("text", x = 0.95, y = Inf, label = sprintf("P80 %.1f%%", p80_val),
                 vjust = 1.3, size = 4.0, color = "steelblue4") +
        annotate("text", x = 1.18, y = Inf, label = sprintf("P90 %.1f%%", p90_val),
                 vjust = 1.3, size = 4.0, color = "orange3") +
        annotate("text", x = 1.40, y = Inf, label = "CH 20%", vjust = 1.3, size = 4.0, color = "firebrick3")


# ==================
# Panel B — Total THC
# ==================
n_THC     <- sum(!is.na(dat$THC))
n_THC_ge1 <- sum(dat$THC >= 1, na.rm = TRUE)
p_THC_ge1 <- 100 * n_THC_ge1 / max(1, n_THC)

pB <- ggplot(dat, aes(x = "", y = THC)) +
        geom_violin(width = 0.6, fill = "grey92", color = "black", alpha = 0.18, trim = FALSE) +
        geom_boxplot(width = 0.22, fill = "white", color = "black", outlier.shape = NA) +
        geom_jitter(width = 0.08, height = 0, shape = 21, size = 2.6,
                    stroke = 0.25, alpha = 0.9, fill = "firebrick", color = "black", na.rm = TRUE) +
        geom_hline(yintercept = 1.0, linetype = "dotted", linewidth = 0.9) +
        annotate("text", x = 1, y = Inf,
                 label = sprintf("n = %d/%d (%.0f%%) ≥ 1%%", n_THC_ge1, n_THC, p_THC_ge1),
                 vjust = 1.25, size = 4.0) +
        labs(title = "Total THC", x = "Samples", y = "Concentration (%)") +
        coord_cartesian(ylim = c(0, NA)) +
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title  = element_text(size = 15, face = "bold"),
              plot.title  = element_text(size = 16, face = "bold"))

# ==========================
# Panel C — CBD:THC ratio (log10)
# ==========================
ratio_df <- dat %>%
        filter(!is.na(CBD), !is.na(THC), THC > 0) %>%
        mutate(ratio = CBD / THC,
               rlog  = log10(ratio))

pct_ge20 <- 100 * mean(ratio_df$ratio >= 20, na.rm = TRUE)
ratio_breaks <- log10(c(20, 30, 50))
ratio_labels <- c("20:1", "30:1", "50:1")

pC <- ggplot(ratio_df, aes(x = "", y = rlog)) +
        geom_violin(width = 0.5, fill = NA, color = "black", trim = FALSE) +
        geom_jitter(width = 0.05, height = 0, size = 2.4, alpha = 0.85, color = "grey30") +
        geom_hline(yintercept = log10(20), linetype = "dotted") +
        geom_hline(yintercept = log10(30), linetype = "dotted") +
        annotate("text", x = 1, y = Inf,
                 label = sprintf("≥20:1 = %.0f%% of samples", pct_ge20),
                 vjust = 1.5, size = 4.0, fontface = "bold") +
        scale_y_continuous(
                breaks = ratio_breaks,
                labels = ratio_labels,
                expand = expansion(mult = c(0.05, 0.12))
        ) +
        labs(title = "CBD:THC ratio (log scale)", x = "Samples", y = "CBD:THC ratio (log10)") +
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_blank(),
              axis.title  = element_text(size = 15, face = "bold"),
              plot.title  = element_text(size = 16, face = "bold"))

# =========================================
# Panel D — THC distribution vs Swiss cut-offs
# =========================================
thc_groups <- dat %>%
        filter(!is.na(THC)) %>%
        mutate(group = cut(
                THC,
                breaks = c(-Inf, 0.3, 0.5, 1.0),
                labels = c("≤0.3%", "0.3–<0.5%", "0.5–<1.0%"),
                right = FALSE
        )) %>%
        count(group, name = "n") %>%
        mutate(prop = n / sum(n),
               lab  = scales::percent(prop, accuracy = 0.1))

pD <- ggplot(thc_groups, aes(x = group, y = prop)) +
        geom_col(width = 0.72, fill = "grey35") +
        geom_text(aes(label = lab), vjust = -0.35, size = 4.0) +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           limits = c(0, max(thc_groups$prop) * 1.15)) +
        labs(title = "THC distribution vs Swiss cut-offs",
             x = "THC (%) group", y = "Proportion of samples") +
        theme_classic(base_size = 14) +
        theme(axis.title  = element_text(size = 15, face = "bold"),
              plot.title  = element_text(size = 16, face = "bold"))

# --------------------------
# Arrange (2×2) and save
# --------------------------
fig_2x2 <- (pA | pB) / (pC | pD)
fig_2x2 <- fig_2x2 + plot_annotation(tag_levels = "A")

print(fig_2x2)

ggsave("Fig01_CBD_THC_Ratio_THCgroups.png", fig_2x2, width = 10.5, height = 8.0, dpi = 300)
ggsave("Fig01_CBD_THC_Ratio_THCgroups.pdf",  fig_2x2, width = 10.5, height = 8.0)
