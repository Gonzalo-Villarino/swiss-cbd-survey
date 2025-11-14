# ============================
# Fig. 3 — THC vs CBD (Pearson & Spearman) + CBD decarb histogram
# ============================

# ---- Paths ----
data_path  <- "/Users/gonzalovillarino/Documents_Local/Swizterland/Puregene/1.Research/2025/1_Projects/40_Swiss_Market/02_R_Codes_Fig_3/Fig.02b/02b_ORIGINAL_DC_SwissMarketStudy_Summary_Table.xlsx"
output_dir <- file.path(dirname(data_path), "outputs_fig02b")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- Packages ----
need <- function(pkgs){
        for (p in pkgs) {
                if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
                suppressPackageStartupMessages(library(p, character.only = TRUE))
        }
}
need(c("readxl","dplyr","ggplot2","broom","readr","patchwork","ggrepel","tibble"))

# ---- Helpers ----
numize <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
pctize <- function(x){
        v <- suppressWarnings(readr::parse_number(as.character(x)))
        if (all(is.na(v))) return(v)
        med <- suppressWarnings(stats::median(v, na.rm = TRUE))
        if (is.finite(med) && med <= 1) v <- v * 100  # convert fraction to %
        v
}
pretty_p <- function(p){
        if (is.na(p)) return("NA")
        if (p < 1e-6) return("< 1e-6")
        if (p < 1e-4) return("< 1e-4")
        if (p < 0.001) return("< 0.001")
        sprintf("= %.3f", p)
}

# ---- Read sheet ----
df <- readxl::read_excel(data_path)

# ---- Required columns ----
required <- c("Total_CBD", "Total_THC", "CBD_decarb_ratio_(%)")
miss <- setdiff(required, names(df))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))

# ---- Build analysis table (robust Sample_ID as character) ----
id_candidates <- c("Sample_ID","Sample","ID","id","sample","sampleid")
id_col <- id_candidates[id_candidates %in% names(df)][1]

if (!is.na(id_col)) {
        sample_id_vec <- as.character(df[[id_col]])
} else {
        sample_id_vec <- as.character(seq_len(nrow(df)))
}
fallback_ids <- sprintf("S%02d", seq_along(sample_id_vec))
sample_id_vec[is.na(sample_id_vec) | trimws(sample_id_vec)==""] <-
        fallback_ids[is.na(sample_id_vec) | trimws(sample_id_vec)==""]

dat <- tibble::tibble(
        Sample_ID      = sample_id_vec,
        total_cbd      = numize(df[["Total_CBD"]]),
        total_thc      = numize(df[["Total_THC"]]),
        cbd_decarb_pct = pctize(df[["CBD_decarb_ratio_(%)"]])
)

# ---- Font sizes ----
y_title_size <- 18; y_tick_size <- 14
x_title_size <- 14; x_tick_size <- 12

# --------------------------------------------------
# (A) THC vs CBD — Pearson & Spearman
# --------------------------------------------------
tc <- dat %>% dplyr::filter(!is.na(total_thc), !is.na(total_cbd))
if (nrow(tc) < 3) stop("Not enough non-NA rows for THC vs CBD plot.")

# Label high-CBD/low-THC outlier(s)
tc <- tc %>%
        mutate(highCBD_lowTHC = ifelse(total_cbd > 18 & total_thc < 0.5,
                                       "High-CBD/Low-THC outlier", "Other"))

# Pearson
lm_fit <- lm(total_thc ~ total_cbd, data = tc)
sm     <- summary(lm_fit)
R2     <- sm$r.squared
n      <- nrow(tc)
fstat  <- sm$fstatistic
pval_P <- pf(fstat["value"], fstat["numdf"], fstat["dendf"], lower.tail = FALSE)

# Spearman (rank-based; robust to outliers)
sp <- suppressWarnings(cor.test(tc$total_cbd, tc$total_thc, method = "spearman", exact = FALSE))
rho_S <- unname(sp$estimate)
pval_S <- sp$p.value

# Console summary
cat("---- CORRELATION SUMMARY (THC vs CBD) ----\n")
cat(sprintf("Pearson R²     = %.3f\n", R2))
cat(sprintf("Pearson p-val  = %.3g\n", pval_P))
cat(sprintf("Spearman rho   = %.3f\n", rho_S))
cat(sprintf("Spearman p-val = %.3g\n", pval_S))
cat(sprintf("n              = %d\n", n))
cat("-----------------------------------------\n")

# Plot
pA <- ggplot(tc, aes(x = total_cbd, y = total_thc)) +
        geom_point(aes(color = highCBD_lowTHC, shape = highCBD_lowTHC),
                   size = 3, alpha = 0.9) +
        geom_smooth(method = "lm", se = TRUE) +
        ggrepel::geom_text_repel(
                data = dplyr::filter(tc, highCBD_lowTHC == "High-CBD/Low-THC outlier"),
                aes(label = Sample_ID),
                size = 3.5, fontface = "bold", min.segment.length = 0
        ) +
        scale_color_manual(values = c("Other" = "black", "High-CBD/Low-THC outlier" = "red")) +
        scale_shape_manual(values = c("Other" = 16, "High-CBD/Low-THC outlier" = 17)) +
        annotate("text", x = -Inf, y = Inf,
                 label = sprintf("Pearson R² = %.2f | p %s | Spearman ρ = %.2f | p %s | n = %d",
                                 R2, pretty_p(pval_P), rho_S, pretty_p(pval_S), n),
                 hjust = -0.1, vjust = 1.3, size = 4.5, fontface = "bold") +
        labs(
                title = "THC vs CBD (Pearson & Spearman)",
                x = "Total CBD (%)", y = "Total THC (%)",
                color = NULL, shape = NULL
        ) +
        theme_bw() +
        theme(
                axis.title.y = element_text(size = y_title_size),
                axis.text.y  = element_text(size = y_tick_size),
                axis.title.x = element_text(size = x_title_size),
                axis.text.x  = element_text(size = x_tick_size),
                legend.position = "top"
        )

# --------------------------------------------------
# (B) CBD decarboxylation histogram — aging proxy
# --------------------------------------------------
dec <- dat %>% dplyr::filter(!is.na(cbd_decarb_pct))
if (nrow(dec) < 3) stop("Not enough non-NA rows for CBD decarboxylation histogram.")

med_val  <- median(dec$cbd_decarb_pct, na.rm = TRUE)
mean_val <- mean(dec$cbd_decarb_pct, na.rm = TRUE)

# Standardize bin labels to match manuscript (Low/Moderate/High)
dec <- dec %>%
        mutate(dec_bin = cut(
                cbd_decarb_pct,
                breaks = c(-Inf, 20, 40, Inf),
                labels = c("Low (≤20%)", "Moderate (20–40%)", "High (>40%)"),
                right = TRUE, include.lowest = TRUE
        ))

low_ct  <- sum(dec$dec_bin == "Low (≤20%)", na.rm = TRUE)
mod_ct  <- sum(dec$dec_bin == "Moderate (20–40%)", na.rm = TRUE)
high_ct <- sum(dec$dec_bin == "High (>40%)", na.rm = TRUE)

pB <- ggplot(dec, aes(x = cbd_decarb_pct)) +
        geom_histogram(binwidth = 5, boundary = 0, closed = "left", alpha = 0.9) +
        geom_vline(xintercept = med_val, linetype = "dashed") +
        geom_vline(xintercept = c(20, 40), linetype = "dotted") +
        annotate("text", x = 10, y = Inf, label = "Low",      vjust = 1.5, fontface = "bold") +
        annotate("text", x = 30, y = Inf, label = "Moderate", vjust = 1.5, fontface = "bold") +
        annotate("text", x = 60, y = Inf, label = "High",     vjust = 1.5, fontface = "bold") +
        scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 10)) +
        labs(
                title    = "CBD decarboxylation (%) — aging proxy",
                x        = "CBD decarboxylation (%)",
                y        = "Sample count",
                subtitle = sprintf("Median = %.1f%% | Mean = %.1f%% | n = %d | L=%d | M=%d | H=%d",
                                   med_val, mean_val, nrow(dec), low_ct, mod_ct, high_ct)
        ) +
        theme_bw() +
        theme(
                axis.title.y = element_text(size = y_title_size),
                axis.text.y  = element_text(size = y_tick_size),
                axis.title.x = element_text(size = x_title_size),
                axis.text.x  = element_text(size = x_tick_size)
        )

# --------------------------------------------------
# Combine and save
# --------------------------------------------------
composite <- pA + pB + patchwork::plot_layout(ncol = 2, widths = c(1, 1)) +
        patchwork::plot_annotation(tag_levels = "A")

png_fp  <- file.path(output_dir, "Composite_THCvsCBD__CBD_Decarb.png")
pdf_fp  <- file.path(output_dir, "Composite_THCvsCBD__CBD_Decarb.pdf")

ggsave(png_fp, composite, width = 12, height = 5.8, dpi = 300)
ggsave(pdf_fp, composite, width = 12, height = 5.8)

message("Saved: ", png_fp)
message("Saved: ", pdf_fp)
