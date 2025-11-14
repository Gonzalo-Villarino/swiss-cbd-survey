# ==== Fig 2 (CBD-only): Vendor CBD claims vs measured CBD (Swiss CBD market) ====
# Vendor = green fill; Measured HPLC = red outline; "*" marks missing vendor claims.

rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(janitor)

# ---- File ----
data_file <- "/Users/gonzalovillarino/Documents_Local/Swizterland/Puregene/1.Research/2025/1_Projects/40_Swiss_Market/02_R_Codes_Fig_2/01_GV_SwissMarketStudy_Summary_Table.xlsx"

# ---------- Read & clean ----------
dat_raw <- read_excel(data_file)
names(dat_raw) <- janitor::make_clean_names(names(dat_raw))

numify <- function(x) suppressWarnings(as.numeric(str_replace_all(as.character(x), "[^0-9.\\-]", "")))

pick_by <- function(nm, patterns) {
        i <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, nm, ignore.case = TRUE))))
        if (length(i)) i[1] else NA_integer_
}
nm <- names(dat_raw)

i_sample <- pick_by(nm, c("^sample","sample_id","id"))
i_claimC <- pick_by(nm, c("label.*cbd","declared.*cbd","claim.*cbd"))
i_measC  <- pick_by(nm, c("^hplc.*cbd","cbd_.*hplc","measured.*cbd"))

dat <- tibble(
        Sample_ID         = if (!is.na(i_sample)) dat_raw[[i_sample]] else seq_len(nrow(dat_raw)),
        Label_claim_CBD   = if (!is.na(i_claimC)) numify(dat_raw[[i_claimC]]) else NA_real_,
        HPLC_CBD_measured = if (!is.na(i_measC))  numify(dat_raw[[i_measC]])  else NA_real_
)

# ---------- Order & anonymize ----------
dat <- dat %>%
        mutate(Sample_ID = paste0("S", Sample_ID)) %>%
        arrange(HPLC_CBD_measured, Label_claim_CBD) %>%
        mutate(Sample_ID = factor(Sample_ID, levels = Sample_ID))

# ---------- Ghost bar for missing claims ----------
eps <- 0.2
dat <- dat %>%
        mutate(
                Claim_plot = ifelse(is.na(Label_claim_CBD), eps, Label_claim_CBD),
                no_claim   = is.na(Label_claim_CBD)
        )

# ---------- Y max from CBD only ----------
y_max <- max(c(dat$HPLC_CBD_measured, dat$Claim_plot), na.rm = TRUE) * 1.10

# ---------- Plot (CBD only) ----------
p <- ggplot(dat, aes(x = Sample_ID)) +
        # Vendor claim = GREEN FILL
        geom_col(aes(y = Claim_plot, fill = "Vendor CBD (label)"),
                 width = 0.8, color = NA, na.rm = TRUE) +
        
        # Measured CBD = RED OUTLINE
        geom_col(aes(y = HPLC_CBD_measured, color = "Measured CBD (HPLC)"),
                 width = 0.8, fill = NA, linewidth = 0.8, na.rm = TRUE) +
        
        # "*" markers for samples lacking vendor claim
        geom_point(
                data = subset(dat, no_claim),
                aes(x = Sample_ID, y = HPLC_CBD_measured + 0.5, shape = "* No vendor claim"),
                inherit.aes = FALSE, size = 2.2, colour = "black", show.legend = TRUE
        ) +
        
        # Scales & legends
        scale_fill_manual(values = c("Vendor CBD (label)" = "seagreen3"), name = NULL) +
        scale_color_manual(values = c("Measured CBD (HPLC)" = "red3"), name = NULL) +
        scale_shape_manual(values = c("* No vendor claim" = 8), name = NULL) +
        
        scale_y_continuous(
                name = "CBD (%)",
                limits = c(0, y_max),
                breaks = scales::pretty_breaks()
        ) +
        labs(
                x = "Sample ID",
                title = "",
                subtitle = "Asterisks mark samples lacking vendor CBD claims"
        ) +
        theme_minimal(base_size = 12) +
        theme(
                plot.title    = element_text(face = "bold", size = 14, hjust = 0),
                plot.subtitle = element_text(size = 11, hjust = 0),
                axis.text.x   = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
                axis.title.y  = element_text(face = "bold", size = 15),
                axis.text.y   = element_text(size = 13),
                panel.grid.major.x = element_blank(),
                legend.position = "top",
                legend.box = "horizontal",
                legend.direction = "horizontal"
        ) +
        guides(
                fill  = guide_legend(nrow = 1, byrow = TRUE, order = 1),
                color = guide_legend(nrow = 1, byrow = TRUE, order = 2),
                shape = guide_legend(nrow = 1, byrow = TRUE, order = 3)
        )

print(p)

## ===== Optional stats (CBD only) =====
dat_stats <- dat %>%
        filter(!is.na(Label_claim_CBD), !is.na(HPLC_CBD_measured))

diffs <- dat_stats$Label_claim_CBD - dat_stats$HPLC_CBD_measured
mean_diff <- mean(diffs, na.rm = TRUE)
sd_diff   <- sd(diffs, na.rm = TRUE)

t_res <- t.test(dat_stats$Label_claim_CBD,
                dat_stats$HPLC_CBD_measured,
                paired = TRUE)

w_res <- wilcox.test(dat_stats$Label_claim_CBD,
                     dat_stats$HPLC_CBD_measured,
                     paired = TRUE)

cat("=== Vendor claim vs measured CBD ===\n")
cat(sprintf("n = %d\n", nrow(dat_stats)))
cat(sprintf("Mean difference (Claim - Measured) = %.2f ± %.2f %%\n",
            mean_diff, sd_diff))
cat(sprintf("Paired t-test: mean = %.2f%%, 95%% CI [%.2f, %.2f], t = %.2f, df = %d, p = %.4g\n",
            mean_diff, t_res$conf.int[1], t_res$conf.int[2],
            t_res$statistic, t_res$parameter, t_res$p.value))
cat(sprintf("Wilcoxon signed-rank test: V = %d, p = %.4g\n",
            w_res$statistic, w_res$p.value))

# ggsave("Fig2_CBD_only_vendor_vs_measured.png", p, width = 13, height = 6, dpi = 300)
