library(readr)

# Read data
dat <- read_csv("data/non-matched(3671).csv", show_col_types = FALSE)

# Required variables
vars <- c("Group", "Sex", "Age", "Industry", "Highedu",
          "Married", "Tiredness", "Mentalrisk", "Poorhealth")
stopifnot(all(vars %in% names(dat)))

dat[vars] <- lapply(dat[vars], as.numeric)

# Split groups
g1 <- subset(dat, Group == 1)  # Taiwanese migrant workers in Vietnam
g0 <- subset(dat, Group == 0)  # Non-migrant workers in Taiwan
N1 <- nrow(g1)
N0 <- nrow(g0)

# Helper functions
fmt_n_pct <- function(n, N) sprintf("%d (%.1f%%)", n, 100 * n / N)
fmt_mean_sd <- function(x) sprintf("%.0f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

p_cat <- function(x, g) {
  keep <- complete.cases(x, g)
  tab <- table(x[keep], g[keep])
  if (min(dim(tab)) < 2) return(NA_real_)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  if (any(chi$expected < 5)) {
    if (all(dim(tab) == c(2, 2))) fisher.test(tab)$p.value
    else suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)
  } else {
    chi$p.value
  }
}

# Build table
Table2 <- rbind(
  data.frame(Characteristic = "Sex", VN = "", TW = "", p = fmt_p(p_cat(dat$Sex, dat$Group))),
  data.frame(Characteristic = "  Male",
             VN = fmt_n_pct(sum(g1$Sex == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Sex == 1, na.rm = TRUE), N0), p = ""),
  data.frame(Characteristic = "  Female",
             VN = fmt_n_pct(sum(g1$Sex == 2, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Sex == 2, na.rm = TRUE), N0), p = ""),
  
  data.frame(Characteristic = "Age, years",
             VN = fmt_mean_sd(g1$Age),
             TW = fmt_mean_sd(g0$Age),
             p = fmt_p(tryCatch(t.test(Age ~ Group, data = dat)$p.value, error = function(e) NA_real_))),
  
  data.frame(Characteristic = "Industry", VN = "", TW = "", p = fmt_p(p_cat(dat$Industry, dat$Group))),
  data.frame(Characteristic = "  Manufacturing",
             VN = fmt_n_pct(sum(g1$Industry == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Industry == 1, na.rm = TRUE), N0), p = ""),
  data.frame(Characteristic = "  Service sector",
             VN = fmt_n_pct(sum(g1$Industry == 2, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Industry == 2, na.rm = TRUE), N0), p = ""),
  data.frame(Characteristic = "  Construction",
             VN = fmt_n_pct(sum(g1$Industry == 3, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Industry == 3, na.rm = TRUE), N0), p = ""),
  
  data.frame(Characteristic = "Higher education, college or above",
             VN = fmt_n_pct(sum(g1$Highedu == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Highedu == 1, na.rm = TRUE), N0),
             p = fmt_p(p_cat(dat$Highedu, dat$Group))),
  
  data.frame(Characteristic = "Married",
             VN = fmt_n_pct(sum(g1$Married == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Married == 1, na.rm = TRUE), N0),
             p = fmt_p(p_cat(dat$Married, dat$Group))),
  
  data.frame(Characteristic = "Severe fatigue, score ≥70",
             VN = fmt_n_pct(sum(g1$Tiredness == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Tiredness == 1, na.rm = TRUE), N0),
             p = fmt_p(p_cat(dat$Tiredness, dat$Group))),
  
  data.frame(Characteristic = "Psychological distress, score ≥6",
             VN = fmt_n_pct(sum(g1$Mentalrisk == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Mentalrisk == 1, na.rm = TRUE), N0),
             p = fmt_p(p_cat(dat$Mentalrisk, dat$Group))),
  
  data.frame(Characteristic = "Self-rated poor health",
             VN = fmt_n_pct(sum(g1$Poorhealth == 1, na.rm = TRUE), N1),
             TW = fmt_n_pct(sum(g0$Poorhealth == 1, na.rm = TRUE), N0),
             p = fmt_p(p_cat(dat$Poorhealth, dat$Group)))
)

colnames(Table2) <- c(
  "Characteristic",
  paste0("Taiwanese migrant workers in Vietnam (N=", N1, ")"),
  paste0("Non-migrant workers in Taiwan (N=", N0, ")"),
  "p value"
)

print(Table2)
write.csv(Table2, "Table2_group_comparison.csv", row.names = FALSE)