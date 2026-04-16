############################################
## Appendix p3: Table 2 before PSM
## Data file: data/non-matched(3671).csv
## Output file: 2.2.Appendix p3 Table 2 before PSM.xlsx
############################################

## ---- 0) Packages ----
## If needed, install packages first:
## install.packages(c("readr", "openxlsx"))

library(readr)
library(openxlsx)

## ---- 1) Read data ----
file <- "data/non-matched(3671).csv"

if (!file.exists(file)) {
  stop("Data file not found: ", file)
}

dat <- read_csv(file, show_col_types = FALSE)

## ---- 2) Required variables ----
vars <- c(
  "Group", "Sex", "Age", "Industry", "Highedu",
  "Married", "Tiredness", "Mentalrisk", "Poorhealth"
)

missing_vars <- setdiff(vars, names(dat))
if (length(missing_vars) > 0) {
  stop(
    "Missing required variables:\n - ",
    paste(missing_vars, collapse = "\n - ")
  )
}

dat[vars] <- lapply(dat[vars], as.numeric)

## ---- 3) Split groups ----
g1 <- subset(dat, Group == 1)  # Taiwanese migrant workers in Vietnam
g0 <- subset(dat, Group == 0)  # Non-migrant workers in Taiwan

N1 <- nrow(g1)
N0 <- nrow(g0)

## ---- 4) Helper functions ----
fmt_n_pct <- function(n, N) {
  sprintf("%d (%.1f%%)", n, 100 * n / N)
}

fmt_mean_sd <- function(x) {
  sprintf("%.0f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

p_cat <- function(x, g) {
  keep <- complete.cases(x, g)
  tab <- table(x[keep], g[keep])
  
  if (min(dim(tab)) < 2) return(NA_real_)
  
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  
  if (any(chi$expected < 5)) {
    if (all(dim(tab) == c(2, 2))) {
      fisher.test(tab)$p.value
    } else {
      suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)
    }
  } else {
    chi$p.value
  }
}

make_row <- function(characteristic, mw, nmw, p) {
  data.frame(
    Characteristic = characteristic,
    MW = mw,
    NMW = nmw,
    `p value` = p,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

## ---- 5) Build table (Yes only where requested) ----
Table2 <- rbind(
  make_row(
    "Sex",
    "",
    "",
    fmt_p(p_cat(dat$Sex, dat$Group))
  ),
  make_row(
    "  Male",
    fmt_n_pct(sum(g1$Sex == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Sex == 1, na.rm = TRUE), N0),
    ""
  ),
  make_row(
    "  Female",
    fmt_n_pct(sum(g1$Sex == 2, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Sex == 2, na.rm = TRUE), N0),
    ""
  ),
  
  make_row(
    "Age, years",
    fmt_mean_sd(g1$Age),
    fmt_mean_sd(g0$Age),
    fmt_p(tryCatch(t.test(Age ~ Group, data = dat)$p.value, error = function(e) NA_real_))
  ),
  
  make_row(
    "Industry",
    "",
    "",
    fmt_p(p_cat(dat$Industry, dat$Group))
  ),
  make_row(
    "  Manufacturing",
    fmt_n_pct(sum(g1$Industry == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Industry == 1, na.rm = TRUE), N0),
    ""
  ),
  make_row(
    "  Service sector",
    fmt_n_pct(sum(g1$Industry == 2, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Industry == 2, na.rm = TRUE), N0),
    ""
  ),
  make_row(
    "  Construction",
    fmt_n_pct(sum(g1$Industry == 3, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Industry == 3, na.rm = TRUE), N0),
    ""
  ),
  
  make_row(
    "Educational attainment: college or above",
    fmt_n_pct(sum(g1$Highedu == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Highedu == 1, na.rm = TRUE), N0),
    fmt_p(p_cat(dat$Highedu, dat$Group))
  ),
  
  make_row(
    "Married",
    fmt_n_pct(sum(g1$Married == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Married == 1, na.rm = TRUE), N0),
    fmt_p(p_cat(dat$Married, dat$Group))
  ),
  
  make_row(
    "Severe fatigue",
    fmt_n_pct(sum(g1$Tiredness == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Tiredness == 1, na.rm = TRUE), N0),
    fmt_p(p_cat(dat$Tiredness, dat$Group))
  ),
  
  make_row(
    "Psychological distress",
    fmt_n_pct(sum(g1$Mentalrisk == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Mentalrisk == 1, na.rm = TRUE), N0),
    fmt_p(p_cat(dat$Mentalrisk, dat$Group))
  ),
  
  make_row(
    "Poor self-rated health",
    fmt_n_pct(sum(g1$Poorhealth == 1, na.rm = TRUE), N1),
    fmt_n_pct(sum(g0$Poorhealth == 1, na.rm = TRUE), N0),
    fmt_p(p_cat(dat$Poorhealth, dat$Group))
  )
)

colnames(Table2) <- c(
  "Characteristic",
  paste0("Taiwanese migrant workers in Vietnam (N=", N1, ")"),
  paste0("Non-migrant workers in Taiwan (N=", N0, ")"),
  "p value"
)

## ---- 6) Preview ----
print(Table2)
View(Table2)

## ---- 7) Export only Excel ----
output_file <- "2.2.Appendix p3 Table 2 before PSM.xlsx"

if (file.exists(output_file)) {
  file.remove(output_file)
}

wb <- createWorkbook()
addWorksheet(wb, "Table 2 before PSM")

writeData(wb, "Table 2 before PSM", Table2, startRow = 1, colNames = TRUE)

header_style <- createStyle(textDecoration = "bold", halign = "left", valign = "center")

addStyle(
  wb,
  "Table 2 before PSM",
  header_style,
  rows = 1,
  cols = 1:ncol(Table2),
  gridExpand = TRUE
)

setColWidths(wb, "Table 2 before PSM", cols = 1:ncol(Table2), widths = "auto")
freezePane(wb, "Table 2 before PSM", firstRow = TRUE)

saveWorkbook(wb, output_file, overwrite = TRUE)

message("Table exported successfully: ", file.path(getwd(), output_file))