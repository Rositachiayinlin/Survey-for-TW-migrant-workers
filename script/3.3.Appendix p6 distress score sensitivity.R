############################################
## Appendix p6: Distress score sensitivity
## Data file: data/matchedsample(1202).csv
## Output file: 3.3.Appendix p6 distress score sensitivity.xlsx
############################################

## ---- 0) Packages ----
## If needed, install packages first:
## install.packages(c("readr", "dplyr", "quantreg", "openxlsx", "tibble"))

library(readr)
library(dplyr)
library(quantreg)
library(openxlsx)
library(tibble)

## ---- 1) Read data ----
file <- "data/matchedsample(1202).csv"

if (!file.exists(file)) {
  stop("Data file not found: ", file)
}

raw_dat <- read_csv(file, show_col_types = FALSE)

## ---- 2) Prepare data for quantile regression ----
dat_qr <- raw_dat %>%
  transmute(
    Mentalsum = as.numeric(Mentalsum),
    Group = factor(
      as.numeric(Group),
      levels = c(0, 1),
      labels = c(
        "Non-migrant workers in Taiwan",
        "Taiwanese migrant workers in Vietnam"
      )
    ),
    Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")),
    Age = as.numeric(Age),
    Industry = factor(
      Industry,
      levels = c(1, 2, 3),
      labels = c("Manufacturing", "Service sector", "Construction")
    ),
    Edu = factor(
      Edu,
      levels = c(1, 2, 3),
      labels = c(
        "High school and below",
        "University or college",
        "Postgraduate"
      )
    ),
    Married = factor(Married, levels = c(0, 1), labels = c("Single", "Married"))
  ) %>%
  na.omit()

## ---- 3) Prepare data for normality check ----
dat_norm <- raw_dat %>%
  transmute(
    Group = as.numeric(Group),
    Mentalsum = as.numeric(Mentalsum)
  ) %>%
  na.omit()

## ---- 4) Descriptive statistics ----
desc_table <- dat_qr %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    median = median(Mentalsum),
    Q1 = quantile(Mentalsum, 0.25),
    Q3 = quantile(Mentalsum, 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    median = sprintf("%.2f", median),
    Q1 = sprintf("%.2f", Q1),
    Q3 = sprintf("%.2f", Q3)
  )

## ---- 5) Quantile regression ----
fit <- rq(
  Mentalsum ~ Group + Sex + Age + Industry + Edu + Married,
  tau = 0.5,
  data = dat_qr
)

res_qr <- summary(fit, se = "nid")$coefficients %>%
  as.data.frame()

colnames(res_qr) <- c("Estimate", "SE", "t", "p")

res_qr <- res_qr %>%
  rownames_to_column("term") %>%
  mutate(
    CI_low = Estimate - 1.96 * SE,
    CI_high = Estimate + 1.96 * SE
  )

## ---- 6) Helper functions ----
fmt <- function(x) sprintf("%.2f", x)

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

## ---- 7) Report table ----
table_report <- res_qr %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "GroupTaiwanese migrant workers in Vietnam" ~ "Migrant vs non-migrant",
      term == "SexFemale" ~ "Female vs Male",
      term == "Age" ~ "Age (years)",
      term == "IndustryService sector" ~ "Service vs Manufacturing",
      term == "IndustryConstruction" ~ "Construction vs Manufacturing",
      term == "EduUniversity or college" ~ "University vs High school",
      term == "EduPostgraduate" ~ "Postgraduate vs High school",
      term == "MarriedMarried" ~ "Married vs Single",
      TRUE ~ term
    ),
    `Adjusted median difference` = fmt(Estimate),
    `95% CI` = paste0(fmt(CI_low), " to ", fmt(CI_high)),
    `p value` = fmt_p(p)
  ) %>%
  select(Predictor, `Adjusted median difference`, `95% CI`, `p value`)

## ---- 8) Normality check ----
g1 <- dat_norm %>% filter(Group == 1) %>% pull(Mentalsum)
g0 <- dat_norm %>% filter(Group == 0) %>% pull(Mentalsum)

shapiro_res <- function(x, label) {
  if (length(x) < 3 || length(x) > 5000) {
    return(data.frame(
      Group = label,
      n = length(x),
      W = NA_real_,
      p = NA_real_
    ))
  }
  s <- shapiro.test(x)
  data.frame(
    Group = label,
    n = length(x),
    W = unname(s$statistic),
    p = s$p.value
  )
}

normality_table <- bind_rows(
  shapiro_res(g1, "Taiwanese migrant workers in Vietnam"),
  shapiro_res(g0, "Non-migrant workers in Taiwan")
) %>%
  mutate(
    W = ifelse(is.na(W), "", sprintf("%.4f", W)),
    p = ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  )

## ---- 9) Create normality plots ----
plot_file <- "3.3.Appendix p6 distress score sensitivity_plots.png"

if (file.exists(plot_file)) {
  file.remove(plot_file)
}

png(filename = plot_file, width = 1600, height = 1200, res = 150)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

hist(
  g1,
  main = "Histogram: Mentalsum (Group 1)",
  xlab = "Mentalsum",
  ylab = "Frequency"
)

qqnorm(
  g1,
  main = "Q-Q Plot: Mentalsum (Group 1)",
  ylab = "Sample Quantiles",
  xlab = "Theoretical Quantiles"
)
qqline(g1)

hist(
  g0,
  main = "Histogram: Mentalsum (Group 0)",
  xlab = "Mentalsum",
  ylab = "Frequency"
)

qqnorm(
  g0,
  main = "Q-Q Plot: Mentalsum (Group 0)",
  ylab = "Sample Quantiles",
  xlab = "Theoretical Quantiles"
)
qqline(g0)

dev.off()

## ---- 10) Export only one Excel file ----
output_file <- "3.3.Appendix p6 distress score sensitivity.xlsx"

if (file.exists(output_file)) {
  file.remove(output_file)
}

wb <- createWorkbook()

addWorksheet(wb, "QR report")
addWorksheet(wb, "QR full")
addWorksheet(wb, "Descriptive stats")
addWorksheet(wb, "Normality")
addWorksheet(wb, "Normality plots")

writeData(wb, "QR report", table_report, startRow = 1, colNames = TRUE)
writeData(wb, "QR full", res_qr, startRow = 1, colNames = TRUE)
writeData(wb, "Descriptive stats", desc_table, startRow = 1, colNames = TRUE)
writeData(wb, "Normality", normality_table, startRow = 1, colNames = TRUE)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "left",
  valign = "center"
)

addStyle(wb, "QR report", header_style, rows = 1, cols = 1:ncol(table_report), gridExpand = TRUE)
addStyle(wb, "QR full", header_style, rows = 1, cols = 1:ncol(res_qr), gridExpand = TRUE)
addStyle(wb, "Descriptive stats", header_style, rows = 1, cols = 1:ncol(desc_table), gridExpand = TRUE)
addStyle(wb, "Normality", header_style, rows = 1, cols = 1:ncol(normality_table), gridExpand = TRUE)

setColWidths(wb, "QR report", cols = 1:ncol(table_report), widths = "auto")
setColWidths(wb, "QR full", cols = 1:ncol(res_qr), widths = "auto")
setColWidths(wb, "Descriptive stats", cols = 1:ncol(desc_table), widths = "auto")
setColWidths(wb, "Normality", cols = 1:ncol(normality_table), widths = "auto")

freezePane(wb, "QR report", firstRow = TRUE)
freezePane(wb, "QR full", firstRow = TRUE)
freezePane(wb, "Descriptive stats", firstRow = TRUE)
freezePane(wb, "Normality", firstRow = TRUE)

insertImage(
  wb,
  sheet = "Normality plots",
  file = plot_file,
  startRow = 1,
  startCol = 1,
  width = 10.5,
  height = 8,
  units = "in"
)

saveWorkbook(wb, output_file, overwrite = TRUE)

## ---- 11) Print ----
print(table_report)
print(desc_table)
print(normality_table)

message("Distress score sensitivity results exported successfully: ", file.path(getwd(), output_file))