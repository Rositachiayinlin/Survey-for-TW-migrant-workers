############################################
## Appendix p4: Table 3 non-match
## Data file: data/non-matched(3671).csv
## Output file: 3.1.Appendix p4 Table 3 non-match.xlsx
############################################

## ---- 0) Packages ----
## If needed, install packages first:
## install.packages(c("readr", "dplyr", "broom", "openxlsx"))

library(readr)
library(dplyr)
library(broom)
library(openxlsx)

## ---- 1) Read data ----
file <- "data/non-matched(3671).csv"

if (!file.exists(file)) {
  stop("Data file not found: ", file)
}

dat <- read_csv(file, show_col_types = FALSE) %>%
  transmute(
    Tiredness  = as.numeric(Tiredness),
    Mentalrisk = as.numeric(Mentalrisk),
    Poorhealth = as.numeric(Poorhealth),
    Group = factor(
      Group,
      levels = c(0, 1),
      labels = c(
        "Non-migrant workers in Taiwan",
        "Taiwanese migrant workers in Vietnam"
      )
    ),
    Sex = factor(Sex, c(1, 2), c("Male", "Female")),
    Age = as.numeric(Age),
    Industry = factor(
      Industry, c(1, 2, 3),
      c("Manufacturing", "Service sector", "Construction")
    ),
    Edu = factor(
      Edu, c(1, 2, 3),
      c("High school and below",
        "University or college",
        "Postgraduate")
    ),
    Married = factor(Married, c(0, 1), c("Single", "Married"))
  ) %>%
  na.omit()

## ---- 2) Helper functions ----
fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

fmt_ci <- function(or, lo, hi) {
  sprintf("%.2f (%.2f to %.2f)", or, lo, hi)
}

## ---- 3) Model function ----
run_model <- function(outcome, outcome_label) {
  glm(
    as.formula(paste(outcome, "~ Group + Sex + Age + Industry + Edu + Married")),
    data = dat,
    family = binomial()
  ) %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Outcome = outcome_label,
      Variable = case_when(
        term == "GroupTaiwanese migrant workers in Vietnam" ~ "Group",
        term == "SexFemale" ~ "Sex",
        term == "Age" ~ "Age",
        term %in% c("IndustryService sector", "IndustryConstruction") ~ "Industry",
        term %in% c("EduUniversity or college", "EduPostgraduate") ~ "Educational attainment",
        term == "MarriedMarried" ~ "Marital status",
        TRUE ~ term
      ),
      Comparison = case_when(
        term == "GroupTaiwanese migrant workers in Vietnam" ~
          "Taiwanese migrant workers in Vietnam (ref: non-migrant workers in Taiwan)",
        term == "SexFemale" ~
          "Female (ref: Male)",
        term == "Age" ~
          "Per 1-year increase",
        term == "IndustryService sector" ~
          "Service sector (ref: Manufacturing)",
        term == "IndustryConstruction" ~
          "Construction (ref: Manufacturing)",
        term == "EduUniversity or college" ~
          "University or college (ref: High school and below)",
        term == "EduPostgraduate" ~
          "Postgraduate (ref: High school and below)",
        term == "MarriedMarried" ~
          "Married (ref: Single)",
        TRUE ~ term
      ),
      OR_num = estimate,
      `Adjusted OR` = sprintf("%.2f", estimate),
      `95% CI` = fmt_ci(estimate, conf.low, conf.high),
      `p value` = fmt_p(p.value)
    ) %>%
    select(Outcome, Variable, Comparison, OR_num, `Adjusted OR`, `95% CI`, `p value`)
}

## ---- 4) Run all models ----
table_full_raw <- bind_rows(
  run_model("Tiredness", "Severe fatigue"),
  run_model("Mentalrisk", "Psychological distress"),
  run_model("Poorhealth", "Poor self-rated health")
)

## ---- 5) Reorder tables ----
outcome_order <- c(
  "Severe fatigue",
  "Psychological distress",
  "Poor self-rated health"
)

variable_order <- c(
  "Group",
  "Sex",
  "Age",
  "Industry",
  "Educational attainment",
  "Marital status"
)

comparison_order <- c(
  "Taiwanese migrant workers in Vietnam (ref: non-migrant workers in Taiwan)",
  "Female (ref: Male)",
  "Per 1-year increase",
  "Service sector (ref: Manufacturing)",
  "Construction (ref: Manufacturing)",
  "University or college (ref: High school and below)",
  "Postgraduate (ref: High school and below)",
  "Married (ref: Single)"
)

table_full <- table_full_raw %>%
  mutate(
    Outcome = factor(Outcome, levels = outcome_order),
    Variable = factor(Variable, levels = variable_order),
    Comparison = factor(Comparison, levels = comparison_order)
  ) %>%
  arrange(Outcome, Variable, Comparison) %>%
  mutate(
    Outcome = as.character(Outcome),
    Variable = as.character(Variable),
    Comparison = as.character(Comparison)
  ) %>%
  select(Outcome, Variable, Comparison, `Adjusted OR`, `95% CI`, `p value`)

table_main <- table_full %>%
  filter(Variable == "Group") %>%
  mutate(`Adjusted covariates` = "Sex, age, industry, educational attainment, marital status") %>%
  select(
    Outcome,
    Exposure = Comparison,
    `Adjusted OR`,
    `95% CI`,
    `p value`,
    `Adjusted covariates`
  )

table_numeric <- table_full_raw %>%
  mutate(
    Outcome = factor(Outcome, levels = outcome_order),
    Variable = factor(Variable, levels = variable_order),
    Comparison = factor(Comparison, levels = comparison_order)
  ) %>%
  arrange(Outcome, Variable, Comparison) %>%
  mutate(
    Outcome = as.character(Outcome),
    Variable = as.character(Variable),
    Comparison = as.character(Comparison)
  ) %>%
  select(Outcome, Variable, Comparison, OR_num, `Adjusted OR`, `95% CI`, `p value`)

## ---- 6) Preview ----
print(table_main)
print(table_full)

## ---- 7) Export only one Excel file ----
output_file <- "3.1.Appendix p4 Table 3 non-match.xlsx"

if (file.exists(output_file)) {
  file.remove(output_file)
}

wb <- createWorkbook()

addWorksheet(wb, "Main results")
addWorksheet(wb, "Full regression table")
addWorksheet(wb, "Numeric table")

writeData(wb, "Main results", table_main, startRow = 1, colNames = TRUE)
writeData(wb, "Full regression table", table_full, startRow = 1, colNames = TRUE)
writeData(wb, "Numeric table", table_numeric, startRow = 1, colNames = TRUE)

header_style <- createStyle(textDecoration = "bold", halign = "left", valign = "center")

addStyle(wb, "Main results", header_style, rows = 1, cols = 1:ncol(table_main), gridExpand = TRUE)
addStyle(wb, "Full regression table", header_style, rows = 1, cols = 1:ncol(table_full), gridExpand = TRUE)
addStyle(wb, "Numeric table", header_style, rows = 1, cols = 1:ncol(table_numeric), gridExpand = TRUE)

setColWidths(wb, "Main results", cols = 1:ncol(table_main), widths = "auto")
setColWidths(wb, "Full regression table", cols = 1:ncol(table_full), widths = "auto")
setColWidths(wb, "Numeric table", cols = 1:ncol(table_numeric), widths = "auto")

freezePane(wb, "Main results", firstRow = TRUE)
freezePane(wb, "Full regression table", firstRow = TRUE)
freezePane(wb, "Numeric table", firstRow = TRUE)

saveWorkbook(wb, output_file, overwrite = TRUE)

message("Table exported successfully: ", file.path(getwd(), output_file))