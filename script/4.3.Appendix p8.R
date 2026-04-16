# ============================================================
# Sensitivity analysis: alternative abuse definitions
# Outcome: Mentalrisk (score >= 6)
# Logistic regression with adjusted OR (95% CI)
# Weekly working hours shown per 10-hour increase
# Input file: data/migrantworker(434)
# Output file: 4.3.Appendix p8.xlsx
# Final version
# ============================================================

# 1) Packages ------------------------------------------------
pkg <- c("readr", "readxl", "dplyr", "tidyr", "purrr", "broom", "openxlsx", "tibble")
new_pkg <- pkg[!sapply(pkg, requireNamespace, quietly = TRUE)]
if (length(new_pkg) > 0) install.packages(new_pkg)

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(openxlsx)
library(tibble)

# 2) Read data ----------------------------------------------
base_path <- "data/migrantworker(434)"

find_input_file <- function(x) {
  candidates <- c(
    x,
    paste0(x, ".csv"),
    paste0(x, ".xlsx"),
    paste0(x, ".xls"),
    paste0(x, ".rds")
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) stop("File not found. Please check path: ", x)
  hit
}

read_input <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv  = readr::read_csv(path, show_col_types = FALSE),
    xlsx = readxl::read_excel(path),
    xls  = readxl::read_excel(path),
    rds  = readRDS(path),
    stop("Unsupported file type: ", ext)
  )
}

data_file <- find_input_file(base_path)
df <- read_input(data_file)

# 3) Prepare analysis dataset -------------------------------
df2 <- df %>%
  transmute(
    Mentalrisk  = suppressWarnings(as.numeric(Mentalrisk)),
    Totalhour10 = suppressWarnings(as.numeric(Totalhour)) / 10,
    
    Psysocabuse = factor(suppressWarnings(as.numeric(Psysocabuse)), levels = c(0, 1)),
    Verabuse    = factor(suppressWarnings(as.numeric(Verabuse)),    levels = c(0, 1)),
    Psyabuse    = factor(suppressWarnings(as.numeric(Psyabuse)),    levels = c(0, 1)),
    
    Transfamily = factor(suppressWarnings(as.numeric(Transfamily)), levels = c(0, 1)),
    Emphousing  = factor(suppressWarnings(as.numeric(Emphousing)),  levels = c(0, 1)),
    
    Sex         = factor(suppressWarnings(as.numeric(Sex)), levels = c(1, 2)),
    Age         = suppressWarnings(as.numeric(Age)),
    
    Industry    = factor(suppressWarnings(as.numeric(Industry)), levels = c(1, 2, 3)),
    Edu_bin     = factor(ifelse(suppressWarnings(as.numeric(Edu)) == 3, 1, 0), levels = c(0, 1)),
    Married     = factor(suppressWarnings(as.numeric(Married)), levels = c(0, 1))
  ) %>%
  filter(complete.cases(.))

cat("Analysis sample size (complete cases):", nrow(df2), "\n")

# 4) Model formulas -----------------------------------------
formulas <- list(
  "Verbal or psychological abuse" =
    Mentalrisk ~ Totalhour10 + Psysocabuse + Transfamily + Emphousing +
    Sex + Age + Industry + Edu_bin + Married,
  
  "Only verbal abuse" =
    Mentalrisk ~ Totalhour10 + Verabuse + Transfamily + Emphousing +
    Sex + Age + Industry + Edu_bin + Married,
  
  "Only psychological abuse" =
    Mentalrisk ~ Totalhour10 + Psyabuse + Transfamily + Emphousing +
    Sex + Age + Industry + Edu_bin + Married
)

# 5) Labels and order ---------------------------------------
term_labels <- c(
  "Totalhour10"  = "Weekly working hours (per 10-hour increase)",
  "Psysocabuse1" = "Abuse exposure",
  "Verabuse1"    = "Verbal abuse only",
  "Psyabuse1"    = "Psychological abuse only",
  "Transfamily1" = "Transnational family",
  "Emphousing1"  = "Employer-controlled housing",
  "Sex2"         = "Sex (female vs male)",
  "Age"          = "Age (years)",
  "Industry2"    = "Industry: service sector vs manufacturing",
  "Industry3"    = "Industry: construction vs manufacturing",
  "Edu_bin1"     = "Education: postgraduate",
  "Married1"     = "Marital status: married vs single"
)

row_order <- c(
  "Weekly working hours (per 10-hour increase)",
  "Abuse exposure",
  "Verbal abuse only",
  "Psychological abuse only",
  "Transnational family",
  "Employer-controlled housing",
  "Sex (female vs male)",
  "Age (years)",
  "Industry: service sector vs manufacturing",
  "Industry: construction vs manufacturing",
  "Education: postgraduate",
  "Marital status: married vs single"
)

# 6) Helper for formatting ----------------------------------
fmt_or_ci <- function(est, low, high) {
  paste0(
    sprintf("%.2f", est), " (",
    sprintf("%.2f", low), "–",
    sprintf("%.2f", high), ")"
  )
}

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

# 7) Fit one model ------------------------------------------
fit_one_model <- function(formula, model_name) {
  fit <- glm(formula, data = df2, family = binomial)
  
  out <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      Predictor = unname(term_labels[term]),
      Predictor = ifelse(is.na(Predictor), term, Predictor),
      `OR (95% CI)` = fmt_or_ci(estimate, conf.low, conf.high),
      `p value` = fmt_p(p.value),
      Model = model_name
    ) %>%
    dplyr::select(Predictor, Model, `OR (95% CI)`, `p value`)
  
  out
}

# 8) Run all models -----------------------------------------
res_long <- purrr::imap_dfr(formulas, fit_one_model)

# 9) Reshape to wide ----------------------------------------
res_wide <- res_long %>%
  pivot_wider(
    names_from = Model,
    values_from = c(`OR (95% CI)`, `p value`),
    names_glue = "{Model}__{.value}"
  )

final_table <- tibble(Predictor = row_order) %>%
  left_join(res_wide, by = "Predictor")

needed_cols <- c(
  "Predictor",
  "Verbal or psychological abuse__OR (95% CI)",
  "Verbal or psychological abuse__p value",
  "Only verbal abuse__OR (95% CI)",
  "Only verbal abuse__p value",
  "Only psychological abuse__OR (95% CI)",
  "Only psychological abuse__p value"
)

for (nm in needed_cols) {
  if (!nm %in% names(final_table)) final_table[[nm]] <- NA_character_
}

final_table <- final_table %>%
  select(all_of(needed_cols)) %>%
  mutate(across(-Predictor, ~replace_na(.x, "—")))

# 10) Insert subgroup row -----------------------------------
subgroup_row <- tibble(
  Predictor = "Abuse exposure",
  `Verbal or psychological abuse__OR (95% CI)` = "",
  `Verbal or psychological abuse__p value` = "",
  `Only verbal abuse__OR (95% CI)` = "",
  `Only verbal abuse__p value` = "",
  `Only psychological abuse__OR (95% CI)` = "",
  `Only psychological abuse__p value` = ""
)

final_table <- bind_rows(
  final_table[1, ],
  subgroup_row,
  final_table[2:nrow(final_table), ]
)

# 11) Create Excel-ready matrix -----------------------------
excel_table <- rbind(
  c(
    "Predictor",
    "Verbal or psychological abuse", "",
    "Only verbal abuse", "",
    "Only psychological abuse", ""
  ),
  c(
    "",
    "OR (95% CI)", "p value",
    "OR (95% CI)", "p value",
    "OR (95% CI)", "p value"
  ),
  as.matrix(final_table)
)

# 12) Export to Excel ---------------------------------------
out_file <- "4.3.Appendix p8.xlsx"

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Table")
openxlsx::writeData(wb, sheet = "Table", x = excel_table, colNames = FALSE)

# Merge grouped headers
openxlsx::mergeCells(wb, "Table", cols = 2:3, rows = 1)
openxlsx::mergeCells(wb, "Table", cols = 4:5, rows = 1)
openxlsx::mergeCells(wb, "Table", cols = 6:7, rows = 1)

# Styles
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

subheader_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

body_center_style <- openxlsx::createStyle(
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

body_left_style <- openxlsx::createStyle(
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight"
)

subgroup_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight"
)

# Apply styles
n_rows <- nrow(excel_table)

openxlsx::addStyle(wb, "Table", header_style, rows = 1, cols = 1:7, gridExpand = TRUE)
openxlsx::addStyle(wb, "Table", subheader_style, rows = 2, cols = 1:7, gridExpand = TRUE)
openxlsx::addStyle(wb, "Table", body_left_style, rows = 3:n_rows, cols = 1, gridExpand = TRUE)
openxlsx::addStyle(wb, "Table", body_center_style, rows = 3:n_rows, cols = 2:7, gridExpand = TRUE)

# Highlight subgroup row
subgroup_excel_row <- which(excel_table[, 1] == "Abuse exposure")[1]
if (!is.na(subgroup_excel_row)) {
  openxlsx::addStyle(
    wb, "Table", subgroup_style,
    rows = subgroup_excel_row, cols = 1:7,
    gridExpand = TRUE, stack = TRUE
  )
}

# Set widths and freeze pane
openxlsx::setColWidths(wb, "Table", cols = 1, widths = 42)
openxlsx::setColWidths(wb, "Table", cols = 2:7, widths = 18)
openxlsx::setRowHeights(wb, "Table", rows = 1:2, heights = 22)
openxlsx::freezePane(wb, "Table", firstActiveRow = 3, firstActiveCol = 2)

# Save workbook
openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Excel file saved:", out_file, "\n")