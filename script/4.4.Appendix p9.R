# ==========================================
# Sensitivity analysis (adjusted model only)
# Outcome: Mentalrisk (0/1)
# Exposure: TransFamily3 (ref = single)
# Weekly working hours: per 10-hour increase
# Input:  data/migrantworker(434).csv
# Output: 4.4.Appendix p9.xlsx
# ==========================================

# 0) Packages
pkg <- c("readr", "dplyr", "broom", "openxlsx")
new_pkg <- pkg[!sapply(pkg, requireNamespace, quietly = TRUE)]
if (length(new_pkg) > 0) install.packages(new_pkg)

library(readr)
library(dplyr)
library(broom)
library(openxlsx)

# 1) Load data
df <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# 2) Recode variables
df2 <- df %>%
  mutate(
    Mentalrisk = as.numeric(Mentalrisk),
    Totalhour10 = as.numeric(Totalhour) / 10,
    Married = as.numeric(Married),
    Vnfamily = as.numeric(Vnfamily),
    Age = as.numeric(Age),
    
    TransFamily3 = case_when(
      Married == 0 ~ "single",
      Married == 1 & Vnfamily == 0 ~ "married, no transnational family",
      Married == 1 & Vnfamily == 1 ~ "married, transnational family",
      TRUE ~ NA_character_
    ),
    TransFamily3 = factor(
      TransFamily3,
      levels = c(
        "single",
        "married, no transnational family",
        "married, transnational family"
      )
    ),
    
    Edu_bin = factor(
      ifelse(Edu == 3, "postgraduate", "non-postgraduate"),
      levels = c("non-postgraduate", "postgraduate")
    ),
    
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("no", "yes")),
    Emphousing  = factor(Emphousing,  levels = c(0, 1), labels = c("no", "yes")),
    Sex         = factor(Sex, levels = c(1, 2), labels = c("male", "female")),
    Industry    = factor(
      Industry,
      levels = c(1, 2, 3),
      labels = c("Manufacturing", "Service sector", "Construction")
    )
  ) %>%
  filter(complete.cases(
    Mentalrisk, Totalhour10, Psysocabuse, TransFamily3,
    Emphousing, Sex, Age, Industry, Edu_bin
  ))

# 3) Run adjusted logistic regression
fit <- glm(
  Mentalrisk ~ Totalhour10 + Psysocabuse + TransFamily3 +
    Emphousing + Sex + Age + Industry + Edu_bin,
  data = df2,
  family = binomial
)

# 4) Format helpers
fmt_orci <- function(or, lcl, ucl) {
  paste0(
    sprintf("%.2f", or), " (",
    sprintf("%.2f", lcl), "–",
    sprintf("%.2f", ucl), ")"
  )
}

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

label_map <- c(
  "Totalhour10" = "Weekly working hours (per 10-hour increase)",
  "Psysocabuseyes" = "Verbal or psychological abuse (yes vs no)",
  "TransFamily3married, no transnational family" =
    "Transnational family status: married without transnational family vs single",
  "TransFamily3married, transnational family" =
    "Transnational family status: married with transnational family vs single",
  "Emphousingyes" = "Employer-controlled housing (yes vs no)",
  "Sexfemale" = "Sex (female vs male)",
  "Age" = "Age (years)",
  "IndustryService sector" = "Industry: service sector vs manufacturing",
  "IndustryConstruction" = "Industry: construction vs manufacturing",
  "Edu_binpostgraduate" = "Education: postgraduate vs non-postgraduate"
)

desired_order <- c(
  "Weekly working hours (per 10-hour increase)",
  "Verbal or psychological abuse (yes vs no)",
  "Transnational family status: married without transnational family vs single",
  "Transnational family status: married with transnational family vs single",
  "Employer-controlled housing (yes vs no)",
  "Sex (female vs male)",
  "Age (years)",
  "Industry: service sector vs manufacturing",
  "Industry: construction vs manufacturing",
  "Education: postgraduate vs non-postgraduate"
)

# 5) Build output table
tab <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Predictor = unname(label_map[term]),
    `Adjusted OR (95% CI)` = fmt_orci(estimate, conf.low, conf.high),
    `p value` = fmt_p(p.value)
  ) %>%
  mutate(
    Predictor = ifelse(is.na(Predictor), term, Predictor),
    Predictor = factor(Predictor, levels = desired_order)
  ) %>%
  arrange(Predictor) %>%
  mutate(Predictor = as.character(Predictor))

# 6) Export to Excel
out_file <- "4.4.Appendix p9.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Adjusted OR")
writeData(wb, "Adjusted OR", tab)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

left_style <- createStyle(
  halign = "left",
  valign = "center",
  border = "TopBottomLeftRight"
)

center_style <- createStyle(
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

addStyle(wb, "Adjusted OR", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
addStyle(wb, "Adjusted OR", left_style, rows = 2:(nrow(tab) + 1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Adjusted OR", center_style, rows = 2:(nrow(tab) + 1), cols = 2:3, gridExpand = TRUE)

setColWidths(wb, "Adjusted OR", cols = 1, widths = 55)
setColWidths(wb, "Adjusted OR", cols = 2:3, widths = 20)
freezePane(wb, "Adjusted OR", firstActiveRow = 2)

saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Excel file saved:", out_file, "\n")