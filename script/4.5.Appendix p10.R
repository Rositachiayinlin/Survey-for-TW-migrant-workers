# ============================================================
# Sex-stratified + interaction logistic regression
# Input:  data/migrantworker(434).csv
# Output: sex_stratified_with_interaction.xlsx
# ============================================================

# 0) Packages ------------------------------------------------
pkg <- c("readr", "dplyr", "broom", "openxlsx")
new_pkg <- pkg[!sapply(pkg, requireNamespace, quietly = TRUE)]
if (length(new_pkg) > 0) install.packages(new_pkg)

library(readr)
library(dplyr)
library(broom)
library(openxlsx)

# 1) Load data ----------------------------------------------
df <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# 2) Recode --------------------------------------------------
df2 <- df %>%
  mutate(
    Mentalrisk = as.numeric(Mentalrisk),
    Totalhour10 = as.numeric(Totalhour) / 10,
    Age = as.numeric(Age),
    
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("no", "yes")),
    Transfamily = factor(Transfamily, levels = c(0, 1), labels = c("no", "yes")),
    Emphousing  = factor(Emphousing,  levels = c(0, 1), labels = c("no", "yes")),
    Married     = factor(Married,     levels = c(0, 1), labels = c("single", "married")),
    Sex         = factor(Sex,         levels = c(1, 2), labels = c("male", "female")),
    Industry    = factor(Industry,    levels = c(1, 2, 3),
                         labels = c("Manufacturing", "Service sector", "Construction")),
    Edu_bin     = factor(ifelse(Edu == 3, "postgraduate", "non-postgraduate"),
                         levels = c("non-postgraduate", "postgraduate"))
  ) %>%
  filter(complete.cases(
    Mentalrisk, Totalhour10, Psysocabuse, Transfamily,
    Emphousing, Age, Married, Sex, Industry, Edu_bin
  ))

# 3) Candidate predictors -----------------------------------
base_vars <- c(
  "Totalhour10",
  "Psysocabuse",
  "Transfamily",
  "Emphousing",
  "Age",
  "Industry",
  "Edu_bin",
  "Married"
)

# 4) Helper: keep variables with variation ------------------
has_variation <- function(x) {
  if (is.factor(x)) nlevels(droplevels(x)) >= 2 else length(unique(stats::na.omit(x))) >= 2
}

available_vars <- function(data, vars) {
  vars[sapply(vars, function(v) has_variation(data[[v]]))]
}

make_formula <- function(outcome, vars) {
  as.formula(paste(outcome, "~", paste(vars, collapse = " + ")))
}

# 5) Stratified datasets ------------------------------------
df_m <- df2 %>% filter(Sex == "male")   %>% droplevels()
df_f <- df2 %>% filter(Sex == "female") %>% droplevels()

vars_m <- available_vars(df_m, base_vars)
vars_f <- available_vars(df_f, base_vars)

fm_m <- make_formula("Mentalrisk", vars_m)
fm_f <- make_formula("Mentalrisk", vars_f)

fit_m <- glm(fm_m, data = df_m, family = binomial)
fit_f <- glm(fm_f, data = df_f, family = binomial)

# 6) Interaction model --------------------------------------
# Use full sample; interaction terms can be tested here
fm_int <- Mentalrisk ~ Sex * (Totalhour10 + Psysocabuse + Transfamily +
                                Emphousing + Age + Industry + Edu_bin + Married)

fit_int <- glm(fm_int, data = df2, family = binomial)

# 7) Format helpers -----------------------------------------
fmt_or <- function(est, low, high) {
  paste0(sprintf("%.2f", est), " (",
         sprintf("%.2f", low), "–",
         sprintf("%.2f", high), ")")
}

fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

label_map <- c(
  "Totalhour10" = "Weekly working hours (per 10-hour increase)",
  "Psysocabuseyes" = "Verbal or psychological abuse (yes vs no)",
  "Transfamilyyes" = "Transnational family (yes vs no)",
  "Emphousingyes" = "Employer-controlled housing (yes vs no)",
  "Age" = "Age (years)",
  "IndustryService sector" = "Industry: service sector vs manufacturing",
  "IndustryConstruction" = "Industry: construction vs manufacturing",
  "Edu_binpostgraduate" = "Education: postgraduate vs non-postgraduate",
  "Marriedmarried" = "Marital status: married vs single"
)

order_vec <- c(
  "Weekly working hours (per 10-hour increase)",
  "Verbal or psychological abuse (yes vs no)",
  "Transnational family (yes vs no)",
  "Employer-controlled housing (yes vs no)",
  "Age (years)",
  "Industry: service sector vs manufacturing",
  "Industry: construction vs manufacturing",
  "Education: postgraduate vs non-postgraduate",
  "Marital status: married vs single"
)

# 8) Extract subgroup tables --------------------------------
extract_tab <- function(fit, prefix) {
  tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Variable = unname(label_map[term]),
      !!paste0(prefix, " OR (95% CI)") := fmt_or(estimate, conf.low, conf.high),
      !!paste0("p (", prefix, ")") := fmt_p(p.value)
    ) %>%
    filter(!is.na(Variable))
}

male_tab   <- extract_tab(fit_m, "Male")
female_tab <- extract_tab(fit_f, "Female")

# 9) Extract interaction p-values ----------------------------
int_terms <- tidy(fit_int) %>%
  filter(grepl("^Sexfemale:", term)) %>%
  mutate(
    raw_term = sub("^Sexfemale:", "", term),
    Variable = unname(label_map[raw_term]),
    `p for interaction` = fmt_p(p.value)
  ) %>%
  select(Variable, `p for interaction`) %>%
  filter(!is.na(Variable))

# 10) Merge final table -------------------------------------
final <- tibble(Variable = order_vec) %>%
  left_join(male_tab, by = "Variable") %>%
  left_join(female_tab, by = "Variable") %>%
  left_join(int_terms, by = "Variable")

# Mark subgroup-dropped variables as em dash
for (j in 2:ncol(final)) {
  final[[j]][is.na(final[[j]])] <- "—"
}

# 11) Export Excel ------------------------------------------
out_file <- "4.5.Appendix p10.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Results")
writeData(wb, "Results", final)

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

addStyle(wb, "Results", header_style, rows = 1, cols = 1:ncol(final), gridExpand = TRUE)
addStyle(wb, "Results", left_style, rows = 2:(nrow(final) + 1), cols = 1, gridExpand = TRUE)
addStyle(wb, "Results", center_style, rows = 2:(nrow(final) + 1), cols = 2:ncol(final), gridExpand = TRUE)

setColWidths(wb, "Results", cols = 1, widths = 45)
setColWidths(wb, "Results", cols = 2:ncol(final), widths = 20)
freezePane(wb, "Results", firstActiveRow = 2)

saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Excel saved:", out_file, "\n")
cat("Male model predictors used:   ", paste(vars_m, collapse = ", "), "\n")
cat("Female model predictors used: ", paste(vars_f, collapse = ", "), "\n")