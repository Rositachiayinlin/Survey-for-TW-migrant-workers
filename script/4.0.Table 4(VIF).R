library(readr)
library(dplyr)
library(broom)
library(stringr)
library(writexl)
library(DescTools)
library(car)

# Read data
df <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# Prepare data
df2 <- df %>%
  mutate(
    Mentalrisk = as.integer(Mentalrisk),
    Totalhour = as.numeric(Totalhour),
    Totalhour10 = Totalhour / 10,
    Age = as.numeric(Age),
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("No", "Yes")),
    Transfamily = factor(Transfamily, levels = c(0, 1), labels = c("No", "Yes")),
    Emphousing = factor(Emphousing, levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")),
    Industry = factor(
      Industry,
      levels = c(1, 2, 3),
      labels = c("Manufacturing", "Service sector", "Construction")
    ),
    Married = factor(Married, levels = c(0, 1), labels = c("Single", "Married")),
    Edu2 = factor(
      ifelse(Edu == 3, "Postgraduate", "Non-postgraduate"),
      levels = c("Non-postgraduate", "Postgraduate")
    )
  ) %>%
  mutate(
    Psysocabuse = relevel(Psysocabuse, ref = "No"),
    Transfamily = relevel(Transfamily, ref = "No"),
    Emphousing  = relevel(Emphousing, ref = "No"),
    Sex         = relevel(Sex, ref = "Male"),
    Industry    = relevel(Industry, ref = "Manufacturing"),
    Married     = relevel(Married, ref = "Single"),
    Edu2        = relevel(Edu2, ref = "Non-postgraduate")
  )

# Helpers
format_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

run_model_table <- function(formula, data, or_col, p_col) {
  glm(formula, data = data, family = binomial()) %>%
    tidy(conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      term,
      !!or_col := sprintf("%.2f (%.2f–%.2f)", exp(estimate), exp(conf.low), exp(conf.high)),
      !!p_col := format_p(p.value)
    )
}

pretty_term <- function(term) {
  term %>%
    str_replace("^Totalhour10$", "Weekly working hours (per 10 hours)") %>%
    str_replace("^PsysocabuseYes$", "Verbal or psychological abuse (Yes vs No)") %>%
    str_replace("^TransfamilyYes$", "Transnational family (Yes vs No)") %>%
    str_replace("^EmphousingYes$", "Employer-controlled housing (Yes vs No)") %>%
    str_replace("^SexFemale$", "Sex (Female vs Male)") %>%
    str_replace("^Age$", "Age (years)") %>%
    str_replace("^IndustryService sector$", "Industry (Service vs Manufacturing)") %>%
    str_replace("^IndustryConstruction$", "Industry (Construction vs Manufacturing)") %>%
    str_replace("^Edu2Postgraduate$", "Education (Postgraduate vs Non-postgraduate)") %>%
    str_replace("^MarriedMarried$", "Marital status (Married vs Single)")
}

# Predictors
predictors <- c(
  "Totalhour10", "Psysocabuse", "Transfamily", "Emphousing",
  "Sex", "Age", "Industry", "Edu2", "Married"
)

# Crude models
crude <- bind_rows(lapply(predictors, function(x) {
  run_model_table(
    as.formula(paste("Mentalrisk ~", x)),
    df2,
    "Crude OR (95% CI)",
    "Crude p"
  )
}))

# Adjusted model
adj_formula <- as.formula(
  paste("Mentalrisk ~", paste(predictors, collapse = " + "))
)

adj_model <- glm(adj_formula, data = df2, family = binomial())

adjusted <- run_model_table(
  adj_formula,
  df2,
  "Adjusted OR (95% CI)",
  "Adjusted p"
)

# Pseudo R-squared
r2_table <- data.frame(
  Measure = c("McFadden", "Nagelkerke"),
  Value = c(
    round(as.numeric(PseudoR2(adj_model, which = "McFadden")), 3),
    round(as.numeric(PseudoR2(adj_model, which = "Nagelkerke")), 3)
  )
)

# VIF / GVIF
vif_raw <- car::vif(adj_model)

if (is.matrix(vif_raw)) {
  vif_table <- as.data.frame(vif_raw) %>%
    tibble::rownames_to_column("Variable") %>%
    mutate(
      GVIF_1_over_2Df = round(GVIF^(1 / (2 * Df)), 3)
    ) %>%
    select(Variable, Df, GVIF, GVIF_1_over_2Df)
} else {
  vif_table <- data.frame(
    Variable = names(vif_raw),
    VIF = round(as.numeric(vif_raw), 3)
  )
}

# Final table
final_table <- full_join(crude, adjusted, by = "term") %>%
  mutate(Predictor = pretty_term(term)) %>%
  select(
    Predictor,
    `Crude OR (95% CI)`, `Crude p`,
    `Adjusted OR (95% CI)`, `Adjusted p`
  )

# Export Excel
write_xlsx(
  list(
    "Logistic regression table" = final_table,
    "Pseudo R2" = r2_table,
    "VIF" = vif_table
  ),
  "4.0.Table 4(VIF).xlsx"
)

# Print
print(final_table)
print(r2_table)
print(vif_table)