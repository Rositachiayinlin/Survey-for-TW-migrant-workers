library(readr)
library(dplyr)
library(broom)
library(flextable)
library(officer)

# Read data
dat <- read_csv("data/matchedsample(1202).csv", show_col_types = FALSE)

# Prepare data
dat <- dat %>%
  transmute(
    Tiredness  = as.numeric(Tiredness),
    Mentalrisk = as.numeric(Mentalrisk),
    Poorhealth = as.numeric(Poorhealth),
    Group = factor(as.numeric(Group), c(0, 1),
                   c("Non-migrant workers in Taiwan",
                     "Taiwanese migrant workers in Vietnam")),
    Sex = factor(Sex, c(1, 2), c("Male", "Female")),
    Age = as.numeric(Age),
    Industry = factor(Industry, c(1, 2, 3),
                      c("Manufacturing", "Service sector", "Construction")),
    Married = factor(Married, c(0, 1), c("Single", "Married")),
    Edu = factor(Edu, c(1, 2, 3),
                 c("High school and below",
                   "University or college",
                   "Postgraduate"))
  ) %>%
  na.omit()

# Helpers
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
fmt_ci <- function(or, lo, hi) sprintf("%.2f (%.2f to %.2f)", or, lo, hi)

# Regression function
run_model <- function(outcome, outcome_label) {
  glm(
    as.formula(paste(outcome, "~ Group + Sex + Age + Industry + Married + Edu")),
    data = dat, family = binomial()
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
        term == "MarriedMarried" ~ "Marital status",
        term %in% c("EduUniversity or college", "EduPostgraduate") ~ "Educational attainment"
      ),
      Comparison = case_when(
        term == "GroupTaiwanese migrant workers in Vietnam" ~ "Taiwanese migrant workers in Vietnam (ref: non-migrant workers in Taiwan)",
        term == "SexFemale" ~ "Female (ref: Male)",
        term == "Age" ~ "Per 1-year increase",
        term == "IndustryService sector" ~ "Service sector (ref: Manufacturing)",
        term == "IndustryConstruction" ~ "Construction (ref: Manufacturing)",
        term == "MarriedMarried" ~ "Married (ref: Single)",
        term == "EduUniversity or college" ~ "University or college (ref: High school and below)",
        term == "EduPostgraduate" ~ "Postgraduate (ref: High school and below)"
      ),
      `Adjusted OR` = sprintf("%.2f", estimate),
      `95% CI` = fmt_ci(estimate, conf.low, conf.high),
      `p value` = fmt_p(p.value)
    ) %>%
    select(Outcome, Variable, Comparison, `Adjusted OR`, `95% CI`, `p value`)
}

# Run all models
table_full <- bind_rows(
  run_model("Tiredness", "Severe fatigue"),
  run_model("Mentalrisk", "Psychological distress"),
  run_model("Poorhealth", "Poor self-rated health")
)

# Main results: Group effect only
table_main <- table_full %>%
  filter(Variable == "Group") %>%
  mutate(`Adjusted covariates` = "Sex, age, industry, marital status, educational attainment") %>%
  select(
    Outcome,
    Exposure = Comparison,
    `Adjusted OR`,
    `95% CI`,
    `p value`,
    `Adjusted covariates`
  )

# Save CSV
write_csv(table_full, "matched_sample_regression_full_table.csv")
write_csv(table_main, "matched_sample_regression_main_results.csv")

# Sample size
n_total <- nrow(dat)
n_migrant <- sum(dat$Group == "Taiwanese migrant workers in Vietnam")
n_nonmigrant <- sum(dat$Group == "Non-migrant workers in Taiwan")

# Word tables
ft_main <- flextable(table_main) %>%
  autofit() %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  set_caption(
    paste0(
      "Table X. Adjusted odds ratios for health outcomes among Taiwanese migrant workers in Vietnam ",
      "compared with non-migrant workers in Taiwan after propensity score matching ",
      "(n = ", n_total, "; migrant workers = ", n_migrant,
      ", non-migrant workers = ", n_nonmigrant, ")."
    )
  )

ft_full <- flextable(table_full) %>%
  autofit() %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  set_caption(
    paste0(
      "Appendix Table S1. Adjusted logistic regression models for matched sample ",
      "(n = ", n_total, "; migrant workers = ", n_migrant,
      ", non-migrant workers = ", n_nonmigrant, ")."
    )
  )

doc <- read_docx() %>%
  body_add_par("Regression tables for matched sample", style = "heading 1") %>%
  body_add_par(
    "Models were adjusted for sex, age, industry, marital status, and educational attainment. Reference group for the main exposure was non-migrant workers in Taiwan.",
    style = "Normal"
  ) %>%
  body_add_par("", style = "Normal") %>%
  flextable::body_add_flextable(ft_main) %>%
  body_add_par("", style = "Normal") %>%
  flextable::body_add_flextable(ft_full)

print(doc, target = "matched_sample_regression_tables.docx")

# Print results
table_main
table_full