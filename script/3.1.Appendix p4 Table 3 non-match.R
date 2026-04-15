library(readr)
library(dplyr)
library(broom)
library(writexl)
library(flextable)
library(officer)

# Read data
dat <- read_csv("data/non-matched(3671).csv", show_col_types = FALSE) %>%
  transmute(
    Tiredness  = as.numeric(Tiredness),
    Mentalrisk = as.numeric(Mentalrisk),
    Poorhealth = as.numeric(Poorhealth),
    Group = factor(Group, c(0,1),
                   c("Non-migrant workers in Taiwan",
                     "Taiwanese migrant workers in Vietnam")),
    Sex = factor(Sex, c(1,2), c("Male","Female")),
    Age = as.numeric(Age),
    Industry = factor(Industry, c(1,2,3),
                      c("Manufacturing","Service sector","Construction")),
    Married = factor(Married, c(0,1), c("Single","Married")),
    Edu = factor(Edu, c(1,2,3),
                 c("High school and below",
                   "University or college",
                   "Postgraduate"))
  ) %>%
  na.omit()

# Helpers
fmt_p  <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
fmt_ci <- function(or, lo, hi) sprintf("%.2f (%.2f–%.2f)", or, lo, hi)

# Model function
run_model <- function(outcome, label){
  glm(
    as.formula(paste(outcome, "~ Group + Sex + Age + Industry + Married + Edu")),
    data = dat, family = binomial()
  ) %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Outcome = label,
      Variable = case_when(
        term == "GroupTaiwanese migrant workers in Vietnam" ~ "Group",
        term == "SexFemale" ~ "Sex",
        term == "Age" ~ "Age",
        term %in% c("IndustryService sector","IndustryConstruction") ~ "Industry",
        term == "MarriedMarried" ~ "Marital status",
        term %in% c("EduUniversity or college","EduPostgraduate") ~ "Educational attainment"
      ),
      Comparison = case_when(
        term == "GroupTaiwanese migrant workers in Vietnam" ~ "Migrant vs non-migrant",
        term == "SexFemale" ~ "Female vs Male",
        term == "Age" ~ "Per 1-year increase",
        term == "IndustryService sector" ~ "Service vs Manufacturing",
        term == "IndustryConstruction" ~ "Construction vs Manufacturing",
        term == "MarriedMarried" ~ "Married vs Single",
        term == "EduUniversity or college" ~ "University vs High school",
        term == "EduPostgraduate" ~ "Postgraduate vs High school"
      ),
      aOR = estimate,
      lower = conf.low,
      upper = conf.high,
      `aOR (95% CI)` = fmt_ci(estimate, conf.low, conf.high),
      `p value` = fmt_p(p.value)
    )
}

# Run models
res <- bind_rows(
  run_model("Tiredness", "Severe fatigue"),
  run_model("Mentalrisk", "Psychological distress"),
  run_model("Poorhealth", "Poor self-rated health")
)

# Tables
table_full <- res %>%
  select(Outcome, Variable, Comparison, `aOR (95% CI)`, `p value`)

table_main <- res %>%
  filter(Variable == "Group") %>%
  select(Outcome, `aOR (95% CI)`, `p value`)

table_numeric <- res %>%
  select(Outcome, Variable, Comparison, aOR, lower, upper, p.value)

# Save files
write_csv(table_main, "table_main.csv")
write_csv(table_full, "table_full.csv")
write_csv(table_numeric, "table_numeric.csv")

write_xlsx(
  list(Main = table_main,
       Full = table_full,
       Numeric = table_numeric),
  "regression_tables.xlsx"
)

# Word output
doc <- read_docx() %>%
  body_add_par("Regression results", style = "heading 1") %>%
  body_add_flextable(flextable(table_main) %>% autofit()) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(flextable(table_full) %>% autofit())

print(doc, target = "regression_tables.docx")

# Print
table_main
table_full