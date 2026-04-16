library(readr)
library(dplyr)
library(broom)

# Read data
df <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# Prepare data
df2 <- df %>%
  mutate(
    Mentalrisk = as.integer(Mentalrisk),
    Totalhour  = as.numeric(Totalhour),
    Age        = as.numeric(Age),
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("No", "Yes")),
    Transfamily = factor(Transfamily, levels = c(0, 1), labels = c("No", "Yes")),
    Emphousing  = factor(Emphousing,  levels = c(0, 1), labels = c("No", "Yes")),
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
    Emphousing  = relevel(Emphousing,  ref = "No"),
    Sex         = relevel(Sex,         ref = "Male"),
    Industry    = relevel(Industry,    ref = "Manufacturing"),
    Married     = relevel(Married,     ref = "Single"),
    Edu2        = relevel(Edu2,        ref = "Non-postgraduate")
  )

# Fit models
m_main <- glm(
  Mentalrisk ~ Totalhour + Psysocabuse + Transfamily + Emphousing +
    Sex + Age + Industry + Edu2 + Married,
  data = df2,
  family = binomial()
)

m_age2 <- glm(
  Mentalrisk ~ Totalhour + Psysocabuse + Transfamily + Emphousing +
    Sex + Age + I(Age^2) + Industry + Edu2 + Married,
  data = df2,
  family = binomial()
)

# Extract ORs
get_or <- function(model, terms, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(term %in% terms) %>%
    mutate(
      Model = model_name,
      term = case_when(
        term == "Totalhour" ~ "Weekly working hours (per hour)",
        term == "PsysocabuseYes" ~ "Verbal/psychological abuse (Yes vs No)",
        TRUE ~ term
      ),
      OR_CI = sprintf("%.2f (%.2f–%.2f)", exp(estimate), exp(conf.low), exp(conf.high)),
      p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Model, term, OR_CI, p)
}

# Compare key adjusted ORs
compare <- bind_rows(
  get_or(m_main, c("Totalhour", "PsysocabuseYes"), "Main (Age linear)"),
  get_or(m_age2, c("Totalhour", "PsysocabuseYes"), "Sensitivity (+Age^2)")
)

print(compare)