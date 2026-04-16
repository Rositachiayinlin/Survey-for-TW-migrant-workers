library(readr)
library(dplyr)
library(broom)
library(writexl)
library(tibble)

# Read data
df <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# Prepare data
df2 <- df %>%
  mutate(
    Mentalrisk  = as.integer(Mentalrisk),
    Totalhour   = as.numeric(Totalhour),
    Age         = as.numeric(Age),
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("No", "Yes")),
    Transfamily = factor(Transfamily, levels = c(0, 1), labels = c("No", "Yes")),
    Emphousing  = factor(Emphousing,  levels = c(0, 1), labels = c("No", "Yes")),
    Sex         = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")),
    Industry    = factor(
      Industry,
      levels = c(1, 2, 3),
      labels = c("Manufacturing", "Service sector", "Construction")
    ),
    Married     = factor(Married, levels = c(0, 1), labels = c("Single", "Married")),
    Edu2        = factor(
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

# =========================
# Part 1: Linear vs quadratic model comparison
# =========================

model_linear <- glm(
  Mentalrisk ~ Totalhour + Age +
    Psysocabuse + Transfamily + Emphousing +
    Sex + Industry + Edu2 + Married,
  data = df2,
  family = binomial()
)

model_quadratic <- glm(
  Mentalrisk ~ Totalhour + I(Totalhour^2) +
    Age + I(Age^2) +
    Psysocabuse + Transfamily + Emphousing +
    Sex + Industry + Edu2 + Married,
  data = df2,
  family = binomial()
)

# Likelihood ratio test
lrt_result <- anova(model_linear, model_quadratic, test = "LRT")
lrt_df <- as.data.frame(lrt_result) %>%
  rownames_to_column("Model")

# Quadratic terms
quad_terms <- tidy(model_quadratic, conf.int = TRUE) %>%
  filter(term %in% c("I(Totalhour^2)", "I(Age^2)")) %>%
  mutate(
    term = case_when(
      term == "I(Totalhour^2)" ~ "Totalhour^2",
      term == "I(Age^2)" ~ "Age^2",
      TRUE ~ term
    ),
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
    conf.low = round(conf.low, 4),
    conf.high = round(conf.high, 4)
  ) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

# Combine into one sheet-friendly table
lrt_section <- tibble(
  Section = "Likelihood ratio test",
  Item = colnames(lrt_df),
  Value = NA_character_
)

lrt_values <- lrt_df %>%
  mutate(across(everything(), as.character))

lrt_long <- bind_rows(lapply(seq_len(nrow(lrt_values)), function(i) {
  tibble(
    Section = paste0("Likelihood ratio test - Row ", i),
    Item = names(lrt_values),
    Value = unlist(lrt_values[i, ], use.names = FALSE)
  )
}))

quad_long <- quad_terms %>%
  mutate(across(everything(), as.character)) %>%
  bind_rows() %>%
  {
    bind_rows(lapply(seq_len(nrow(.)), function(i) {
      tibble(
        Section = paste0("Quadratic terms - ", .$term[i]),
        Item = names(.),
        Value = unlist(.[i, ], use.names = FALSE)
      )
    }))
  }

lrt_quad_output <- bind_rows(lrt_long, quad_long)

# =========================
# Part 2: OR comparison table
# =========================

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

get_or <- function(model, terms, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(term %in% terms) %>%
    mutate(
      Model = model_name,
      term = case_when(
        term == "Totalhour" ~ "Weekly working hours (per hour)",
        term == "PsysocabuseYes" ~ "Verbal/psychological abuse (Yes vs No)",
        term == "I(Age^2)" ~ "Age^2",
        TRUE ~ term
      ),
      OR_CI = sprintf("%.2f (%.2f–%.2f)", exp(estimate), exp(conf.low), exp(conf.high)),
      p = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Model, term, OR_CI, p)
}

compare <- bind_rows(
  get_or(m_main, c("Totalhour", "PsysocabuseYes"), "Main (Age linear)"),
  get_or(m_age2, c("Totalhour", "PsysocabuseYes"), "Sensitivity (+Age^2)")
)

# =========================
# Export to one Excel file
# =========================

write_xlsx(
  list(
    LRT_and_Quadratic_Terms = lrt_quad_output,
    OR_Comparison = compare
  ),
  "4.1.Model comparison and test for non-linearity.xlsx"
)