library(readr)
library(dplyr)
library(quantreg)
library(writexl)
library(tibble)

# Read data
dat <- read_csv("data/migrantworker(434).csv", show_col_types = FALSE)

# Prepare data
dat <- dat %>%
  transmute(
    Mentalsum   = as.numeric(Mentalsum),
    Totalhour10 = as.numeric(Totalhour) / 10,
    Psysocabuse = factor(Psysocabuse, levels = c(0, 1), labels = c("No", "Yes")),
    Transfamily = factor(Transfamily, levels = c(0, 1), labels = c("No", "Yes")),
    Emphousing  = factor(Emphousing,  levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")),
    Age = as.numeric(Age),
    Industry = factor(
      Industry,
      levels = c(1, 2, 3),
      labels = c("Manufacturing", "Service sector", "Construction")
    ),
    Edu2 = factor(
      ifelse(Edu == 3, "Postgraduate", "Non-postgraduate"),
      levels = c("Non-postgraduate", "Postgraduate")
    ),
    Married = factor(Married, levels = c(0, 1), labels = c("Single", "Married"))
  ) %>%
  na.omit()

# Quantile regression (median regression)
fit <- rq(
  Mentalsum ~ Totalhour10 + Psysocabuse + Transfamily + Emphousing +
    Sex + Age + Industry + Edu2 + Married,
  tau = 0.5,
  data = dat
)

# Extract coefficients
coef_mat <- summary(fit, se = "nid")$coefficients %>%
  as.data.frame()

colnames(coef_mat) <- c("Estimate", "Std_Error", "t_value", "p_value")

coef_mat <- coef_mat %>%
  rownames_to_column("term") %>%
  mutate(
    CI_low  = Estimate - 1.96 * Std_Error,
    CI_high = Estimate + 1.96 * Std_Error,
    Predictor = case_when(
      term == "Totalhour10" ~ "Weekly working hours (per 10-hour increase)",
      term == "PsysocabuseYes" ~ "Verbal or psychological abuse (yes vs no)",
      term == "TransfamilyYes" ~ "Transnational family (yes vs no)",
      term == "EmphousingYes" ~ "Employer-controlled housing (yes vs no)",
      term == "SexFemale" ~ "Sex (female vs male)",
      term == "Age" ~ "Age (years)",
      term == "IndustryService sector" ~ "Industry: service sector vs manufacturing",
      term == "IndustryConstruction" ~ "Industry: construction vs manufacturing",
      term == "Edu2Postgraduate" ~ "Education: postgraduate vs non-postgraduate",
      term == "MarriedMarried" ~ "Marital status: married vs single",
      TRUE ~ term
    )
  )

# Predictor order
predictor_order <- c(
  "Weekly working hours (per 10-hour increase)",
  "Verbal or psychological abuse (yes vs no)",
  "Transnational family (yes vs no)",
  "Employer-controlled housing (yes vs no)",
  "Sex (female vs male)",
  "Age (years)",
  "Industry: service sector vs manufacturing",
  "Industry: construction vs manufacturing",
  "Education: postgraduate vs non-postgraduate",
  "Marital status: married vs single"
)

# Format functions
fmt_num <- function(x) formatC(x, format = "f", digits = 2)
fmt_p <- function(p) ifelse(
  is.na(p), "",
  ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
)

# Final table
final_table <- coef_mat %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Predictor,
    `B coefficient (95% CI)` = paste0(
      fmt_num(Estimate), " (",
      fmt_num(CI_low), " to ",
      fmt_num(CI_high), ")"
    ),
    `p value` = fmt_p(p_value)
  ) %>%
  mutate(Predictor = factor(Predictor, levels = predictor_order)) %>%
  arrange(Predictor) %>%
  mutate(Predictor = as.character(Predictor))

# Save ONLY ONE Excel file (final output)
write_xlsx(final_table, "4.2.Appendix p7.xlsx")

# Print to console
print(final_table)