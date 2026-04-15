library(readr)
library(dplyr)
library(quantreg)
library(writexl)
library(tibble)

# Read data
dat <- read_csv("data/matchedsample(1202).csv", show_col_types = FALSE) %>%
  transmute(
    Mentalsum = as.numeric(Mentalsum),
    Group = factor(Group, c(0,1),
                   c("Non-migrant workers in Taiwan",
                     "Taiwanese migrant workers in Vietnam")),
    Sex = factor(Sex, c(1,2), c("Male","Female")),
    Age = as.numeric(Age),
    Industry = factor(Industry, c(1,2,3),
                      c("Manufacturing","Service sector","Construction")),
    Edu = factor(Edu, c(1,2,3),
                 c("High school and below",
                   "University or college",
                   "Postgraduate")),
    Married = factor(Married, c(0,1), c("Single","Married"))
  ) %>%
  na.omit()

# Descriptive stats
desc <- dat %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    median = median(Mentalsum),
    Q1 = quantile(Mentalsum, 0.25),
    Q3 = quantile(Mentalsum, 0.75)
  )

# Quantile regression
fit <- rq(
  Mentalsum ~ Group + Sex + Age + Industry + Edu + Married,
  tau = 0.5,
  data = dat
)

# ⭐ 修正重點（不要用 rename V1）
res <- summary(fit, se = "nid")$coefficients %>%
  as.data.frame()

colnames(res) <- c("Estimate", "SE", "t", "p")

res <- res %>%
  rownames_to_column("term") %>%
  mutate(
    CI_low = Estimate - 1.96 * SE,
    CI_high = Estimate + 1.96 * SE
  )

# Format
fmt <- function(x) sprintf("%.2f", x)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Report table
table_report <- res %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = case_when(
      term == "GroupTaiwanese migrant workers in Vietnam" ~ "Migrant vs non-migrant",
      term == "SexFemale" ~ "Female vs Male",
      term == "Age" ~ "Age (years)",
      term == "IndustryService sector" ~ "Service vs Manufacturing",
      term == "IndustryConstruction" ~ "Construction vs Manufacturing",
      term == "EduUniversity or college" ~ "University vs High school",
      term == "EduPostgraduate" ~ "Postgraduate vs High school",
      term == "MarriedMarried" ~ "Married vs Single"
    ),
    `Adjusted median difference` = fmt(Estimate),
    `95% CI` = paste0(fmt(CI_low), " to ", fmt(CI_high)),
    `p value` = fmt_p(p)
  ) %>%
  select(Predictor, `Adjusted median difference`, `95% CI`, `p value`)

# Save outputs
write_csv(table_report, "QR_Mentalsum_table.csv")
write_xlsx(table_report, "QR_Mentalsum_table.xlsx")

write_csv(res, "QR_Mentalsum_full.csv")
write_xlsx(res, "QR_Mentalsum_full.xlsx")

write_csv(desc, "Mentalsum_desc.csv")
write_xlsx(desc, "Mentalsum_desc.xlsx")

# Print
table_report
