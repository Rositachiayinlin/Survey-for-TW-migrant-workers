# =========================================================
# Table 1 descriptive statistics
# Data file: data/migrantworker(434).csv
# Output: Table1_full.csv
# =========================================================

# 1. Packages
library(readr)
library(readxl)
library(haven)

# 2. Input file
file <- "data/migrantworker(434).csv"

# 3. Read data
read_data <- function(file) {
  ext <- tolower(tools::file_ext(file))
  
  if (!file.exists(file)) {
    stop("Data file not found: ", file)
  }
  
  if (ext == "csv") {
    read_csv(file, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    read_excel(file)
  } else if (ext == "sav") {
    read_sav(file)
  } else if (ext == "dta") {
    read_dta(file)
  } else {
    stop("Unsupported file format: ", ext)
  }
}

df <- read_data(file)

# 4. Helper functions
pct <- function(x, n) sprintf("%d (%.1f%%)", x, 100 * x / n)

mean_sd <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  sprintf("%.0f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

n_yes <- function(x, value = 1) sum(x == value, na.rm = TRUE)

# 5. Required variables
required_vars <- c(
  "Sex", "Highedu", "Age", "Migrationyear",
  "Married", "Vnfamily", "Motive_income",
  "Industry", "Management", "Highincome", "Emphousing",
  "Totalhour", "Weekhour", "Weekovertime", "Psysocabuse",
  "Sicksleep", "Tiredness", "Mentalrisk", "Diamental", "Poorhealth",
  "Curetwmed", "Twnhi", "Vnmedlan", "Telemed"
)

missing_vars <- setdiff(required_vars, names(df))
if (length(missing_vars) > 0) {
  stop("Missing required variables:\n - ", paste(missing_vars, collapse = "\n - "))
}

# 6. Convert numeric variables
numeric_vars <- c("Age", "Migrationyear", "Totalhour", "Weekhour", "Weekovertime")
for (v in numeric_vars) {
  df[[v]] <- suppressWarnings(as.numeric(as.character(df[[v]])))
}

# 7. Summary statistics
N <- nrow(df)

married_df  <- df[df$Married == 1, ]
den_married <- nrow(married_df)

Table1 <- data.frame(
  Characteristics = c(
    "Sociodemographic background",
    "Sex",
    "  Male",
    "  Female",
    "Education level, college or above",
    "Age, mean (SD)",
    "Duration of migration in Vietnam, years, mean (SD)",
    "Marital status",
    "  Single",
    "  Married",
    "    With transnational family a",
    "    Without transnational family a",
    "Reason for working in Vietnam",
    "  Higher income opportunities",
    
    "Work-related conditions and experiences",
    "Employed in the manufacturing sector",
    "Working as a supervisor",
    "High wage income b",
    "Living in employer-controlled residences",
    "Weekly working hours, mean (SD)",
    "  Regular working hours, mean (SD)",
    "  Overtime working hours, mean (SD)",
    "Exposed to verbal or psychological abuse in past year",
    
    "Health outcomes",
    "Self-reported sleep problems",
    "Severe fatigue",
    "Psychological distress, scale defined",
    "Self-reported diagnosed mental disorders (e.g. depression and anxiety)",
    "Self-rated poor health",
    
    "Healthcare utilization",
    "Delayed care until returning to Taiwan for treatment",
    "Taiwan's National Health Insurance coverage",
    "Language barriers to healthcare in Vietnam",
    "Use of Taiwan-based telemedicine"
  ),
  `n (%)` = c(
    "",
    "",
    pct(n_yes(df$Sex, 1), N),
    pct(n_yes(df$Sex, 2), N),
    pct(n_yes(df$Highedu, 1), N),
    mean_sd(df$Age),
    mean_sd(df$Migrationyear),
    "",
    pct(n_yes(df$Married, 0), N),
    pct(n_yes(df$Married, 1), N),
    pct(n_yes(married_df$Vnfamily, 1), den_married),
    pct(n_yes(married_df$Vnfamily, 0), den_married),
    "",
    pct(n_yes(df$Motive_income, 1), N),
    
    "",
    pct(n_yes(df$Industry, 1), N),
    pct(n_yes(df$Management, 1), N),
    pct(n_yes(df$Highincome, 1), N),
    pct(n_yes(df$Emphousing, 1), N),
    mean_sd(df$Totalhour),
    mean_sd(df$Weekhour),
    mean_sd(df$Weekovertime),
    pct(n_yes(df$Psysocabuse, 1), N),
    
    "",
    pct(n_yes(df$Sicksleep, 1), N),
    pct(n_yes(df$Tiredness, 1), N),
    pct(n_yes(df$Mentalrisk, 1), N),
    pct(n_yes(df$Diamental, 1), N),
    pct(n_yes(df$Poorhealth, 1), N),
    
    "",
    pct(n_yes(df$Curetwmed, 1), N),
    pct(n_yes(df$Twnhi, 1), N),
    pct(n_yes(df$Vnmedlan, 1), N),
    pct(n_yes(df$Telemed, 1), N)
  ),
  stringsAsFactors = FALSE
)

# 8. Show and export
print(Table1)
write.csv(Table1, "Table1_full.csv", row.names = FALSE)

message("Table 1 exported successfully: ", file.path(getwd(), "Table1_full.csv"))
