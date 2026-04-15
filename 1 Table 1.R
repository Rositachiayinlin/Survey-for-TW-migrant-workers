############################################
## Table 1 (Full) - RStudio choose file -> Table
## - Labels show NO parentheses like (Telemed=1)
## - Health outcomes order adjusted:
##   Sleep problems -> Severe fatigue -> Psychological distress -> Diagnosed mental disorders -> Poor health
############################################

## ---- 0) Packages ----
## 若沒裝套件先跑：
## install.packages(c("readr","readxl","haven"))
library(readr)
library(readxl)
library(haven)

## ---- 1) Read data (choose file in RStudio) ----
file <- "4.3 Data of migrant workers (N=434).csv"
ext  <- tolower(tools::file_ext(file))

if (ext == "csv") {
  df <- read_csv(file, show_col_types = FALSE)
} else if (ext %in% c("xlsx", "xls")) {
  df <- read_excel(file)
} else if (ext == "sav") {
  df <- read_sav(file)
} else if (ext == "dta") {
  df <- read_dta(file)
} else {
  stop("不支援的檔案格式：", ext, "（支援 csv/xlsx/xls/sav/dta）")
}

## ---- 2) Helpers ----
pct <- function(x, d) sprintf("%d (%.1f%%)", x, 100 * x / d)

ms <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  sprintf("%.0f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

## ---- 3) Check required variables ----
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
  stop("❌ 你的資料缺少以下變項：\n - ", paste(missing_vars, collapse = "\n - "))
} else {
  message("✅ 變項檢查通過：Table 1 需要的變項都存在。")
}

## ---- 4) Fix numeric type for hour variables ----
df$Totalhour    <- suppressWarnings(as.numeric(as.character(df$Totalhour)))
df$Weekhour     <- suppressWarnings(as.numeric(as.character(df$Weekhour)))
df$Weekovertime <- suppressWarnings(as.numeric(as.character(df$Weekovertime)))

## ---- 5) Compute statistics ----
N <- nrow(df)

# Sociodemographic
n_male   <- sum(df$Sex == 1, na.rm = TRUE)
n_female <- sum(df$Sex == 2, na.rm = TRUE)
n_highedu <- sum(df$Highedu == 1, na.rm = TRUE)
age_ms <- ms(df$Age)
mig_ms <- ms(df$Migrationyear)

n_single  <- sum(df$Married == 0, na.rm = TRUE)
n_married <- sum(df$Married == 1, na.rm = TRUE)

married_df  <- df[df$Married == 1, ]
den_married <- nrow(married_df)
n_vn_yes <- sum(married_df$Vnfamily == 1, na.rm = TRUE)
n_vn_no  <- sum(married_df$Vnfamily == 0, na.rm = TRUE)

n_motive_income <- sum(df$Motive_income == 1, na.rm = TRUE)

# Work-related
n_industry   <- sum(df$Industry == 1, na.rm = TRUE)
n_management <- sum(df$Management == 1, na.rm = TRUE)
n_highincome <- sum(df$Highincome == 1, na.rm = TRUE)
n_emphousing <- sum(df$Emphousing == 1, na.rm = TRUE)

totalhour_ms <- ms(df$Totalhour)
weekhour_ms  <- ms(df$Weekhour)
overtime_ms  <- ms(df$Weekovertime)

n_abuse <- sum(df$Psysocabuse == 1, na.rm = TRUE)

# Health outcomes (order adjusted)
n_sicksleep  <- sum(df$Sicksleep == 1, na.rm = TRUE)
n_tiredness  <- sum(df$Tiredness == 1, na.rm = TRUE)   # fatigue first
n_mentalrisk <- sum(df$Mentalrisk == 1, na.rm = TRUE)  # then distress
n_diamental  <- sum(df$Diamental == 1, na.rm = TRUE)
n_poorhlth   <- sum(df$Poorhealth == 1, na.rm = TRUE)

# Healthcare utilization
n_curetwmed <- sum(df$Curetwmed == 1, na.rm = TRUE)
n_twnhi     <- sum(df$Twnhi == 1, na.rm = TRUE)
n_vnmedlan  <- sum(df$Vnmedlan == 1, na.rm = TRUE)
n_telemed   <- sum(df$Telemed == 1, na.rm = TRUE)

## ---- 6) Build Table 1 (NO parentheses in labels) ----
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
    "Taiwan’s National Health Insurance coverage",
    "Language barriers to healthcare in Vietnam",
    "Use of Taiwan-based telemedicine"
  ),
  `n (%)` = c(
    "",
    "",
    pct(n_male, N),
    pct(n_female, N),
    pct(n_highedu, N),
    age_ms,
    mig_ms,
    "",
    pct(n_single, N),
    pct(n_married, N),
    pct(n_vn_yes, den_married),
    pct(n_vn_no, den_married),
    "",
    pct(n_motive_income, N),

    "",
    pct(n_industry, N),
    pct(n_management, N),
    pct(n_highincome, N),
    pct(n_emphousing, N),
    totalhour_ms,
    weekhour_ms,
    overtime_ms,
    pct(n_abuse, N),

    "",
    pct(n_sicksleep, N),
    pct(n_tiredness, N),
    pct(n_mentalrisk, N),
    pct(n_diamental, N),
    pct(n_poorhlth, N),

    "",
    pct(n_curetwmed, N),
    pct(n_twnhi, N),
    pct(n_vnmedlan, N),
    pct(n_telemed, N)
  ),
  stringsAsFactors = FALSE
)

## ---- 7) Show / Export ----
print(Table1)     # Console 顯示
View(Table1)      # RStudio 表格視窗

write.csv(Table1, "Table1_full_no_parentheses.csv", row.names = FALSE)
message("✅ 已輸出：Table1_full_no_parentheses.csv；位置：", getwd() 

