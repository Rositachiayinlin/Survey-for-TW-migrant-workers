############################################
## Internal consistency
## Data file: data/migrantworker(434).csv
## Output file: 1.1 Internal consistency (fatigue, psychological distress).xlsx
############################################

## ---- 0) Packages ----
## If needed, install packages first:
## install.packages(c("readr", "dplyr", "psych", "openxlsx"))

library(readr)
library(dplyr)
library(psych)
library(openxlsx)

## ---- 1) Read data ----
file <- "data/migrantworker(434).csv"

if (!file.exists(file)) {
  stop("Data file not found: ", file)
}

dat <- read_csv(file, show_col_types = FALSE)

## ---- 2) Helper function ----
get_alpha <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
    na.omit() %>%
    psych::alpha() %>%
    `[[`("total") %>%
    `[[`("raw_alpha")
}

## ---- 3) Check required variables ----
required_vars <- c(paste0("Tired", 1:5), paste0("Mental", 1:5))
missing_vars <- setdiff(required_vars, names(dat))

if (length(missing_vars) > 0) {
  stop(
    "Missing required variables:\n - ",
    paste(missing_vars, collapse = "\n - ")
  )
} else {
  message("Variable check passed.")
}

## ---- 4) Compute Cronbach's alpha ----
alpha_tired  <- get_alpha(dat, paste0("Tired", 1:5))
alpha_mental <- get_alpha(dat, paste0("Mental", 1:5))

## ---- 5) Create output table ----
Table_internal_consistency <- data.frame(
  Scale = c("Fatigue", "Psychological distress"),
  `Cronbach's alpha` = c(
    sprintf("%.3f", alpha_tired),
    sprintf("%.3f", alpha_mental)
  ),
  stringsAsFactors = FALSE
)

## ---- 6) Show result ----
print(Table_internal_consistency)
View(Table_internal_consistency)

## ---- 7) Export to formatted Excel ----
output_file <- "1.1 Internal consistency (fatigue, psychological distress).xlsx"

if (file.exists(output_file)) {
  file.remove(output_file)
}

wb <- createWorkbook()
addWorksheet(wb, "Internal consistency")

writeData(wb, "Internal consistency", Table_internal_consistency, startRow = 1, colNames = TRUE)

header_style <- createStyle(textDecoration = "bold", halign = "left", valign = "center")
body_style   <- createStyle(halign = "left", valign = "center")

addStyle(wb, "Internal consistency", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
addStyle(wb, "Internal consistency", body_style, rows = 2:(nrow(Table_internal_consistency) + 1), cols = 1:2, gridExpand = TRUE)

setColWidths(wb, "Internal consistency", cols = 1:2, widths = "auto")
freezePane(wb, "Internal consistency", firstRow = TRUE)

saveWorkbook(wb, output_file, overwrite = TRUE)

message("Internal consistency table exported successfully: ", file.path(getwd(), output_file))