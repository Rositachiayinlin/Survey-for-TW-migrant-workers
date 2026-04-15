library(readr)
library(dplyr)
library(tableone)
library(writexl)

# Read data
file <- "data/matchedsample(1202).csv"
dat <- read_csv(file, show_col_types = FALSE)

# Recode variables
dat <- dat %>%
  mutate(
    Group = factor(Group, c(0, 1),
                   c("Non-migrant workers in Taiwan",
                     "Taiwanese migrant workers in Vietnam")),
    Sex = factor(Sex, c(1, 2), c("Male", "Female")),
    Industry = factor(Industry, c(1, 2, 3),
                      c("Manufacturing", "Service sector", "Construction")),
    Highedu = factor(Highedu, c(0, 1), c("No", "Yes")),
    Married = factor(Married, c(0, 1), c("Single", "Married")),
    Tiredness = factor(Tiredness, c(0, 1), c("No", "Yes")),
    Mentalrisk = factor(Mentalrisk, c(0, 1), c("No", "Yes")),
    Poorhealth = factor(Poorhealth, c(0, 1), c("No", "Yes"))
  )

# Variables for comparison
vars <- c("Industry", "Highedu", "Married",
          "Tiredness", "Mentalrisk", "Poorhealth")

# Create Table 1
tab <- CreateTableOne(
  vars = vars,
  strata = "Group",
  data = dat,
  factorVars = vars,
  test = TRUE,
  smd = FALSE
)

# Export table
tab_df <- print(
  tab,
  showAllLevels = TRUE,
  noSpaces = TRUE,
  quote = FALSE,
  printToggle = FALSE
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")

write.csv(tab_df, "Matched_group_comparison.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(tab_df, "Matched_group_comparison.xlsx")