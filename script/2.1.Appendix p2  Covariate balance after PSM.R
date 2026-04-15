library(readr)
library(dplyr)
library(tableone)
library(writexl)
library(tibble)

# Read data + recode
dat <- read_csv("data/matchedsample(1202).csv", show_col_types = FALSE) %>%
  mutate(
    Group = factor(Group, c(0, 1),
                   c("Non-migrant workers in Taiwan",
                     "Taiwanese migrant workers in Vietnam")),
    Sex = factor(Sex, c(1, 2), c("Male", "Female"))
  )

# Variables for balance check
vars <- c("Sex", "Age")

# Create balance table
tab_df <- CreateTableOne(
  vars = vars,
  strata = "Group",
  data = dat,
  factorVars = "Sex",
  test = FALSE,
  smd = TRUE
) %>%
  print(
    smd = TRUE,
    showAllLevels = TRUE,
    noSpaces = TRUE,
    quote = FALSE,
    printToggle = FALSE
  ) %>%
  as.data.frame() %>%
  rownames_to_column("Variable")

# Export
write.csv(tab_df, "PSM_balance_table.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(tab_df, "PSM_balance_table.xlsx")