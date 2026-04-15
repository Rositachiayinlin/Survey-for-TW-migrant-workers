# Packages
library(readr)
library(dplyr)
library(psych)

# Read data
file <- "data/migrantworker(434).csv"
dat <- read_csv(file, show_col_types = FALSE)

# Cronbach's alpha function
get_alpha <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), as.numeric)) %>%
    na.omit() %>%
    psych::alpha() %>%
    `[[`("total") %>%
    `[[`("raw_alpha")
}

# Psychological distress: Mental1–Mental5
alpha_mental <- get_alpha(dat, paste0("Mental", 1:5))

# Fatigue: Tired1–Tired5
alpha_tired <- get_alpha(dat, paste0("Tired", 1:5))

# Print results
cat("Psychological distress Cronbach's alpha:", alpha_mental, "\n")
cat("Fatigue Cronbach's alpha:", alpha_tired, "\n")
