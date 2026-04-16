library(readr)
library(dplyr)

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

# Linear model
model_linear <- glm(
  Mentalrisk ~ Totalhour + Age +
    Psysocabuse + Transfamily + Emphousing +
    Sex + Industry + Edu2 + Married,
  data = df2,
  family = binomial()
)

# Quadratic model
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

# Quadratic terms
quad_terms <- summary(model_quadratic)$coefficients[
  c("I(Totalhour^2)", "I(Age^2)"), ,
  drop = FALSE
]

# Print
print(lrt_result)
print(quad_terms)
