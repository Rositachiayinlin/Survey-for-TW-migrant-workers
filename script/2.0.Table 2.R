library(readr)
library(dplyr)
library(writexl)

# Read data
dat <- read_csv("data/matchedsample(1302).csv", show_col_types = FALSE) %>%
  mutate(
    Group = factor(
      Group,
      levels = c(1, 0),
      labels = c("Taiwanese migrant workers in Vietnam",
                 "Non-migrant workers in Taiwan")
    )
  )

# Sample sizes
n_vn <- sum(dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE)
n_tw <- sum(dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE)

# Format n (%)
fmt <- function(n, d) {
  sprintf("%d (%.1f)", n, 100 * n / d)
}

# Format p value
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# Chi-square / Fisher test
get_p <- function(var) {
  x <- dat[[var]]
  g <- dat$Group
  keep <- !is.na(x) & !is.na(g)
  tab <- table(x[keep], g[keep])
  
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  
  if (any(chi$expected < 5)) {
    if (all(dim(tab) == c(2, 2))) {
      fisher.test(tab)$p.value
    } else {
      suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)
    }
  } else {
    chi$p.value
  }
}

# Build table
tab_df <- bind_rows(
  
  data.frame(
    Variable = "n",
    level = "",
    `Taiwanese migrant workers in Vietnam` = as.character(n_vn),
    `Non-migrant workers in Taiwan` = as.character(n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Industry",
    level = "Manufacturing",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Industry == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Industry == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Industry")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "Service sector",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Industry == 2 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Industry == 2 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "Construction",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Industry == 3 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Industry == 3 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "College or above",
    level = "Yes",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Highedu == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Highedu == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Highedu")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "No",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Highedu == 0 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Highedu == 0 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Married",
    level = "Yes",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Married == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Married == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Married")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "No",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Married == 0 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Married == 0 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Severe fatigue",
    level = "Yes",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Tiredness == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Tiredness == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Tiredness")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "No",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Tiredness == 0 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Tiredness == 0 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Psychological distress",
    level = "Yes",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Mentalrisk == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Mentalrisk == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Mentalrisk")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "No",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Mentalrisk == 0 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Mentalrisk == 0 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  ),
  
  data.frame(
    Variable = "Poor self-rated health",
    level = "Yes",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Poorhealth == 1 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Poorhealth == 1 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = fmt_p(get_p("Poorhealth")),
    check.names = FALSE
  ),
  data.frame(
    Variable = "",
    level = "No",
    `Taiwanese migrant workers in Vietnam` = fmt(sum(dat$Poorhealth == 0 & dat$Group == "Taiwanese migrant workers in Vietnam", na.rm = TRUE), n_vn),
    `Non-migrant workers in Taiwan` = fmt(sum(dat$Poorhealth == 0 & dat$Group == "Non-migrant workers in Taiwan", na.rm = TRUE), n_tw),
    p = "",
    check.names = FALSE
  )
)

# Preview
print(tab_df)

# Save files
write.csv(
  tab_df,
 
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write_xlsx(
  tab_df,
  "2.0.Table 2.xlsx"
)
