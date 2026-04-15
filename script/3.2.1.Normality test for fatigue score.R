library(readr)
library(dplyr)
library(writexl)

# Read data
dat <- read_csv("data/matchedsample(1202).csv", show_col_types = FALSE) %>%
  transmute(
    Group = as.numeric(Group),
    Tiredsum = as.numeric(Tiredsum)
  ) %>%
  na.omit()

# Split groups
g1 <- dat %>% filter(Group == 1) %>% pull(Tiredsum)
g0 <- dat %>% filter(Group == 0) %>% pull(Tiredsum)

# Descriptive stats
desc <- function(x) c(
  n = length(x),
  mean = mean(x),
  sd = sd(x),
  median = median(x)
)

desc_g1 <- desc(g1)
desc_g0 <- desc(g0)

# Shapiro test
shapiro_res <- function(x, label){
  if (length(x) < 3 | length(x) > 5000) {
    return(data.frame(Group = label, n = length(x), W = NA, p = NA))
  }
  s <- shapiro.test(x)
  data.frame(Group = label, n = length(x),
             W = unname(s$statistic),
             p = s$p.value)
}

res <- rbind(
  shapiro_res(g1, "Migrant workers (Vietnam)"),
  shapiro_res(g0, "Non-migrant workers (Taiwan)")
)

# Save results
write_csv(res, "Tiredsum_normality.csv")
write_xlsx(res, "Tiredsum_normality.xlsx")

# Plots
png("Tiredsum_normality_plots.png", width = 1000, height = 800)
par(mfrow = c(2,2))

hist(g1, main = "Group 1", xlab = "Tiredsum")
qqnorm(g1); qqline(g1)

hist(g0, main = "Group 0", xlab = "Tiredsum")
qqnorm(g0); qqline(g0)

dev.off()

# Output
res