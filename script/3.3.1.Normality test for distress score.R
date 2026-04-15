library(readr)
library(dplyr)
library(writexl)

# Read data
dat <- read_csv("data/matchedsample(1202).csv", show_col_types = FALSE) %>%
  transmute(
    Group = as.numeric(Group),
    Mentalsum = as.numeric(Mentalsum)
  ) %>%
  na.omit()

# Split groups
g1 <- dat %>% filter(Group == 1) %>% pull(Mentalsum)
g0 <- dat %>% filter(Group == 0) %>% pull(Mentalsum)

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
write_csv(res, "Mentalsum_normality.csv")
write_xlsx(res, "Mentalsum_normality.xlsx")

# Plots
png("Mentalsum_normality_plots.png", width = 1000, height = 800)
par(mfrow = c(2,2))

hist(g1, main = "Group 1", xlab = "Mentalsum")
qqnorm(g1); qqline(g1)

hist(g0, main = "Group 0", xlab = "Mentalsum")
qqnorm(g0); qqline(g0)

dev.off()

# Output
res