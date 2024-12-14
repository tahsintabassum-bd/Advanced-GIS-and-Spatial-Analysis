
age <- read.csv("AGE.csv")
install.packages("modelsummary")
library(modelsummary)

# Plot the scatterplot
plot(x = age$DBH, y = age$BHAGE,
main = "The scatterplot of Tree Diameter and Age ",
xlab = "DBH in cm",
ylab = "BHAGE in year")

# Create a new data frame with trees less than 200 years old
ageunder200 <- age[age$BHAGE <200, ]
ageunder200

# Count the number of records in the new data frame
num_age <- nrow(ageunder200)
print(num_age)

# Perform two-sample t-test
PSME_AGE <-ageunder200$BHAGE[ageunder200$SP == "PSME"]
PISI_AGE <-ageunder200$BHAGE[ageunder200$SP == "PISI"]
ttest_result <-t.test(x=PSME_AGE, y=PISI_AGE,conf.level = .95)

# Extracting the difference between means
mean_diff <- mean(ageunder200$BHAGE[ageunder200$SP == "PSME"]) -
mean(ageunder200$BHAGE[ageunder200$SP == "PISI"])

# Print the results
cat("Difference between means:", mean_diff, "\n")
cat("Is the result statistically significant at alpha = 0.05:", ttest_result$p.value < 0.05)

# linear regression models for each species
reg_psme <- lm(DBH ~ BHAGE, data = age[ageunder200$SP == "PSME", ])
reg_pisi <- lm(DBH ~ BHAGE, data = age[ageunder200$SP == "PISI", ])
reg_alru <- lm(DBH ~ BHAGE, data = age[ageunder200$SP == "ALRU", ])
modelsummary(reg_psme)
modelsummary(reg_pisi)
modelsummary(reg_alru)

# Extract R-squared values
r_squared_psme <- summary(reg_psme)$r.squared
r_squared_pisi <- summary(reg_pisi)$r.squared
r_squared_alru <- summary(reg_alru)$r.squared

# Print R-squared values
cat("R-squared value for Douglas-fir:", r_squared_psme, "\n")
cat("R-squared value for Sitka spruce:", r_squared_pisi, "\n")
cat("R-squared value for Red alder:", r_squared_alru, "\n")
