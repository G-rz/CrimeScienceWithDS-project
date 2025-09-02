# background variables 
data <- data.frame(
  ID = c(1, 4, 6, 8, 9, 10, 12, 14, 16, 18, 20, 22, 2, 3, 5, 7, 11, 13, 15, 17, 19, 21, 23, 24),
  Gender = c("M", "F", "M", "F", "M", "M", "F", "M", "M", "F", "F", "F", "M", "F", "F", "F", "F", "F", "M", "M", "F", "M", "M", "M"),
  Age = c(29, 24, 23, 22, 26, 22, 22, 32, 23, 22, 22, 45, 45, 23, 25, 23, 22, 33, 22, 22, 22, 31, 29, 27),
  Live_in_London = c("N", "Y", "Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y"),
  Q8 = c(1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 2, 1, 1, 1),
  Q9 = c("N", "N", "Y", "N", "N", "Y", "N", "N", "N", "Y", "Y", "N", "N", "N", "N", "Y", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
  Q10 = c("N", "N", "Y", "Y", "N", "N", "Y", "N", "Y", "Y", "N", "N", "Y", "N", "Y", "Y", "N", "Y", "N", "Y", "Y", "N", "N", "Y"),
  Q11 = c(1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 3, 3, 2, 2, 1, 1, 1, 1, 3, 1),
  Q3_V11 = c(3, 2, 3, 3, 1, 2, 2, 2, 3, 2, 2, 2, 2, 3, 2, 1, 1, 2, 1, 2, 3, 2, 3, 2),
  Q4_V11 = c(3, 4, 4, 3, 2, 2, 2, 4, 4, 4, 2, 3, 2, 4, 2, 2, 3, 2, 3, 4, 2, 3, 2, 3),
  Q5_V11 = c(3, 3, 3, 4, 4, 3, 2, 3, 3, 4, 2, 4, 2, 3, 3, 4, 2, 2, 3, 3, 2, 3, 3, 4),
  Q6_V11 = c(2, 3, 4, 3, 2, 4, 4, 3, 2, 3, 4, 2, 2, 3, 2, 3, 2, 2, 3, 3, 4, 2, 3, 4),
  Q7_V11 = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 4, 2, 3, 3, 3, 2, 2, 2, 3, 3, 2, 2, 4),
  Perception = c(3, 3, 3.25, 3.25, 2.5, 2.5, 2.25, 3, 3.25, 3.5, 2.25, 3.25, 2, 3.25, 2.5, 2.5, 2, 2, 2, 3, 2.5, 2.5, 2.25, 3.25)
)

# convert categorical variables to numerical values
data <- data %>%
  mutate(
    Gender = ifelse(Gender == "M", 0, 1),
    Live_in_London = ifelse(Live_in_London == "N", 0, 1),
    Q9 = ifelse(Q9 == "N", 0, 1),
    Q10 = ifelse(Q10 == "N", 0, 1)
  )

# the Pearson correlation coefficient
cor_results <- sapply(data[, c("Gender", "Age", "Live_in_London", "Q8", "Q9", "Q10", "Q11", "Q3_V11", "Q4_V11", "Q5_V11", "Q6_V11", "Q7_V11")], function(x) {
  cor.test(x, data$Perception)
})

# output
cat("Pearson correlation coefficient results：\n")
for (name in names(cor_results)) {
  cat(name, ": Correlation coefficient =", cor_results[[name]]$estimate, ", P_value =", cor_results[[name]]$p.value, "\n")
}

# Chi-square test
categorical_vars <- c("Gender", "Live_in_London", "Q8","Q9","Q10","Q11")
chi2_results <- lapply(categorical_vars, function(var) {
  table <- table(data[[var]], data$Perception)
  chisq.test(table)
})

# output
cat("\nChi-square test results：\n")
for (i in seq_along(categorical_vars)) {
  var <- categorical_vars[i]
  cat(var, ": Chi-square value =", chi2_results[[i]]$statistic, ", P_value =", chi2_results[[i]]$p.value, "\n")
}

# Fisher test
fisher_results <- lapply(categorical_vars, function(var) {
  table <- table(data[[var]], data$Q6_V11)
  fisher.test(table)
})

cat("\nFisher Accurate test results：\n")
for (i in seq_along(categorical_vars)) {
  var <- categorical_vars[i]
  cat(var, ": P_value =", fisher_results[[i]]$p.value, "\n")
}


# 创建数据集
dataset1 <- c(3.4, 4, 3.5, 3.5, 3.7, 3.9, 3.7, 3.7, 3.6, 3.9, 3.8, 4)
dataset2 <- c(3.2, 3.1, 2.9, 2.7, 2.5, 2.2, 2.3, 2, 2.5, 2.1, 2.1, 1.9)

# Shapiro-Wilk. test
shapiro1 <- shapiro.test(dataset1)
shapiro2 <- shapiro.test(dataset2)

# output
cat("Shapiro-Wilk Test for Dataset 1:\n")
print(shapiro1)
cat("\nShapiro-Wilk Test for Dataset 2:\n")
print(shapiro2)

# t-test
t_test_result <- t.test(dataset1, dataset2, var.equal = TRUE) 
print(t_test_result)

# HR differences from baseline
control_group <- c(-0.002, 0.981, 2.231, 11.514, -1.565, -1.011, -1.004, 0.265, -0.434, 0.968, -0.055, 1.093)
experimental_group <- c(-3.785, 1.964, 9.337, 2.6, -2.147, -3.063, 4.393, -1.061, 7.265, 8.751, -9.881, 2.784)

# Shapiro-Wilk test
shapiro_control <- shapiro.test(control_group)
shapiro_experimental <- shapiro.test(experimental_group)

# output
cat("Shapiro-Wilk Test for Control Group:\n")
print(shapiro_control)
cat("\nShapiro-Wilk Test for Experimental Group:\n")
print(shapiro_experimental)

# Mann–Whitney test
mann_whitney_result <- wilcox.test(control_group, experimental_group, exact = FALSE)
print(mann_whitney_result)

# environmental perception scores
control_group <- c(3.425, 3.85, 3.85, 3.575, 2.875, 3.625, 3.45, 3.425, 3.225, 3.425, 3.475, 3.225)
experimental_group <- c(3.4, 3.475, 3.025, 3.075, 2.95, 2.725, 2.725, 3.05, 2.75, 2.8, 2.8, 3.15)
# Shapiro-Wilk test
shapiro_control <- shapiro.test(control_group)
shapiro_experimental <- shapiro.test(experimental_group)
# output
cat("Shapiro-Wilk Test for Control Group:\n")
print(shapiro_control)
cat("\nShapiro-Wilk Test for Experimental Group:\n")
print(shapiro_experimental)

# t test
t_test_result <- t.test(control_group, experimental_group, var.equal = TRUE) 
print(t_test_result)

# fear of crime indicator scores in a moderately disordered environment
control_group <- c(2, 3, 4, 3, 2, 4, 4, 3, 2, 3, 4, 2)
experimental_group <- c(2, 3, 2, 3, 3, 2, 3, 2, 4, 2, 3, 2)

# Shapiro-Wilk test
shapiro_control <- shapiro.test(control_group)
shapiro_experimental <- shapiro.test(experimental_group)

# output
cat("Shapiro-Wilk Test for Control Group:\n")
print(shapiro_control)
cat("\nShapiro-Wilk Test for Experimental Group:\n")
print(shapiro_experimental)

# Mann–Whitney test
mann_whitney_result <- wilcox.test(control_group, experimental_group, exact = FALSE)
print(mann_whitney_result)

dataset1 <- c(3, 3, 3.25, 3.25, 2.5, 2.5, 2.25, 3, 3.25, 3.5, 2.25, 3.25)
dataset2 <- c(2, 3.25, 2.5, 2.5, 2, 2, 2, 3, 2.5, 2.5, 2.25, 3.25)

# Shapiro-Wilk test
shapiro1 <- shapiro.test(dataset1)
shapiro2 <- shapiro.test(dataset2)

# output
cat("Shapiro-Wilk Test for Dataset 1:\n")
print(shapiro1)
cat("\nShapiro-Wilk Test for Dataset 2:\n")
print(shapiro2)

# t-test
t_test_result <- t.test(dataset1, dataset2, var.equal = TRUE) 
print(t_test_result)


# 创建数据集
control_group <- c(3.16, 4.33, 3.25, 4.25, 3.08, 4, 3.91, 3.66, 4.41, 3.16)
experimental_group <- c(1.66, 2.91, 2.25, 2.75, 1.83, 2.66, 2.58, 2.08, 3.33, 2.5)

# Spearman’s rank-order correlation
spearman_result <- cor.test(control_group, experimental_group, method = "spearman")
print(spearman_result)

# fear of crime indicator scores
control_group <- c(2.93, 3.18, 3.5, 3.7, 2.95, 3.77, 3.7, 3.41, 3.64, 3.68)
experimental_group <- c(2.64, 2.83, 3.1, 3.22, 2.52, 3.16, 2.91, 3.1, 3.1, 3.31)

# Spearman’s rank-order correlation
spearman_result <- cor.test(control_group, experimental_group, method = "spearman")
print(spearman_result)


# environmental perception scores
control_group <- c(-0.353, 0.368, 0.817, 2.493, 0.826, 0.292, 2.408, 1.811, 0.956, 1.199)
experimental_group <- c(0.637, 2.051, 7.026, -3.711, 8.638, 1.954, 3.237, 0.313, -2.571, -3.278)

# Spearman’s rank-order correlation
spearman_result <- cor.test(control_group, experimental_group, method = "spearman")
print(spearman_result)


#Generate Tables
install.packages("gt")
library(gt)

data <- data.frame(
  Score = c("Control Group", "Experimental Group"),
  Statistics = c(XXX, XXX),
  P_value = c(XXX, XXX)
)

data %>%
  gt() %>%
  tab_header(
    title = "Table 1. Shapiro-Wilk test results of XXX"
  )

data <- data.frame(
  t = c("XXX"),
  df = c(XXX),
  P_value = c(XXX)
)

data %>%
  gt() %>%
  tab_header(
    title = "Table 2. T-test results of XXX"
  )

data <- data.frame(
  W = c(XXX),
  P_value = c(XXX)
)


data %>%
  gt() %>%
  tab_header(
    title = "Table 8. Mann-Whitney test on XXX"
  )


data <- data.frame(
  Scores = c("XXX","XXX"),
  P_value = c(XXX,XXX)
)

data %>%
  gt() %>%
  tab_header(
    title = "Table 11. Fisher’s test for XXX"
  )

data <- data.frame(
  rho = c(XXX),
  P_value = c(XXX)
)


data %>%
  gt() %>%
  tab_header(
    title = "Table 14. Spearman Correlation Test of XXX"
  )
