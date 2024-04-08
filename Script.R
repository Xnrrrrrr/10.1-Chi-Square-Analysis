# Read data into dataframe

lab10.data <- read.csv("lab 10 sra 365 wc-1.csv")

# Question 1

# Create a contingency table of the two categorical variables
contingency_table <- table(lab10.data$num_people_v2, lab10.data$cost_controls_v2)

# Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Print the results
print(chi_square_test)

#OR 

library(gmodels)
CrossTable(lab10.data$cost_controls_v2, lab10.data$num_people_v2, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# Question 2

# Create a contingency table of the two categorical variables
contingency_table <- table(lab10.data$dys_detect_v2, lab10.data$cost_controls_v2)

# Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Print the results
print(chi_square_test)

#OR 

library(gmodels)

# Perform cross-tabulation and chi-square test of independence
cross_table <- CrossTable(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2, 
                          fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, 
                          format = "SPSS")

# Print the results
print(cross_table)

# Question 3

# Create a contingency table of the two categorical variables
contingency_table <- table(lab10.data$per_sensitive_v2, lab10.data$cost_controls_v2)

# Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Print the results
print(chi_square_test)

# OR

library(gmodels)

# Perform cross-tabulation and chi-square test of independence
cross_table <- CrossTable(lab10.data$per_sensitive_v2, lab10.data$cost_controls_v2, 
                          fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, 
                          format = "SPSS")

# Print the results
print(cross_table)

# Question 4

# Calculate expected frequencies
expected <- chisq.test(contingency_table)$expected

# Assumption 3: Homogeneity of Variance
variances <- apply(expected, 1, var)
variance_heterogeneity <- any(variances > 5)  # Check if any variance exceeds a threshold

# Assumption 4: Low Expected Frequencies
low_expected <- any(expected < 5)  # Check if any expected frequency is below a threshold

# Print the results
cat("Homogeneity of Variance Violated:", variance_heterogeneity, "\n")
cat("Low Expected Frequencies Violated:", low_expected, "\n")


# Question 6

# Question 7

# Load the required data
lab10.data <- read.csv("lab 10 sra 365 wc-1.csv", header = TRUE)

# Install and load the 'gmodels' package
install.packages("gmodels")  # Uncomment and run this line if you haven't installed the package yet
library(gmodels)

# Perform chi-square test
cross_tab <- CrossTable(lab10.data$dys_detect_v2, lab10.data$cost_controls_v2, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# View the results
summary(cross_tab)

# OR 

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table, correct = FALSE)

# Print the results
print(chi_square_test)


# Question 8

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Step 3: Extract p-value from the chi-square test object
p_value <- chi_square_test$p.value

# Step 4: Print the p-value
print(p_value)

# Question 9 

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Step 3: Extract computed chi-square value from the chi-square test object
chi_square_value <- chi_square_test$statistic

# Step 4: Print the computed chi-square value
print(chi_square_value)

# Question 10

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Step 3: Determine if the chi-square test is statistically significant
is_significant <- chi_square_test$p.value < 0.05

# Step 4: Print the result
print(is_significant)


# Question 11

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Step 3: Calculate Cramer's V
chi_square_value <- chi_square_test$statistic
num_rows <- nrow(contingency_table)
num_cols <- ncol(contingency_table)
n <- sum(contingency_table)
effect_size <- sqrt(chi_square_value / n * min(num_rows - 1, num_cols - 1))

# Step 4: Round the effect size to two decimal places
effect_size_rounded <- round(effect_size, 2)

# Step 5: Print the effect size
print(effect_size_rounded)


# Question 12

# Question 13 

# Step 1: Create a contingency table
contingency_table <- table(lab10.data$cost_controls_v2, lab10.data$dys_detect_v2)

# Step 2: Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table,correct = FALSE))

# Print the results
print(chi_square_test)

#OR 

# Create a contingency table of the two categorical variables
contingency_table <- matrix(c(10, 45, 118, 327), nrow = 2, byrow = TRUE)
rownames(contingency_table) <- c("Fast", "Slow")
colnames(contingency_table) <- c("High", "Low")

# Perform chi-square test of independence
chi_square_test <- chisq.test(contingency_table)

# Print the observed and expected frequencies, and standardized residuals
print("Cost of Controls")
print("Days Detect\tHigh\tLow")
print("Fast")
print("Observed Frequency:")
print(contingency_table[1, ])
print("Expected Frequency:")
print(chi_square_test$expected[1, ])
print("Standardized Residual:")
print(chi_square_test$residuals[1, ])
print("Observed Frequency:")
print(contingency_table[2, ])
print("Expected Frequency:")
print(chi_square_test$expected[2, ])
print("Standardized Residual:")
print(chi_square_test$residuals[2, ])


# Question 14 
# Define the critical value for significance level 0.05
critical_value <- 1.96

# Check if any of the absolute values of standardized residuals are greater than the critical value
significant <- any(abs(chi_square_test$residuals) > critical_value)

# Print the conclusion
print(significant)


# Question 15

# Update the observed frequency for the specific cell
contingency_table[1, 1] <- 20

# Recompute the chi-square test
chi_square_test_updated <- chisq.test(contingency_table)

# Get the standardized residual for the updated cell
standardized_residual_updated <- chi_square_test_updated$residuals[1, 1]

# Round the result to 2 decimal places
standardized_residual_updated_rounded <- round(standardized_residual_updated, 2)

# Print the recalculated standardized residual
print(standardized_residual_updated_rounded)

# Define the standardized residual value
standardized_residual_value <- -5.73

# Define the critical value for significance level 0.05
critical_value <- 1.96

# Check if the absolute value of the standardized residual is greater than the critical value
if(abs(standardized_residual_value) > critical_value) {
  if(standardized_residual_value > 0) {
    # If the standardized residual is positive, the observed frequency is significantly higher than expected
    print("significantly higher than what we would expect based on chance.")
  } else {
    # If the standardized residual is negative, the observed frequency is significantly lower than expected
    print("significantly lower than what we would expect based on chance.")
  }
} else {
  # If the absolute value of the standardized residual is not greater than the critical value, it is not significantly different from expected
  print("no different than what we would expect based on chance.")
}

