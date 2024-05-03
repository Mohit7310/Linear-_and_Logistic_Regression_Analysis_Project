cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session


                                                    
                                                                   # Final Project
                                                      




# Load required library for ANOVA
library(stats)
# Load the ggplot2 package
library(ggplot2)

# Install and load the readxl package if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)


#1.) Does the mean sales of products differ significantly among different product categories (Furniture, Office Supplies, Technology) in the dataset?
# Load the dataset from the CSV file
superstore_sample <- read.csv("Sample - Superstore.csv")
data <- superstore_sample

# Subset the data for each product category
furniture_sales <- data[data$Category == "Furniture", "Sales"]
office_supplies_sales <- data[data$Category == "Office Supplies", "Sales"]
technology_sales <- data[data$Category == "Technology", "Sales"]

# Perform one-way ANOVA
anova_result <- aov(Sales ~ Category, data = data)

# Summarize the ANOVA results
summary(anova_result)

#  Based on the output, the p-value associated with the F statistic for the Category variable is extremely small (<< 0.05), 
# which is typically considered statistically significant.
# Therefore, we can conclude that the mean sales of products significantly differ among different product categories 
# (Furniture, Office Supplies, Technology) in the dataset.

# Post-hoc analysis (optional) - if ANOVA result is significant
posthoc_test <- TukeyHSD(anova_result)
print(posthoc_test)


# Load required library for visualization
library(ggplot2)

# Create a dataframe for mean sales across product categories
mean_sales <- data.frame(
  Category = c("Furniture", "Office Supplies", "Technology"),
  Mean_Sales = c(mean(furniture_sales), mean(office_supplies_sales), mean(technology_sales))
)

# Create a bar plot
ggplot(mean_sales, aes(x = Category, y = Mean_Sales, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Sales Across Product Categories",
       x = "Product Category",
       y = "Mean Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#2.) Is there a significant interaction effect between the 'Ship Mode' and 'Region' variables on the 'Profit' margin of orders, considering that different shipping modes and regions may impact profitability differently?
# Perform two-way ANOVA
anova_result <- aov(Profit ~ Ship.Mode * Region, data = superstore_sample)

# Summary of ANOVA
summary(anova_result)

# The p-value associated with the interaction term "Ship.Mode:Region" is 0.3595.
# Since this p-value is greater than the typical significance level of 0.05, we fail to reject the null hypothesis.

#Therefore, there is no significant interaction effect between the 'Ship Mode' and 'Region' 
#variables on the 'Profit' margin of orders based on the given dataset. 
#This suggests that the impact of different shipping modes on profitability 
#does not vary significantly across different regions, and vice versa.

# Create a data frame with mean profit margin for each combination of Ship Mode and Region
mean_profit <- aggregate(Profit ~ Ship.Mode + Region, data = superstore_sample, FUN = mean)

# Create a heatmap
ggplot(mean_profit, aes(x = Ship.Mode, y = Region, fill = Profit)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Interaction Effect between Ship Mode and Region on Profit Margin",
       x = "Ship Mode",
       y = "Region",
       fill = "Mean Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#3.) Is there a significant association between the 'Segment' of customers (Consumer, Corporate, Home Office) and the 'Ship Mode' they prefer, indicating whether different customer segments have distinct preferences for shipping methods?
# Create a contingency table
contingency_table <- table(superstore_sample$Segment, superstore_sample$Ship.Mode)

# Perform chi-square test
chi_square_result <- chisq.test(contingency_table)

# Print the result
print(chi_square_result)

# The p-value associated with Pearson's chi-squared test is approximately 0.00009005,
# which is much smaller than the typical significance level of 0.05.
# Since the p-value is below the significance level, we reject the null hypothesis.
# Therefore, we can conclude that there is a significant association between the 'Segment' 
# of customers (Consumer, Corporate, Home Office) and the 'Ship Mode' they prefer. 
# This indicates that different customer segments have distinct preferences for shipping methods.


#4.) Can we predict the 'Profit' of an order based on the 'Sales,' 'Quantity,' and 'Discount' variables, understanding the linear relationship between these factors and the overall profitability?
# Fit linear regression model
lm_model <- lm(Profit ~ Sales + Quantity + Discount, data = superstore_sample)

# Summary of the regression model
summary(lm_model)

# Based on the statistical output, we can conclude that we can predict 
# the 'Profit' of an order based on the 'Sales,' 'Quantity,' and 'Discount' variables. 
# However, it's important to note that the model explains only a portion of the 
# variability in 'Profit', and there may be other factors influencing profitability t
# hat are not captured in the model. Additionally, further analysis and validation 
# may be necessary to assess the accuracy and reliability of the predictions.

# Predict profit using the linear regression model
predicted_profit <- predict(lm_model)

# Create a scatter plot
plot(superstore_sample$Profit, predicted_profit, 
     xlab = "Actual Profit", ylab = "Predicted Profit",
     main = "Actual vs. Predicted Profit",
     col = "blue", pch = 16)

# Add a diagonal line for reference
abline(0, 1, col = "red")

# Add a legend
legend("topleft", legend = "Ideal Fit", col = "red", pch = 16)


#5.) Is there a significant relationship between the 'Discount' offered on a product and the likelihood of a customer placing a 'High' value order (considered as a binary outcome), helping us understand the impact of discounts on customer purchasing behavior?
# Load required library
library(dplyr)

# Define a binary outcome variable 'High' indicating whether an order is considered 'High' value
# Let's assume orders with Sales greater than $500 are considered 'High' value
threshold <- 500
superstore_sample <- mutate(superstore_sample, High = ifelse(Sales > threshold, 1, 0))

# Fit logistic regression model
logit_model <- glm(High ~ Discount, data = superstore_sample, family = "binomial")

# Summary of the logistic regression model
summary(logit_model)

# The coefficient for the 'Discount' variable is -0.28515. 
# This indicates that for each unit increase in the discount offered, 
# the log odds of a customer placing a 'High' value order decreases by 0.28515.
# The p-value associated with the 'Discount' variable is 0.0697, which is greater 
# than the conventional significance level of 0.05. Therefore, we fail to reject the null 
# hypothesis at the 0.05 significance level, suggesting that there may not be a 
# statistically significant relationship between the discount offered on a 
# product and the likelihood of a customer placing a 'High' value order.

# However, it's worth noting that the p-value is relatively close to 0.05, indicating a borderline significance. Depending on the context and the specific requirements of the analysis, further investigation or additional data may be needed to draw conclusive insights regarding the impact of discounts on customer purchasing behavior.

# Calculate the proportion of 'High' value orders for each discount level
discount_proportions <- superstore_sample %>%
  group_by(Discount) %>%
  summarise(Proportion_High_Value = mean(High))

# Create a bar plot
ggplot(discount_proportions, aes(x = Discount, y = Proportion_High_Value)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Discount", y = "Proportion of High Value Orders", 
       title = "Proportion of High Value Orders by Discount Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#6. What is the most predictive combination of independent variables (such as 'Product Category,' 'Ship Mode,' and 'Region') that significantly influences the 'Sales' performance, aiming to identify the most impactful factors contributing to higher sales in the dataset?

library(leaps)
# Assuming 'Sales', 'Product_Category', 'Ship_Mode', and 'Region' are relevant variables
best_subset <- regsubsets(Sales ~ Category + Ship.Mode + Region, data = data)
summary(best_subset)

# Plotting the results
plot(best_subset, scale = "adjr2")  # Adjusted R-squared

# Create a formula including the variables of interest
formula <- Sales ~ Category + Ship.Mode + Region

# Run Subset Regression
subset_reg <- regsubsets(formula, data = data)

# Get adjusted R-squared values
adj_r_squared <- summary(subset_reg)$adjr2

# Plotting the results
plot(1:length(adj_r_squared), adj_r_squared, type = "b", xlab = "Number of Predictors", ylab = "Adjusted R-squared")

