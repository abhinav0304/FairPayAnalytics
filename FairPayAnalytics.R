# Installing libraries (only once)
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("maps")
# install.packages("mapdata")
# install.packages("dplyr")
install.packages("caret")
 install.packages("e1071")
 install.packages("caTools")
 install.packages("plotly")

# Loading libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(maps)
library(mapdata)
library(caret)
library(e1071)
library(caTools)
library(plotly)

# Read the data
data <- read.csv("C:/Users/kkart/Downloads/ds_salaries.csv")

# Data Preprocessing
data <- data %>%
  mutate(
    employment_type = as.factor(employment_type),
    job_title = as.factor(job_title),
    experience_level = as.factor(experience_level),
    company_size = as.factor(company_size),
    remote_ratio = as.factor(remote_ratio)
  )

# Display the first few records of the dataset
head(data)

# Check the dimensions of the dataset (rows × columns)
dim(data)

# Check the column names
names(data)

# Extract unique job titles
unique_job_titles <- unique(data$job_title)
print(unique_job_titles)

# Count total missing values in each column of the data frame
sapply(data, function(x) sum(is.na(x)))

# Count total missing values in the entire data frame
sum(is.na(data))

# Summary statistics of the dataset
summary(data)

# Data Visualization
# Plot for work_year
ggplot(data, aes(x = work_year)) + geom_histogram(binwidth = 1, fill = "gray")

# Density plot for experience_level
ggplot(data, aes(x = experience_level)) + geom_density()

# Histogram for salary
ggplot(data, aes(x = salary)) + geom_histogram(binwidth = 100000, fill = "blue")

# Density plot for salary
ggplot(data, aes(x = salary)) + geom_density()

# Density plot for salary_in_usd with dollar format
ggplot(data, aes(x = salary_in_usd)) + geom_density() + scale_x_continuous(labels = scales::dollar_format())

# Bar plot for experience level
ggplot(data, aes(x = experience_level)) + geom_bar(fill = "red")

# Bar plot for employment type
ggplot(data, aes(x = employment_type)) + geom_bar(fill = "red")

# Bar plot for job title
ggplot(data, aes(x = job_title)) + geom_bar(fill = "black")

# Bar plot for salary currency
ggplot(data, aes(x = salary_currency)) + geom_bar(fill = "black")

# Bar plot for employee residence
ggplot(data, aes(x = employee_residence)) + geom_bar(fill = "black") + coord_flip()

# Bar plot for company location
ggplot(data, aes(x = company_location)) + geom_bar(fill = "black") + coord_flip()

# Bar plot for company size
ggplot(data, aes(x = company_size)) + geom_bar(fill = "black") + coord_flip()

# Visualizing the relation between attributes
# Work year vs. Salary
sample_data <- dplyr::sample_frac(data, size = 1, replace = FALSE)
ggplot(sample_data, aes(x = work_year, y = salary)) + geom_point()

# Experience level vs. Salary
ggplot(sample_data, aes(x = experience_level, y = salary)) + geom_point()

# Job title vs. Salary with smoothing
ggplot(sample_data, aes(x = job_title, y = salary)) + geom_point() + geom_smooth() + coord_flip()

# Bar charts for salary vs. experience level
sample_data <- dplyr::sample_frac(data, size = 0.8, replace = FALSE)
ggplot(sample_data, aes(x = salary, fill = experience_level)) + geom_bar() + scale_x_continuous(labels = scales::dollar)
ggplot(sample_data, aes(x = salary, fill = experience_level)) + geom_bar(position = "dodge") + scale_x_continuous(labels = scales::dollar)

# Bar charts for employment type vs. salary
ggplot(sample_data, aes(x = employment_type, fill = salary)) + geom_bar(position = "dodge")

# Handling Categorical Data
# Performing label encoding for 'experience_level' and 'employment_type'
data$coded_experience_level <- as.integer(factor(data$experience_level))
data$employment_type <- as.integer(factor(data$employment_type))

# Visualizing encoded experience level
ggplot(data, aes(x = factor(coded_experience_level))) + geom_bar() +
  labs(x = "Encoded Experience Level", y = "Frequency", title = "Distribution of Encoded Experience Level")

# Visualizing encoded employment type
ggplot(data, aes(x = factor(employment_type))) + geom_bar() +
  labs(x = "Encoded Employment Type", y = "Frequency", title = "Distribution of Encoded Employment Type")

# Scaling and Normalizing
# Visualizing unscaled salary
ggplot(data, aes(y = salary_in_usd)) + geom_boxplot() +
  labs(y = "Salary (USD)", title = "Box Plot of unscaled Salary")

# Scaling numerical feature (salary_in_usd)
data$scaled_salary <- scale(data$salary_in_usd)  # Using standardization

# Visualizing scaled salary
ggplot(data, aes(y = scaled_salary)) + geom_boxplot() +
  labs(y = "Scaled Salary", title = "Box Plot of Scaled Salary")

# Handling Outliers
# Box plot before outlier removal
ggplot(data, aes(y = salary_in_usd)) + geom_boxplot() +
  labs(y = "Salary (USD)", title = "Box Plot of Salary Before Outlier Removal")

# Exclude missing values before calculating z-scores
salary_values <- na.omit(data$salary_in_usd)
z_scores <- scale(salary_values)

# Identifying outliers based on z-scores
outliers <- abs(z_scores) > 3  # Assuming a threshold of 3 standard deviations

# Create logical vector with the same length as the original data frame
all_outliers <- rep(FALSE, nrow(data))
all_outliers[which(!is.na(data$salary_in_usd))] <- outliers

# Subsetting the outliers to match the number of rows in the data frame
outliers <- outliers[1:nrow(data)]

# Box plot after outlier removal
ggplot(data[!outliers, ], aes(y = salary_in_usd)) +
  geom_boxplot() +
  labs(y = "Salary (USD)", title = "Box Plot of Salary After Outlier Removal")

# Dealing with Imbalanced Data
# Checking class distribution for imbalance
table(data$experience_level)

# Visualizing class distribution of experience level
ggplot(data, aes(x = factor(experience_level))) + geom_bar() +
  labs(x = "Experience Level", y = "Frequency", title = "Class Distribution of Experience Level")

# Missing Data
# Histogram before imputation
ggplot(data, aes(x = salary_in_usd)) + geom_histogram() +
  labs(x = "Salary (USD)", y = "Frequency", title = "Histogram of Salary Before Imputation")

# Checking for missing values
sum(is.na(data))

# Replacing missing values with mean
data$salary_in_usd[is.na(data$salary_in_usd)] <- mean(data$salary_in_usd, na.rm = TRUE)

# Histogram after replacing missing values
ggplot(data, aes(x = salary_in_usd)) + geom_histogram() +
  labs(x = "Salary (USD)", y = "Frequency", title = "Histogram of Salary After Imputation")

# Feature Engineering
# Creating new feature: Salary per year
data$salary_per_year <- data$salary_in_usd / (2024 - data$work_year)

# Scatter plot of salary_per_year vs. work_year
ggplot(data, aes(x = work_year, y = salary_per_year)) + geom_point() +
  labs(x = "Work Year", y = "Salary per Year", title = "Scatter Plot of Salary per Year vs. Work Year")

# Splitting the Data Set
# Splitting the dataset into training and testing sets (e.g., 80% training, 20% testing)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Double Density Plots for Visualization
# Double density plot of salary by experience level
ggplot(data, aes(x = salary_in_usd, fill = experience_level)) +
  geom_density(alpha = 0.5) +
  labs(x = "Salary (USD)", y = "Density", fill = "Experience Level", title = "Density Plot of Salary by Experience Level")

# Building the Model
set.seed(123)

# Defining the formula
formula <- salary_in_usd ~ experience_level + employment_type + job_title + work_year + remote_ratio + company_location + employee_residence + company_size

# Fitting the random forest model
rf_model <- randomForest(formula, data = train_data, ntree = 100, importance = TRUE)

# Making predictions
predictions <- predict(rf_model, newdata = test_data)

# Evaluating the model
mae <- mean(abs(predictions - test_data$salary_in_usd))
rmse <- sqrt(mean((predictions - test_data$salary_in_usd)^2))

# Printing the results
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Importance of features
importance_rf <- importance(rf_model)
var_importance <- data.frame(Variable = row.names(importance_rf), Importance = importance_rf[, 1])

# Plotting feature importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Feature", y = "Importance", title = "Feature Importance from Random Forest Model")

# Mapping Geographical Data
# Load USA map data
usa_map <- map_data("state")

# Aggregating salary by employee residence (assuming 'employee_residence' has state names)
agg_data <- data %>%
  group_by(employee_residence) %>%
  summarise(average_salary = mean(salary_in_usd, na.rm = TRUE))

# Merging aggregated data with map data
merged_data <- merge(usa_map, agg_data, by.x = "region", by.y = "employee_residence", all.x = TRUE)

# Plotting the map with average salary
ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = average_salary), color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "gray") +
  labs(fill = "Average Salary", title = "Average Salary by State", x = "", y = "") +
  theme_void()

# Interactive Plotly Visualizations
# Interactive scatter plot for salary vs. job title
plot_ly(data, x = ~job_title, y = ~salary_in_usd, type = 'scatter', mode = 'markers') %>%
  layout(title = 'Interactive Scatter Plot of Salary vs. Job Title', xaxis = list(title = 'Job Title'), yaxis = list(title = 'Salary (USD)'))

# Interactive bar chart for salary by experience level
plot_ly(data, x = ~experience_level, y = ~salary_in_usd, type = 'bar') %>%
  layout(title = 'Interactive Bar Chart of Salary by Experience Level', xaxis = list(title = 'Experience Level'), yaxis = list(title = 'Salary (USD)'))

# Predictive Modeling
# Linear regression model
linear_model <- lm(salary_in_usd ~ ., data = train_data)
summary(linear_model)

# Model predictions on test set
linear_predictions <- predict(linear_model, newdata = test_data)

# Evaluation
linear_mae <- mean(abs(linear_predictions - test_data$salary_in_usd))
linear_rmse <- sqrt(mean((linear_predictions - test_data$salary_in_usd)^2))

# Printing the results
cat("Linear Regression - Mean Absolute Error (MAE):", linear_mae, "\n")
cat("Linear Regression - Root Mean Squared Error (RMSE):", linear_rmse, "\n")

# Visualization of Actual vs. Predicted values
ggplot(test_data, aes(x = salary_in_usd, y = linear_predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Actual Salary (USD)", y = "Predicted Salary (USD)", title = "Actual vs. Predicted Salary")

# Conclusion: Understanding Salary Disparities
cat("Key drivers of salary disparities include experience level, job role, geographic location, and company size. The Random Forest model provided insights into the importance of these features. Further investigation and continuous updates to the model with new data can improve the understanding of salary structures in the job market.")

