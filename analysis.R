# ============================================
# WEEK 1 - DATA CLEANING AND PRELIMINARY ANALYSIS
# Titanic Dataset
# ============================================

# Step 1: Load the dataset
data <- read.csv("titanic_data.csv")

print(head(data))
print(str(data))
print(summary(data))
print(dim(data))


# ============================================
# Step 2: Check Missing Values
# ============================================

missing_values <- colSums(is.na(data))
print(missing_values)


# ============================================
# Step 3: Handle Missing Values
# ============================================

# Replace missing Age values with median Age
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)

# Treat blank Embarked values as missing
data$Embarked[data$Embarked == ""] <- NA

# Replace missing Embarked values with most common value
data$Embarked[is.na(data$Embarked)] <-
  names(sort(table(data$Embarked), decreasing = TRUE))[1]

# Remove Cabin because it has too many missing values
data$Cabin <- NULL

# Check missing values again
print(colSums(is.na(data)))


# ============================================
# Step 4: Check and Remove Duplicate Rows
# ============================================

duplicate_count <- sum(duplicated(data))

print(paste("Number of duplicate rows:", duplicate_count))

data <- data[!duplicated(data), ]

print(paste("Rows after removing duplicates:", nrow(data)))


# ============================================
# Step 5: Outlier Detection
# ============================================

# Boxplot for Age
boxplot(
  data$Age,
  main = "Boxplot of Age",
  ylab = "Age"
)

# Boxplot for Fare
boxplot(
  data$Fare,
  main = "Boxplot of Fare",
  ylab = "Fare"
)

# Calculate IQR for Fare
Q1 <- quantile(data$Fare, 0.25)
Q3 <- quantile(data$Fare, 0.75)

IQR_value <- Q3 - Q1

# Define lower and upper limits
lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

# Find Fare outliers
fare_outliers <- data[
  data$Fare < lower | data$Fare > upper,
]

print(paste(
  "Number of Fare outliers:",
  nrow(fare_outliers)
))


# ============================================
# Step 6: Normalize Numerical Variables
# ============================================

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data$Age_normalized <- normalize(data$Age)
data$Fare_normalized <- normalize(data$Fare)

# Display normalized values
print(
  head(
    data[, c(
      "Age",
      "Age_normalized",
      "Fare",
      "Fare_normalized"
    )]
  )
)


# ============================================
# Step 7: Categorical Encoding
# ============================================

# Encode Sex
data$Sex_encoded <- ifelse(
  data$Sex == "male",
  1,
  0
)

# Convert Embarked to factor
data$Embarked <- as.factor(data$Embarked)

# One-hot encode Embarked
embarked_encoded <- model.matrix(
  ~ Embarked - 1,
  data = data
)

# Give clear names
colnames(embarked_encoded) <- paste0(
  "Embarked_",
  levels(data$Embarked)
)

# Remove original Embarked column
data$Embarked <- NULL

# Add encoded columns
data <- cbind(
  data,
  embarked_encoded
)

# Display encoded data
print(
  head(
    data[, c(
      "Sex",
      "Sex_encoded",
      "Embarked_C",
      "Embarked_Q",
      "Embarked_S"
    )]
  )
)


# ============================================
# Step 8: Exploratory Data Analysis
# ============================================

# Summary statistics
print(summary(data))

# Structure of dataset
print(str(data))

# Fare distribution
hist(
  data$Fare,
  main = "Distribution of Passenger Fare",
  xlab = "Fare",
  ylab = "Number of Passengers"
)

# Survival count
barplot(
  table(data$Survived),
  main = "Survival Status of Passengers",
  xlab = "Survived (0 = No, 1 = Yes)",
  ylab = "Number of Passengers"
)

# Survival by passenger class
barplot(
  table(data$Pclass, data$Survived),
  beside = TRUE,
  main = "Survival by Passenger Class",
  xlab = "Passenger Class",
  ylab = "Number of Passengers",
  legend.text = TRUE
)

# Age vs Fare
plot(
  data$Age,
  data$Fare,
  main = "Age vs Fare",
  xlab = "Age",
  ylab = "Fare"
)


# ============================================
# Step 9: Correlation Analysis
# ============================================

numeric_data <- data[
  ,
  c(
    "Age",
    "Fare",
    "SibSp",
    "Parch",
    "Survived"
  )
]

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

print(correlation_matrix)


# ============================================
# Step 10: Final Dataset Checks
# ============================================

print("Final dataset dimensions:")
print(dim(data))

print("Final column names:")
print(colnames(data))

print("Final missing values:")
print(colSums(is.na(data)))


# ============================================
# Step 11: Initial Insights
# ============================================

cat("\n--- Initial Insights ---\n")

cat("1. The dataset contains passengers with different ages, fares, and passenger classes.\n")

cat("2. Missing values in Age and blank values in Embarked were handled during data cleaning.\n")

cat("3. Age and Fare were normalized to a 0-1 scale for preprocessing.\n")

cat("4. Categorical variables such as Sex and Embarked were converted into numerical form.\n")

cat("5. Boxplots were used to identify potential outliers in Age and Fare.\n")

cat("6. Correlation analysis was performed to understand relationships between numerical variables.\n")