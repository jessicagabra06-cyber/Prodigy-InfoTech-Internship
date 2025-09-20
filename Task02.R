## Task 02: Data Cleaning and EDA on Titanic dataset
## Dataset: train.csv (contains Survived column, the target variable)

# ===============================
# Step 0: Load Required Packages
# ===============================
library(ggplot2)
library(dplyr)
library(readr)
library(e1071)     # for skewness test
library(corrplot)  # for later correlation plots

# ===============================
# Step 1: Load Dataset
# ===============================
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)

# Quick inspection
str(titanic)
summary(titanic)
head(titanic)

# ===============================
# Step 2: Missing Values Analysis
# ===============================
colSums(is.na(titanic))  
# Output: Only Age shows missing values; Cabin has empty strings instead of NA.

# --- Visualize Age distribution to decide imputation strategy
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count.. * 5), color = "red", size = 1) +
  labs(title = "Distribution of Passenger Age", x = "Age", y = "Count")

# Boxplot for Age
ggplot(titanic, aes(y = Age)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Age")

# Test skewness
skewness(titanic$Age, na.rm = TRUE)  
# Result: Age is positively skewed → impute with median

# Impute Age with median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# ===============================
# Step 3: Duplicates Check
# ===============================
sum(duplicated(titanic))                       # No full-row duplicates
sum(duplicated(titanic[, c("Name", "Age")]))   # No duplicates based on Name + Age

# ===============================
# Step 4: Outliers
# ===============================
numeric_cols <- c("Age", "Fare", "SibSp", "Parch")
for (col in numeric_cols) {
  boxplot(titanic[[col]], main = paste("Boxplot of", col))
}
# Conclusion: Outliers exist but are realistic values → keep them.

# ===============================
# Step 5: Categorical Variables Cleaning
# ===============================
# Fix Embarked: replace "" with NA
titanic$Embarked[titanic$Embarked == ""] <- NA

# Check distribution
table(titanic$Embarked, useNA = "ifany")
# Output: C=168, Q=77, S=644, NA=2

# Impute missing Embarked with mode ("S")
titanic$Embarked[is.na(titanic$Embarked)] <- "S" ## S is the mode( the most repeated)

# Final distribution check
table(titanic$Embarked)

# ===============================
# Step 6: Drop Useless Columns
# ===============================
# Cabin is ~77% missing → drop it
sum(titanic$Cabin == "")  
titanic <- titanic %>% select(-Cabin)

# ===============================
# Step 7: Data Type Conversion
# ===============================
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass   <- as.factor(titanic$Pclass)
titanic$Sex      <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# ===============================
# Step 8: Final Check
# ===============================
str(titanic)
colSums(is.na(titanic))   # Confirm no missing values

## ===============================
## Exploratory Data Analysis (EDA)
## ===============================

# 1) Survival distribution
ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "red", "1" = "green"),
                    labels = c("Died", "Survived")) +
  labs(title = "Survival Counts", x = "Survival Status", y = "Count")

# 2) Survival by gender

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("red", "steelblue"),   # "teal" "steelblue" or "#008080"
    labels = c("Died", "Survived"),
    name = "Survival Status"
  ) +
  labs(title = "Survival by Gender", x = "Gender", y = "Count")


# 3) Survival by passenger class
ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("red", "steelblue"),
    labels = c("Died", "Survived"),
    name = "Survival Status"
  ) +
  scale_x_discrete(labels = c("1" = "First", "2" = "Second", "3" = "Third")) +
  labs(
    title = "Survival by Passenger Class",
    x = "Passenger Class",
    y = "Count",
    fill = "Survival Status"
  ) +
  theme_minimal()

# 4)Age distribution by survival
ggplot(titanic, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(
    values = c("red", "steelblue"),
    labels = c("Died", "Survived"),
    name = "Survival Status"
  ) +
  labs(
    title = "Age Distribution by Survival",
    x = "Age (years)",
    y = "Count",
    fill = "Survival Status"
  ) +
  theme_minimal()

# 5) Survival by Embarked port
ggplot(titanic, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("red", "steelblue"),
    labels = c("Died", "Survived"),
    name = "Survival Status"
  ) +
  scale_x_discrete(labels = c("C" = "Cherbourg", "Q" = "Queenstown", "S" = "Southampton")) +
  labs(
    title = "Survival by Port of Embarkation",
    x = "Port of Embarkation",
    y = "Count",
    fill = "Survival Status"
  ) +
  theme_minimal()
# 6) Correlation among numeric variables
library(corrplot)

numeric_data <- titanic %>% 
  select(Age, SibSp, Parch, Fare) %>%
  select(where(is.numeric))  # Extra safety

corr_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(corr_matrix, 
         method = "circle", 
         type = "upper", 
         tl.cex = 0.9,
         tl.col = "black",
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0, 0, 2, 0))
# 7) Boxplot of Fare by Class and Survival
ggplot(titanic, aes(x = factor(Pclass), y = Fare, fill = factor(Survived))) +
  geom_boxplot(outlier.shape = NA) +  # Hide extreme outliers for clarity
  scale_fill_manual(
    values = c("red", "steelblue"),
    labels = c("Died", "Survived"),
    name = "Survival Status"
  ) +
  scale_x_discrete(labels = c("1" = "First", "2" = "Second", "3" = "Third")) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  labs(
    title = "Fare Distribution by Class and Survival",
    x = "Passenger Class",
    y = "Fare ($)",
    fill = "Survival Status"
  ) +
  theme_minimal()

