# Week 4: Comprehensive Titanic Data Analysis
# Language: R
# Dataset: Titanic (built into base R)
# Run this script from the repository root in R or VS Code with R installed.

required <- c("ggplot2", "dplyr", "tidyr", "knitr", "rmarkdown")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
library(ggplot2)
library(dplyr)
library(tidyr)

dir.create("Week4_Titanic/outputs/graphs", recursive = TRUE, showWarnings = FALSE)
dir.create("Week4_Titanic/outputs/tables", recursive = TRUE, showWarnings = FALSE)

# The built-in Titanic data is a frequency table. Expand it into passenger rows
# so that plots and the logistic regression operate on individual observations.
data("Titanic", package = "datasets")
titanic_counts <- as.data.frame(Titanic)
titanic <- titanic_counts[rep(seq_len(nrow(titanic_counts)), titanic_counts$Freq),
                          c("Class", "Sex", "Age", "Survived")]
rownames(titanic) <- NULL
titanic <- titanic %>%
  mutate(
    Survived = factor(Survived, levels = c("No", "Yes")),
    Class = factor(Class, levels = c("1st", "2nd", "3rd", "Crew")),
    Sex = factor(Sex),
    Age = factor(Age, levels = c("Child", "Adult"))
  )

# Basic data checks and clean dataset export
write.csv(titanic, "Week4_Titanic/outputs/tables/titanic_cleaned.csv", row.names = FALSE)
capture.output(
  list(
    dimensions = dim(titanic),
    column_types = str(titanic),
    missing_values = colSums(is.na(titanic)),
    summary = summary(titanic)
  ),
  file = "Week4_Titanic/outputs/tables/data_quality_summary.txt"
)

# Descriptive statistics
overall <- titanic %>%
  count(Survived) %>%
  mutate(percent = round(100 * n / sum(n), 2))
by_sex <- titanic %>%
  count(Sex, Survived) %>%
  group_by(Sex) %>%
  mutate(percent_within_group = round(100 * n / sum(n), 2)) %>%
  ungroup()
by_class <- titanic %>%
  count(Class, Survived) %>%
  group_by(Class) %>%
  mutate(percent_within_group = round(100 * n / sum(n), 2)) %>%
  ungroup()
by_age <- titanic %>%
  count(Age, Survived) %>%
  group_by(Age) %>%
  mutate(percent_within_group = round(100 * n / sum(n), 2)) %>%
  ungroup()

write.csv(overall, "Week4_Titanic/outputs/tables/overall_survival.csv", row.names = FALSE)
write.csv(by_sex, "Week4_Titanic/outputs/tables/survival_by_sex.csv", row.names = FALSE)
write.csv(by_class, "Week4_Titanic/outputs/tables/survival_by_class.csv", row.names = FALSE)
write.csv(by_age, "Week4_Titanic/outputs/tables/survival_by_age.csv", row.names = FALSE)

# Graph 1: overall passenger survival
p1 <- ggplot(titanic, aes(x = Survived, fill = Survived)) +
  geom_bar() +
  labs(title = "Titanic Passengers: Survival Counts",
       x = "Survival outcome", y = "Number of passengers",
       caption = "Source: Titanic frequency table included with R") +
  theme_minimal() + theme(legend.position = "none")
ggsave("Week4_Titanic/outputs/graphs/01_overall_survival.png", p1, width = 7, height = 5, dpi = 300)

# Graph 2: survival proportions by sex
p2 <- ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Proportion by Sex", x = "Sex", y = "Share of passengers",
       fill = "Survived") + theme_minimal()
ggsave("Week4_Titanic/outputs/graphs/02_survival_by_sex.png", p2, width = 7, height = 5, dpi = 300)

# Graph 3: survival proportions by passenger class
p3 <- ggplot(titanic, aes(x = Class, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Proportion by Passenger Class", x = "Passenger class",
       y = "Share of passengers", fill = "Survived") + theme_minimal()
ggsave("Week4_Titanic/outputs/graphs/03_survival_by_class.png", p3, width = 7, height = 5, dpi = 300)

# Graph 4: survival proportions by age group
p4 <- ggplot(titanic, aes(x = Age, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Survival Proportion by Age Group", x = "Age group",
       y = "Share of passengers", fill = "Survived") + theme_minimal()
ggsave("Week4_Titanic/outputs/graphs/04_survival_by_age.png", p4, width = 7, height = 5, dpi = 300)

# Statistical association tests. These show association, not causation.
sex_test <- chisq.test(table(titanic$Sex, titanic$Survived))
class_test <- chisq.test(table(titanic$Class, titanic$Survived))
age_test <- chisq.test(table(titanic$Age, titanic$Survived))
capture.output(
  list(sex_survival_chi_square = sex_test,
       class_survival_chi_square = class_test,
       age_survival_chi_square = age_test),
  file = "Week4_Titanic/outputs/tables/association_tests.txt"
)

# Logistic regression: estimate survival probability using sex, class and age.
# Use a stratified random split so both outcomes are represented in train/test.
set.seed(42)
train_idx <- unlist(lapply(split(seq_len(nrow(titanic)), titanic$Survived),
                           function(idx) sample(idx, floor(0.8 * length(idx)))))
train <- titanic[train_idx, ]
test <- titanic[-train_idx, ]
model <- glm(Survived ~ Sex + Class + Age, data = train, family = binomial)
prob <- predict(model, newdata = test, type = "response")
pred <- factor(ifelse(prob >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
confusion <- table(Predicted = pred, Actual = test$Survived)
accuracy <- mean(pred == test$Survived)
precision <- if (sum(pred == "Yes") == 0) NA_real_ else
  sum(pred == "Yes" & test$Survived == "Yes") / sum(pred == "Yes")
recall <- if (sum(test$Survived == "Yes") == 0) NA_real_ else
  sum(pred == "Yes" & test$Survived == "Yes") / sum(test$Survived == "Yes")
metrics <- data.frame(
  metric = c("Accuracy", "Precision (survived)", "Recall (survived)"),
  value = c(accuracy, precision, recall)
)
write.csv(as.data.frame.matrix(confusion), "Week4_Titanic/outputs/tables/confusion_matrix.csv")
write.csv(metrics, "Week4_Titanic/outputs/tables/model_metrics.csv", row.names = FALSE)
capture.output(summary(model), file = "Week4_Titanic/outputs/tables/logistic_regression_summary.txt")
capture.output(confusion, metrics, file = "Week4_Titanic/outputs/tables/model_evaluation.txt")

# Export model coefficients as odds ratios for easier interpretation.
or_table <- data.frame(
  term = names(coef(model)),
  coefficient = unname(coef(model)),
  odds_ratio = exp(unname(coef(model)))
)
write.csv(or_table, "Week4_Titanic/outputs/tables/model_odds_ratios.csv", row.names = FALSE)

cat("\nAnalysis complete. Files were saved under Week4_Titanic/outputs/.\n")
cat("Test-set accuracy:", round(accuracy * 100, 2), "%\n")
