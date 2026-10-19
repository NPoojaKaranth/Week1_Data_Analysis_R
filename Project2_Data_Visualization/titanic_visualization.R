

required_packages <- c("ggplot2", "dplyr", "rpart", "rpart.plot", "officer")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages)) install.packages(missing_packages, repos = "https://cloud.r-project.org")
invisible(lapply(required_packages, library, character.only = TRUE))

# Locate the existing Week 1 Titanic CSV whether run from repo root or this folder.
possible_paths <- c("titanic_data.csv", "../titanic_data.csv", "../../titanic_data.csv")
data_path <- possible_paths[file.exists(possible_paths)][1]
if (is.na(data_path)) stop("Cannot find titanic_data.csv. Run this script from the repository root or Project2_Data_Visualization folder.")

titanic <- read.csv(data_path, stringsAsFactors = FALSE)
required_columns <- c("Survived", "Pclass", "Sex", "Age", "Fare", "SibSp", "Parch", "Embarked")
missing_columns <- setdiff(required_columns, names(titanic))
if (length(missing_columns)) stop(paste("Dataset is missing columns:", paste(missing_columns, collapse = ", ")))

out_dir <- if (dir.exists("Project2_Data_Visualization")) {
  "Project2_Data_Visualization/outputs"
} else {
  "outputs"
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

titanic$Survived <- as.integer(titanic$Survived)
titanic$Survival <- factor(titanic$Survived, levels = c(0, 1),
                           labels = c("Did not survive", "Survived"))
titanic$Pclass <- factor(titanic$Pclass, levels = c(1, 2, 3),
                         labels = c("1st Class", "2nd Class", "3rd Class"))
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked[titanic$Embarked == ""] <- NA

# Save an overview and group summaries.
overview <- data.frame(
  Measure = c("Passenger records", "Survived", "Did not survive", "Overall survival rate (%)"),
  Value = c(nrow(titanic), sum(titanic$Survived == 1, na.rm = TRUE),
            sum(titanic$Survived == 0, na.rm = TRUE),
            round(mean(titanic$Survived == 1, na.rm = TRUE) * 100, 2))
)
write.csv(overview, file.path(out_dir, "dataset_overview.csv"), row.names = FALSE)

by_sex <- dplyr::summarise(dplyr::group_by(titanic, Sex),
  Passengers = dplyr::n(),
  Survivors = sum(Survived == 1, na.rm = TRUE),
  SurvivalRatePercent = round(mean(Survived == 1, na.rm = TRUE) * 100, 2),
  .groups = "drop")
write.csv(by_sex, file.path(out_dir, "survival_by_sex.csv"), row.names = FALSE)

by_class <- dplyr::summarise(dplyr::group_by(titanic, Pclass),
  Passengers = dplyr::n(),
  Survivors = sum(Survived == 1, na.rm = TRUE),
  SurvivalRatePercent = round(mean(Survived == 1, na.rm = TRUE) * 100, 2),
  .groups = "drop")
write.csv(by_class, file.path(out_dir, "survival_by_class.csv"), row.names = FALSE)

# Visualization 1: survival counts
p1 <- ggplot(titanic, aes(Survival, fill = Survival)) +
  geom_bar() + labs(title = "Titanic Passenger Survival Count", x = "Outcome", y = "Passengers") +
  theme_minimal() + theme(legend.position = "none")
ggsave(file.path(out_dir, "01_survival_count.png"), p1, width = 8, height = 5, dpi = 300)

# Visualization 2: survival proportions by sex
p2 <- ggplot(titanic, aes(Sex, fill = Survival)) +
  geom_bar(position = "fill") + scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
  labs(title = "Survival Proportion by Sex", x = "Sex", y = "Percentage", fill = "Outcome") +
  theme_minimal()
ggsave(file.path(out_dir, "02_survival_by_sex.png"), p2, width = 8, height = 5, dpi = 300)

# Visualization 3: survival proportions by passenger class
p3 <- ggplot(titanic, aes(Pclass, fill = Survival)) +
  geom_bar(position = "fill") + scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
  labs(title = "Survival Proportion by Passenger Class", x = "Passenger class", y = "Percentage", fill = "Outcome") +
  theme_minimal()
ggsave(file.path(out_dir, "03_survival_by_class.png"), p3, width = 8, height = 5, dpi = 300)

# Visualization 4: passenger age distribution
p4 <- ggplot(titanic, aes(Age)) + geom_histogram(binwidth = 5, na.rm = TRUE, color = "white") +
  labs(title = "Age Distribution of Passengers", x = "Age (years)", y = "Passengers") + theme_minimal()
ggsave(file.path(out_dir, "04_age_distribution.png"), p4, width = 8, height = 5, dpi = 300)

# Visualization 5: fare distribution
p5 <- ggplot(titanic, aes(Fare)) + geom_histogram(bins = 30, na.rm = TRUE, color = "white") +
  labs(title = "Distribution of Passenger Fares", x = "Fare", y = "Passengers") + theme_minimal()
ggsave(file.path(out_dir, "05_fare_distribution.png"), p5, width = 8, height = 5, dpi = 300)

# Visualization 6: age versus fare, coloured by outcome
p6 <- ggplot(titanic, aes(Age, Fare, color = Survival)) + geom_point(alpha = 0.65, na.rm = TRUE) +
  labs(title = "Age and Fare by Survival Outcome", x = "Age (years)", y = "Fare", color = "Outcome") +
  theme_minimal()
ggsave(file.path(out_dir, "06_age_fare_survival.png"), p6, width = 8, height = 5, dpi = 300)

# Predictive models use complete cases for the selected columns.
model_data <- titanic |>
  dplyr::select(Survived, Pclass, Sex, Age, Fare, SibSp, Parch, Embarked) |>
  na.omit()
model_data$Pclass <- factor(model_data$Pclass)
model_data$Sex <- factor(model_data$Sex)
model_data$Embarked <- factor(model_data$Embarked)

logistic_model <- glm(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch + Embarked,
                      data = model_data, family = binomial())
writeLines(capture.output(summary(logistic_model)), file.path(out_dir, "logistic_regression_results.txt"))
prob <- predict(logistic_model, type = "response")
accuracy <- mean(ifelse(prob >= 0.5, 1, 0) == model_data$Survived) * 100
write.csv(data.frame(Metric = "Training accuracy (not test accuracy)", ValuePercent = round(accuracy, 2)),
          file.path(out_dir, "model_accuracy.csv"), row.names = FALSE)

tree_model <- rpart(factor(Survived) ~ Pclass + Sex + Age + Fare + SibSp + Parch + Embarked,
                    data = model_data, method = "class",
                    control = rpart.control(cp = 0.015))
png(file.path(out_dir, "07_survival_decision_tree.png"), width = 1400, height = 900, res = 150)
rpart.plot(tree_model, main = "Decision Tree for Titanic Survival")
dev.off()
writeLines(capture.output(print(tree_model)), file.path(out_dir, "decision_tree_results.txt"))

# Generate a Word report containing all charts and their interpretations.
doc <- read_docx()
doc <- body_add_par(doc, "Project 2: Data Visualization and Insight Communication Using R", style = "Title")
doc <- body_add_par(doc, "Dataset: Titanic Passenger Dataset", style = "Subtitle")
doc <- body_add_par(doc, "1. Objective", style = "heading 1")
doc <- body_add_par(doc, "This project uses R to explore the Titanic passenger dataset, create clear visualizations, communicate findings, and demonstrate introductory predictive modelling.")
doc <- body_add_par(doc, "2. Dataset Overview", style = "heading 1")
doc <- body_add_par(doc, paste0("The dataset contains ", nrow(titanic), " passenger records. ",
  sum(titanic$Survived == 1, na.rm = TRUE), " passengers survived. The analysis uses survival status, passenger class, sex, age, fare, family counts, and embarkation port."))
doc <- body_add_par(doc, "Key Variables", style = "heading 2")
doc <- body_add_par(doc, "Survived: 0 = did not survive, 1 = survived; Pclass: passenger class; Sex: passenger sex; Age: age in years; Fare: ticket fare; SibSp and Parch: family members aboard; Embarked: port of embarkation.")
doc <- body_add_par(doc, "3. Visualizations and Interpretation", style = "heading 1")

add_chart <- function(d, heading, file, note) {
  d <- body_add_par(d, heading, style = "heading 2")
  d <- body_add_img(d, src = file.path(out_dir, file), width = 6.2, height = 3.9)
  body_add_par(d, note)
}
doc <- add_chart(doc, "3.1 Overall Survival Count", "01_survival_count.png",
  "This bar chart compares survivors and non-survivors. It shows the overall distribution of the target variable and provides a starting point for the analysis.")
doc <- add_chart(doc, "3.2 Survival by Sex", "02_survival_by_sex.png",
  "The proportion chart compares survival outcomes for female and male passengers. Differences between the bars indicate that survival outcomes varied by sex in this dataset.")
doc <- add_chart(doc, "3.3 Survival by Passenger Class", "03_survival_by_class.png",
  "This chart compares survival proportions for first-, second-, and third-class passengers. It helps show the relationship between ticket class and survival.")
doc <- add_chart(doc, "3.4 Age Distribution", "04_age_distribution.png",
  "The histogram groups passenger ages into five-year intervals. Missing ages are excluded, and the chart helps identify common age ranges.")
doc <- add_chart(doc, "3.5 Fare Distribution", "05_fare_distribution.png",
  "The fare histogram shows how ticket prices are distributed. A small number of expensive tickets can produce a long right tail.")
doc <- add_chart(doc, "3.6 Age and Fare by Survival", "06_age_fare_survival.png",
  "The scatter plot compares age and fare while using colour for survival outcome. The overlapping points suggest that these variables alone do not perfectly distinguish survivors.")
doc <- add_chart(doc, "3.7 Decision Tree", "07_survival_decision_tree.png",
  "The decision tree presents feature-based rules for classifying survival. It is an exploratory model; independent test data should be used before claiming general predictive performance.")
doc <- body_add_par(doc, "4. Predictive Analysis", style = "heading 1")
doc <- body_add_par(doc, paste0("A logistic regression model was fitted using passenger class, sex, age, fare, family counts, and embarkation port. Rows with missing model values were omitted. Apparent training accuracy at a 0.5 threshold was ", round(accuracy, 2), "%. This is training accuracy, not independent test accuracy, and may overestimate performance."))
doc <- body_add_par(doc, "5. Conclusion", style = "heading 1")
doc <- body_add_par(doc, "The charts show how survival outcomes vary across passenger groups and summarize passenger ages and fares. Combining visual exploration with logistic regression and a decision tree demonstrates a basic data analysis workflow. Future work could use a train/test split, cross-validation, and precision, recall, and a confusion matrix.")
doc <- body_add_par(doc, "6. Reproducibility", style = "heading 1")
doc <- body_add_par(doc, "Run this script in R or VS Code with an R installation. It reads the Titanic CSV already present in the Week 1 repository and writes the report, charts, summaries, and model outputs to the outputs folder.")
print(doc, target = file.path(out_dir, "Titanic_Project_2_Report.docx"))

cat("\nProject 2 completed. Output folder: ", normalizePath(out_dir), "\n")
