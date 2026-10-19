# Week 2: Statistical Analysis and Predictive Modeling in R

This project continues Week 1 using the same Titanic dataset, titanic_data.csv.

## Run it
1. Open this repository in RStudio or VS Code with R installed.
2. Run Week2_Titanic_Analysis.R from the repository root.
3. The script installs required packages, analyzes the Titanic data, and creates the outputs folder.
4. For the Word report, install rmarkdown and knitr, then run:
   rmarkdown::render("Week2_Titanic_Report.Rmd", output_file="Week2_Titanic_Report.docx")
5. Take screenshots of the script, console output, hypothesis tests, model summary, and plots for your assignment submission.

## Included methods
- Descriptive statistics and missing-value analysis
- Spearman correlation and Shapiro-Wilk normality checks
- Chi-square/Fisher test for sex and survival association
- Two-proportion test for survival rates by sex
- Wilcoxon test for age differences by survival
- Logistic regression for binary survival prediction
- Stratified 80/20 train/test split and 5-fold cross-validation
- Accuracy, precision, recall, specificity, F1, confusion matrix, and model diagnostics

Numeric results are produced when you run the script; do not claim unexecuted results. The 30–35 hours is the assignment's expected effort, so report actual time spent honestly.
