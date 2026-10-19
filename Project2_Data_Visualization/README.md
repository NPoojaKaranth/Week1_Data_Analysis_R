# Project 2 — Titanic Data Visualization and Insight Communication using R

This project continues the Titanic dataset work from Week 1. It creates six visualizations, summary tables, a logistic regression model, a decision tree, and a Word report.

## Dataset
The project uses the existing `titanic_data.csv` file in the repository root. It expects columns including `Survived`, `Pclass`, `Sex`, `Age`, `Fare`, `SibSp`, `Parch`, and `Embarked`.

## Requirements
- R installed on your computer
- VS Code with the R extension (or RStudio)
- Internet connection the first time packages are installed

The script installs missing R packages automatically: `ggplot2`, `dplyr`, `rpart`, `rpart.plot`, and `officer`.

## Run the project
1. Download or clone this repository.
2. Open the repository folder in VS Code.
3. Open `Project2_Data_Visualization/titanic_visualization.R`.
4. Start an R terminal using the VS Code Command Palette: **R: Create R Terminal**.
5. Run the complete script. Run it from the repository root or from the Project 2 folder.
6. Open the generated `outputs` folder.

## Expected outputs
- Six PNG visualizations
- A decision-tree PNG
- Dataset overview and survival summary CSV files
- Logistic regression and decision-tree text results
- `Titanic_Project_2_Report.docx`

Generated outputs are created on the computer when the script runs; they are not committed to GitHub automatically.

## Analysis included
1. Overall survival count
2. Survival proportion by sex
3. Survival proportion by passenger class
4. Passenger age distribution
5. Passenger fare distribution
6. Age versus fare by survival outcome
7. Decision tree and logistic regression

**Model note:** The reported logistic-regression accuracy is training accuracy, not independent test accuracy. Use a separate test set for an unbiased performance estimate.
