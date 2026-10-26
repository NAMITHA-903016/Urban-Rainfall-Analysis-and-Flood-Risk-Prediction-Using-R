# Urban-Rainfall-Analysis-and-Flood-Risk-Prediction-Using-R
# 🌧️ Urban Rainfall Analysis and Flood-Risk Prediction Using R

## 📌 Project Overview

**Urban Rainfall Analysis and Flood-Risk Prediction Using R** is a data analytics and machine learning project that analyzes district-wise rainfall patterns and estimates relative district-level flood susceptibility using R.

The project combines rainfall data with district-level flooded-area information to explore rainfall trends, identify patterns, and classify districts into **Low, Medium, and High susceptibility categories** using the Random Forest algorithm.

An interactive dashboard built with **R Shiny** presents rainfall analysis, district susceptibility classifications, and model evaluation results in an accessible format.

> **Important:** The current model estimates relative district-level susceptibility based on historical flooded-area information. It is not a validated prediction of whether a flood will occur on a particular future date.

---

## 🎯 Project Objectives

- Analyze daily rainfall patterns across districts.
- Clean and preprocess rainfall and flooded-area datasets.
- Extract useful rainfall features for analysis.
- Explore relationships between rainfall and district-level flooded-area information.
- Apply the Random Forest classification algorithm.
- Classify districts into Low, Medium, and High susceptibility categories.
- Evaluate model performance using appropriate classification metrics.
- Develop an interactive dashboard using R Shiny.
- Present the results through clear visualizations.

---

## 🛠️ Technologies Used

- **Programming Language:** R
- **Machine Learning:** Random Forest
- **Dashboard:** Shiny
- **Data Manipulation:** dplyr, tidyr
- **Data Visualization:** ggplot2
- **Model Evaluation:** caret and classification metrics
- **Development Environment:** RStudio

---

## 📂 Project Structure

```text
Urban_Rainfall_FloodRisk/
│
├── 01_prepare_train.R
├── app.R
├── check_model_results.R
├── README.md
│
├── rainfall_districtwise_daily_imd.csv
├── District_FloodedArea.csv
│
├── model_metrics.csv
├── class_metrics.csv
├── confusion_matrix.csv
├── random_forest_tuning.csv
├── district_risk_predictions.csv
├── feature_importance.csv
├── feature_importance.png
│
└── random_forest_model.rds
```

*Note: The model output files are generated when the training script runs successfully. Your repository may not contain all these files initially.*

---

## 📊 Datasets Used

### 1. District-Wise Daily Rainfall Dataset

**File:** `rainfall_districtwise_daily_imd.csv`

This dataset contains district-level daily rainfall observations from the India Meteorological Department (IMD).

It is used to analyze rainfall patterns and calculate rainfall-related features.

### 2. District Flooded-Area Dataset

**File:** `District_FloodedArea.csv`

This dataset contains district-level flooded-area information, including the percentage of flooded area and corrected flooded-area percentages.

The corrected flooded-area percentage is used to derive relative district susceptibility categories.

**Data limitation:** The flooded-area dataset provides district-level information rather than dated flood-event labels. Therefore, the model should not be interpreted as a validated event-based flood forecasting system.

---

## ⚙️ Methodology

### Step 1: Data Collection
Collect the district-wise daily rainfall dataset and district flooded-area dataset.

### Step 2: Data Preprocessing
- Inspect the datasets and their column names.
- Handle missing and invalid values.
- Standardize district names to support dataset matching.
- Convert dates and rainfall values into appropriate formats.
- Combine relevant information from both datasets.

### Step 3: Feature Engineering
Generate useful rainfall features, such as daily rainfall and accumulated rainfall over recent days, where the available observations support these calculations.

### Step 4: Susceptibility Classification
Use corrected flooded-area percentages to create Low, Medium, and High relative susceptibility categories.

These categories represent relative levels within the analyzed data, not independently verified flood-risk thresholds.

### Step 5: Random Forest Model
Train a Random Forest classifier using the prepared district-level features and susceptibility categories.

### Step 6: Model Evaluation
Evaluate the model on held-out test data using measures such as:
- Accuracy
- Confusion matrix
- Class-wise precision and recall
- F1-score
- Balanced accuracy, where applicable

Compare the results with a simple baseline to understand whether the model improves on a basic prediction strategy.

### Step 7: Interactive Dashboard
Use R Shiny to present rainfall analysis, susceptibility classifications, model performance, and relevant data limitations.

---

## 💻 Installation and Setup

### Prerequisites

Install the following software:

- R
- RStudio

### Step 1: Clone the Repository

Open a terminal and run:

```bash
git clone YOUR_GITHUB_REPOSITORY_URL
```

Replace `YOUR_GITHUB_REPOSITORY_URL` with your actual GitHub repository URL.

Alternatively, download the repository as a ZIP file and extract it.

### Step 2: Open the Project Folder

Open the project folder in RStudio.

Ensure that the two input CSV files are in the same folder as the R scripts, unless the scripts have been configured to use another location.

### Step 3: Install Required Packages

Run the following command in the R console:

```r
install.packages(c(
  "shiny",
  "dplyr",
  "tidyr",
  "ggplot2",
  "randomForest",
  "caret"
))
```

If the scripts use additional packages, install those packages as well.

---

## ▶️ How to Run the Project

### Step 1: Prepare Data and Train the Model

Run the following command from the project directory in RStudio:

```r
source("01_prepare_train.R")
```

This script prepares the data, trains the Random Forest model, evaluates its performance, and generates the model output files.

Check the console for errors and review the reported evaluation metrics.

### Step 2: Check Model Results

After training finishes, run:

```r
source("check_model_results.R")
```

This script can be used to inspect the generated model evaluation results.

### Step 3: Launch the Interactive Dashboard

Run:

```r
shiny::runApp("app.R")
```

The dashboard should open in your default web browser or display a local address in the RStudio console.

**Important:** Run the training script successfully before launching the dashboard because the application may depend on the generated model and output files.

---

## 📈 Expected Outputs

Depending on the available data and successful model execution, the project generates:

- Rainfall analysis and visualizations
- District-level susceptibility classifications
- Random Forest model evaluation metrics
- Confusion matrix
- Class-wise evaluation metrics
- Random Forest tuning results
- Feature importance information
- District-level prediction results
- An interactive Shiny dashboard

Actual results depend on the datasets, preprocessing, train-test split, and model performance.

---

## 📏 Model Evaluation

The model is evaluated using held-out test observations rather than training accuracy alone.

| Metric | Purpose |
|---|---|
| Accuracy | Measures the proportion of correct predictions |
| Precision | Measures how many predicted members of a class belong to that class |
| Recall | Measures how many actual members of a class were identified |
| F1-score | Balances precision and recall |
| Confusion Matrix | Shows correct and incorrect predictions by class |
| Balanced Accuracy | Helps evaluate performance when class frequencies differ |

No particular accuracy level is guaranteed. Performance depends on the amount and quality of the data, the distribution of susceptibility categories, and how well the features represent the underlying problem.

---

## 🌟 Key Features

- District-wise rainfall analysis
- Rainfall feature engineering
- Random Forest classification
- Three relative susceptibility categories
- Model evaluation and comparison
- Feature importance analysis
- Interactive R Shiny dashboard
- Visual presentation of analytical results
- Explicit documentation of data and model limitations

---

## ⚠️ Limitations

- Rainfall observations may cover a limited period and may not represent long-term climate patterns.
- District-level rainfall and flooded-area data may differ in coverage and reporting methods.
- District matching and missing data can affect the number of observations available for modeling.
- Relative susceptibility categories depend on the available flooded-area data and the classification method.
- A high classification accuracy does not, by itself, establish that the model can predict real-world floods.
- The model does not currently provide validated predictions of flood occurrence on specific future dates.

### Future Improvements

- Obtain longer historical rainfall records.
- Integrate verified flood-event records with dates and locations.
- Include river levels, drainage capacity, elevation, soil characteristics, and land-use information.
- Validate predictions against independent flood observations.
- Evaluate performance across different time periods and geographical regions.
- Develop and validate a separate event-based flood forecasting model.

---

## 🔮 Conclusion

This project demonstrates how R, machine learning, data preprocessing, and interactive visualization can be combined to analyze urban rainfall and estimate relative district-level flood susceptibility.

The Random Forest model provides a way to classify districts using the available rainfall and flooded-area information, while the Shiny dashboard makes the results easier to explore.

The project also highlights the importance of data quality, appropriate model evaluation, and scientific validation when developing flood-related analytical systems.

---

## 👩‍💻 Author

**Name:** S. Tulasi  
**Department:** Artificial Intelligence and Data Science (AI&DS)  
**Section:** AI&DS 2  
**Academic Year:** 2026–2027

---

## 📜 License

This project is intended for educational and research purposes. Add an appropriate open-source license, such as the MIT License, if you want others to reuse and modify your code.
