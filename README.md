Overview
This repository contains an R-based machine learning pipeline designed to predict the risk of moderate-to-severe bone marrow fibrosis (MF grades 2/3) in patients with JAK2 V617F-mutated myeloproliferative neoplasms (MPNs).
The pipeline processes baseline clinical, hematological, and cytokine data from 336 patients, performs feature selection using Boruta and LASSO, compares multiple classification models, and interprets the best model (XGBoost) using SHAP. This code generates the data, tables, figures, and results presented in the paper, including baseline tables, differential analyses, model performance metrics, and SHAP visualizations.
Note: This code assumes access to the de-identified dataset (not included in this repo for privacy reasons; contact the authors for access).

Installation
Clone the repository:
git clone https://github.com/your-username/mpn-fibrosis-ml-pipeline.git
cd mpn-fibrosis-ml-pipeline
Install required R packages (run in R console):
install.packages(c("tidyverse", "caret", "glmnet", "Boruta", "pROC", "shapviz", "randomForest", "xgboost", "lightgbm", "catboost", "e1071", "nnet", "gbm", "adabag", "tableone", "DynNom"))
Note: Some packages like lightgbm and catboost may require additional setup (e.g., via install.packages("lightgbm", type="source") or from CRAN/GitHub).
Place your data file (e.g., "mpn_data.csv") in the data directory.

Useage
Run the main script (main.R) in RStudio or via command line: Rscript main.R. The script follows a sequential workflow, generating outputs like tables (CSV/HTML), plots (PNG/PDF), model summaries, and SHAP visualizations in the specified output directory.
