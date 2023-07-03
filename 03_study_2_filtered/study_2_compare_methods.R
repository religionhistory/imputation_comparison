# Compare imputation methods

# Load functions
source("../project_support.R")

# Find overall metrics per condition
overall_MCAR_10 <- overall_metrics(study = 2, condition = "MCAR_10")
overall_MCAR_20 <- overall_metrics(study = 2, condition = "MCAR_20")
overall_MCAR_30 <- overall_metrics(study = 2, condition = "MCAR_30")
overall_MAR_10 <- overall_metrics(study = 2, condition = "MAR_10")
overall_MAR_20 <- overall_metrics(study = 2, condition = "MAR_20")
overall_MAR_30 <- overall_metrics(study = 2, condition = "MAR_30")
overall_MNAR_10 <- overall_metrics(study = 2, condition = "MNAR_10")
overall_MNAR_20 <- overall_metrics(study = 2, condition = "MNAR_20")
overall_MNAR_30 <- overall_metrics(study = 2, condition = "MNAR_30")

# Find NRMSE foe each variable per condition
var_MCAR_10 <- variable_metrics(study = 2, condition = "MCAR_10")
var_MCAR_20 <- variable_metrics(study = 2, condition = "MCAR_20")
var_MCAR_30 <- variable_metrics(study = 2, condition = "MCAR_30")
var_MAR_10 <- variable_metrics(study = 2, condition = "MAR_10")
var_MAR_20 <- variable_metrics(study = 2, condition = "MAR_20")
var_MAR_30 <- variable_metrics(study = 2, condition = "MAR_30")
var_MNAR_10 <- variable_metrics(study = 2, condition = "MNAR_10")
var_MNAR_20 <- variable_metrics(study = 2, condition = "MNAR_20")
var_MNAR_30 <- variable_metrics(study = 2, condition = "MNAR_30")

# Find the percent of missing values remaining after imputation
percent_missing(study = 2)

