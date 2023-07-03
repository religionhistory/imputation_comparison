# Run imputation on complete cases dataset

# Load functions
source("../project_support.R")

# Load data
data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv") 

# Select only complete cases of the 15 most answered questions
data_complete <- complete_cases_filter(data, 15)

# Save complete cases data
if(dir.exists(file.path("../output/study1/complete_cases/")) == FALSE) {
  dir.create(file.path("../output/study1/complete_cases/"))
}
write_csv(data_complete, paste0("../output/study1/complete_cases/complete_cases.csv"))

# Initiate h2o
h2o.init()

# Run imputation
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.1, kNN_k = 6, GLRM_k = 6, FAMD_c = 7, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.2, kNN_k = 7, GLRM_k = 10, FAMD_c = 3, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.3, kNN_k = 11, GLRM_k = 11, FAMD_c = 7, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.4, kNN_k = 9, GLRM_k = 10, FAMD_c = 3, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.5, kNN_k = 10, GLRM_k = 8, FAMD_c = 8, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.6, kNN_k = 5, GLRM_k = 11, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.7, kNN_k = 10, GLRM_k = 8, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.8, kNN_k = 5, GLRM_k = 9, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.1, kNN_k = 8, GLRM_k = 8, FAMD_c = 6, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.2, kNN_k = 11, GLRM_k = 9, FAMD_c = 8, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.3, kNN_k = 11, GLRM_k = 9, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.4, kNN_k = 11, GLRM_k = 11, FAMD_c = 6, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.5, kNN_k = 11, GLRM_k = 8, FAMD_c = 6, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.6, kNN_k = 5, GLRM_k = 10, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.7, kNN_k = 7, GLRM_k = 9, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.8, kNN_k = 11, GLRM_k = 7, FAMD_c = 2, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.1, kNN_k = 11, GLRM_k = 9, FAMD_c = 6, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.2, kNN_k = 7, GLRM_k = 8, FAMD_c = 11, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.3, kNN_k = 10, GLRM_k = 9, FAMD_c = 11, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.4, kNN_k = 10, GLRM_k = 11, FAMD_c = 4, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.5, kNN_k = 9, GLRM_k = 6, FAMD_c = 8, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.6, kNN_k = 11, GLRM_k = 5, FAMD_c = 10, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.7, kNN_k = 7, GLRM_k = 7, FAMD_c = 7, seed = 658)
run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.8, kNN_k = 9, GLRM_k = 5, FAMD_c = 2, seed = 658)

# Shutdown h2o    
h2o.shutdown(prompt=FALSE)


