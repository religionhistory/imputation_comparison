# Run imputation on complete cases dataset

# Load functions
source("../project_support.R")

# Load data
data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv") 

# Filter data with <50% missing values
filtered_data <- missing_value_filter(data, 50)

# Save filtered data
if(dir.exists(file.path("../output/study2/filtered_data/")) == FALSE) {
  dir.create(file.path("../output/study2/filtered_data/"))
}
write_csv(filtered_data, paste0("../output/study2/filtered_data/filtered_data.csv"))

# Initiate h2o
h2o.init()

# Run imputation
run_imputation(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.1, kNN_k = 3, GLRM_k = 54, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.2, kNN_k = 11, GLRM_k = 2, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.3, kNN_k = 9, GLRM_k = 23, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.1, kNN_k = 1, GLRM_k = 14, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.2, kNN_k = 3, GLRM_k = 26, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.3, kNN_k = 9, GLRM_k = 15, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.1, kNN_k = 1, GLRM_k = 14, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.2, kNN_k = 3, GLRM_k = 26, seed = 658)
run_imputation(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.3, kNN_k = 9, GLRM_k = 15, seed = 658)

# Shutdown h2o    
h2o.shutdown(prompt=FALSE)


