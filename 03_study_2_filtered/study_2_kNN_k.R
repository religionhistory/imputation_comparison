# Find optimal value for k for kNN

# Load functions
source("../project_support.R")

# Load data
data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv") 

# Filter data with >50% missing values
filtered_data <- missing_value_filter(data, 50)

# Find optimal value of k for kNN 
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.1, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.2, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.3, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.1, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.2, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.3, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.1, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.2, seed = 1)
kNN_k_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.3, seed = 1)
