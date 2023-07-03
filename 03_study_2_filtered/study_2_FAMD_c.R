# Find optimal number of components for FAMD

# Load functions
source("../project_support.R")

# Load data
data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv")

# Filter data with >50% missing values
filtered_data <- missing_value_filter(data, 50)

# Find optimal number of components for FAMD
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.1, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.2, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MCAR", missing_prop = 0.3, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.1, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.2, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MAR", missing_prop = 0.3, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.1, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.2, seed = 1)
FAMD_c_idx(filtered_data, study = 2, missing_pattern = "MNAR", missing_prop = 0.3, seed = 1)

