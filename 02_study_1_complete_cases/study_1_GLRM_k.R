# Find optimal value for k for GLRM

# Load functions
source("../project_support.R")

# Load data
data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv")

# Select only complete cases of the 15 most answered questions
data_complete <- complete_cases_filter(data, 15)

# Initiate h2o
h2o.init(nthreads = -1)

# Find optimal value of k for glrm 
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.1, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.2, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.3, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.4, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.5, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.6, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.7, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.8, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.1, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.2, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.3, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.4, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.5, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.6, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.7, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.8, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.1, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.2, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.3, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.4, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.5, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.6, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.7, seed = 1)
GLRM_k_idx(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.8, seed = 1)

# Shutdown H2O    
h2o.shutdown(prompt=FALSE)

