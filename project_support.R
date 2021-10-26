# Load packages
library(tidyverse)
library(mice)
library(missForest)
library(VIM)
library(missMDA)
library(h2o)
library(Metrics)
library(devtools)

# Load na generation functions
source_url('https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/generate/amputation.R')

# Functions
# Filter data to select only complete cases
complete_cases_filter <- function(data, no_most_answered) {
  # Extract percentage missing per variable
  missing <- data %>%
    select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`) %>%
    summarise_all(list(~(sum(is.na(.))/length(.))*100)) %>%
    pivot_longer(everything(), names_to = "Question_ID", values_to = "Percentage NA") %>%
    mutate(`Percentage NA` = round(`Percentage NA`, 2)) %>%
    mutate(`Question ID` = gsub("_.*$", "", Question_ID)) %>%
    mutate(`Question ID` = as.numeric(`Question ID`)) %>%
    left_join(questions) %>%
    mutate(`Percentage Non Missing` = round(100 - `Percentage NA`, 2)) %>%
    arrange(`Percentage NA`) %>%
    mutate(id = row_number())
  # Filter by No. most answered questions
  most_answered_questions <- missing %>%
    filter(id <= no_most_answered)
    # Select most answered questions from dataset
    data_filt <- data %>% 
    select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`, all_of(most_answered_questions$Question_ID))
  # Extract complete cases
  data_complete <- data_filt[complete.cases(data_filt),]
  # Recombine entries where the different groups of people and time periods that have the same answers for all questions
  data_complete <- as_tibble(data_complete) %>%
    group_by(across(c(-`Branching question`))) %>%
    summarise(`Branching question` = paste(unique(`Branching question`), collapse=","), .groups = "keep") %>% 
    ungroup() %>%
    group_by(across(c(-`End Date`, -`Start Date`))) %>%
    summarise(`Start Date` = min(`Start Date`), `End Date` = min(`End Date`), .groups = "keep") %>% 
    ungroup() %>%
    select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, everything())
}

# Filter data by percentage of non-missing values
missing_value_filter <- function(data, percentage_non_missing) {
  # Extract percentage missing per variable
  missing <- data %>%
    select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`) %>%
    summarise_all(list(~(sum(is.na(.))/length(.))*100)) %>%
    pivot_longer(everything(), names_to = "Question_ID", values_to = "Percentage_NA") %>%
    mutate(Percentage_NA = round(Percentage_NA, 2)) %>%
    mutate(`Question ID` = gsub("_.*$", "", Question_ID)) %>%
    mutate(`Question ID` = as.numeric(`Question ID`)) %>%
    left_join(questions) %>%
    mutate(percentage_present = round(100 - Percentage_NA, 2)) %>%
    arrange(Percentage_NA) %>%
    mutate(id = row_number())
  # Filter by percentage of answered questions
  var_filter <- missing %>%
    filter(percentage_present >= percentage_non_missing)
  # Select most answered questions from dataset
  data_filt <- data %>% 
    select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`, all_of(var_filter$Question_ID))
  # Recombine entries where the different groups of people and time periods that have the same answers for all questions
  data_filt <- as_tibble(data_filt) %>%
    group_by(across(c(-`Branching question`))) %>%
    summarise(`Branching question` = paste(unique(`Branching question`), collapse=","), .groups = "keep") %>% 
    ungroup() %>%
    group_by(across(c(-`End Date`, -`Start Date`))) %>%
    summarise(`Start Date` = min(`Start Date`), `End Date` = min(`End Date`), .groups = "keep") %>% 
    ungroup() %>%
    select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, everything())
  # Filter variables with no answers to the selected questions
  quest_var <- data_filt %>%
    select(all_of(var_filter$Question_ID)) 
  just_na <- rowSums(is.na(quest_var)) == ncol(quest_var)
  just_na <- cbind(just_na, row_no = seq(1:nrow(quest_var))) 
  just_na <- as_tibble(just_na) %>%
    filter(just_na == 1)
  data_filt <- data_filt %>%
    mutate(row_no = row_number()) %>%
    filter(!row_no %in% just_na$row_no) %>%
    select(-row_no)
  data_filt
}

# Replace branching questions with separate variables for each group of people
replace_branching <- function(data){
  data <- data %>%
    rename("ID" = `Entry ID`, start_date = `Start Date`, end_date = `End Date`, region = `Region ID`) %>%
    mutate(elite = ifelse(grepl("E", `Branching question`), "1", "0")) %>%
    mutate(non_elite = ifelse(grepl("N", `Branching question`), "1", "0")) %>%
    mutate(religious_specialist = ifelse(grepl("R", `Branching question`), "1", "0")) %>%
    select(-`Branching question`) %>%
    select(ID, elite, non_elite, religious_specialist, everything()) 
}

# Add an additional X% missing values to complete data for testing prediction accuracy 
add_NA <- function(data, study, missing_pattern, missing_prop){
  # Create output folder
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/additional_NA/")) == FALSE) {
    dir.create(file.path("../output/study1/additional_NA/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/additional_NA/")) == FALSE) {
    dir.create(file.path("../output/study2/additional_NA/"))
  }
  # Remove ID variables
  id_var <- data %>% select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`)
  data_no_id <- data %>% select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`)
  # Create empty list
  missing_list <- list()
  for(i in 1:5){
 #for(i in 1:100){
    # Add missing values
    missingness <- produce_NA(data_no_id, mechanism = missing_pattern, perc.missing = missing_prop, seed = i)
    missing <- cbind(id_var, missingness$data.incomp)
    missing <- replace_branching(missing)
    missing_list[[i]] <- missing
    missing_per = missing_prop * 100
  if(study == 1){
   write_csv(missing, paste0("../output/study1/additional_NA/NA_", missing_pattern, "_", missing_per, "_", i, ".csv"))
  } else if(study == 2){
    write_csv(missing, paste0("../output/study2/additional_NA/NA_", missing_pattern, "_", missing_per, "_", i, ".csv"))
  }}
  missing_list
}

# Create index of variable types for GLRM and conversion of data type
variables_types <- function(data){
  var_types <- data.frame(ID = colnames(data)) %>%
    mutate(`Question ID` = gsub("_.*$","", ID)) %>%
    mutate(`Question ID` = gsub("[^0-9.-]", "", `Question ID`)) %>%
    mutate(`Question ID` = as.numeric(`Question ID`)) %>%
    left_join(questions) %>%
    mutate(loss_func = case_when(ID == "ID" ~ "Categorical",
                                 ID == "elite" ~ "Categorical",
                                 ID == "non_elite" ~ "Categorical",
                                 ID == "religious_specialist" ~ "Categorical",
                                 ID == "start_date" ~ "Absolute",
                                 ID == "end_date" ~ "Absolute",
                                 ID == "region" ~ "Categorical",
                                 `Data Type` == "Nominal" ~ "Categorical",
                                 `Data Type` == "Nominal - Other"  ~ "Categorical",
                                 `Data Type` == "Nominal - Multiple"  ~ "Categorical",
                                 `Data Type` == "Discrete" ~ "Absolute",
                                 `Data Type` == "Continuous" ~ "Absolute"))
}

# Convert variables to correct class
correct_class <- function(data, var_types) {
  continuous <- filter(var_types, `Data Type` == "Continuous")
  discrete <- filter(var_types, `Data Type` == "Discrete")
  nominal <- filter(var_types, `Data Type` == "Nominal" | `Data Type` == "Nominal - Multiple" | `Data Type` == "Nominal - Other")
  data <- data %>%
    mutate(across(ID:religious_specialist, factor)) %>%
    mutate(across(start_date:end_date, as.integer)) %>%
    mutate(region = factor(region)) %>%
    mutate(across(nominal$ID, factor)) %>%
    mutate(across(continuous$ID, as.numeric)) %>%
    mutate(across(discrete$ID, as.integer))
  data <- as.data.frame(data)
}

# Find optimal value of k for GLRM
GLRM_k_idx <- function(data, study, missing_pattern, missing_prop, seed) {
  
  # Remove ID variables
  id_var <- data %>% select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`)
  data_no_id <- data %>% select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`)

  # Replace branching questions with separate variables for each group of people
  data <- replace_branching(data)
  
  # Index all cells with non-added missing data
  if(study == 2){
    org_missing_idx <- as.data.frame(which(is.na(data), arr.ind=TRUE)) 
  }
  
  # Add missing values
  missingness <- produce_NA(data_no_id, mechanism = missing_pattern, perc.missing = missing_prop, seed = seed)
  missingness <- cbind(id_var, missingness$data.incomp)
  
  # Replace branching questions with separate variables for each group of people
  data_missing <- replace_branching(missingness)
  
  # Create index of variable types for GLRM and conversion of data type
  var_types <- variables_types(data_missing)
  
  # Convert variables to correct class
  data <- correct_class(data, var_types)
  data_class <- correct_class(data_missing, var_types)
  
  # Index all cells with missing data
  missing_idx <- as.data.frame(which(is.na(data_class), arr.ind=TRUE)) 
  # Index only additional missing data
  if(study == 2){
    missing_idx <- anti_join(missing_idx, org_missing_idx)
  }
  
  # Extract the original values of all data points replaced with NA
  observed <- list()
  for(i in 1:nrow(missing_idx)) {
    observed[[i]] = data[missing_idx$row[i], missing_idx$col[i]]
  }
  observed = as.data.frame(unlist(observed))
  
  # Run GLRM
  validation_data <- as.h2o(data)
  GLRM_data <- as.h2o(data_class)
  GLRM_list <- list()
  glrm_imp <- possibly(h2o.glrm, otherwise = NA)
  for (i in 1:(ceiling(ncol(data_class)/2))){
   GLRM_list[[i]] <- glrm_imp(training_frame = GLRM_data, validation_frame = validation_data, cols = 1:ncol(GLRM_data), k = i,
                               loss = "Absolute", transform = "None", regularization_x = "None", regularization_y = "None",
                               multi_loss = "Categorical", loss_by_col = var_types$loss_func, loss_by_col_idx = c(0:(ncol(GLRM_data)-1)),
                               recover_svd = TRUE, seed = seed, ignore_const_cols = TRUE, max_iterations = 1000)
  }
  
  if(length(GLRM_list) == 0) {
    output <- "The algorithm fails to converge."
  } else {
    
  # Predict missing values
  predict_na <- list()
  for (i in 1:length(GLRM_list)){
    set.seed(seed)
    predict_na[[i]] <- h2o.predict(GLRM_list[[i]], GLRM_data)
  }
  predict_na <- lapply(predict_na, function(x) as.data.frame(x))
  
  # Extract the predicted values of all data points replaced with NA
  predict_list <- list()
  for(i in 1:length(predict_na)) {
    predicted <- predict_na[[i]]
    data_list <- list()
    for(j in 1:nrow(missing_idx)) {
      data_list[[j]] <- predicted[missing_idx$row[j], missing_idx$col[j]]
    }
    predict_list[[i]] <- as.data.frame(unlist(data_list))
  }
  
  # Bind predicted and observed values
  predict_list <- lapply(predict_list, setNames, nm = "predicted")
  predicted <- lapply(predict_list, function(x) as.numeric(as.character(x$predicted)))
  colnames(observed) <- "observed"
  observed <- observed %>% mutate(observed = as.numeric(as.character(observed)))
  predict_comp <- Map(cbind, predicted, observed)
  predict_comp <- lapply(predict_comp, function(x) as.data.frame(x))
  predict_comp <- lapply(predict_comp, setNames, nm = c("predicted", "observed"))
  
  # Find difference between predicted and actual values
  predict_diff <- lapply(predict_comp, function(x) {x$difference <- abs(x$observed - x$predicted); return(x)})
  predict_pcc <- lapply(predict_comp, function(x) sum(x$observed == x$predicted)/nrow(x))
  predict_pcc <- unlist(predict_pcc)
  
  # Calculate NRMSE using the difference between maximum and minimum
  numeric_data <- data %>% select(-ID, -elite, -non_elite, -religious_specialist, -start_date, -end_date, -region) %>% mutate(across(everything(), as.character)) %>% mutate(across(everything(), as.numeric))
  predict_nrmse <- lapply(predict_comp, function(x) rmse(x$observed, x$predicted)/ (max(numeric_data, na.rm = T) - min(numeric_data, na.rm = T)))
  predict_nrmse <- unlist(predict_nrmse)
  predict_df <- data.frame(k = 1:length(predict_nrmse), nrmse = predict_nrmse, pcc = round(predict_pcc, 3)) %>%
    mutate(`K NRMSE PCC` = paste0(k, " ", nrmse, " ", pcc)) %>%
    select(`K NRMSE PCC`)
  
  # Find index of value of K with the highest accuracy
  k_idx <- which.min(predict_nrmse)
  
  # Prepare output
  output <- rbind(paste("Optimal value k =", k_idx, "\n\n", colnames(predict_df)), predict_df) 
  colnames(output) <- ""
  }
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/k_value/")) == FALSE) {
    dir.create(file.path("../output/study1/k_value/"))
  }
  if(dir.exists(file.path("../output/study1/k_value/GLRM/")) == FALSE) {
    dir.create(file.path("../output/study1/k_value/GLRM/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/k_value/")) == FALSE) {
    dir.create(file.path("../output/study2/k_value/"))
  }
  if(dir.exists(file.path("../output/study2/k_value/GLRM/")) == FALSE) {
    dir.create(file.path("../output/study2/k_value/GLRM/"))
  }
  missing_per = missing_prop * 100
  if(study == 1){
  write.table(output, file = paste0("../output/study1/k_value/GLRM/", missing_pattern, "_", missing_per, "_GLRM_k_value.txt"), quote = FALSE, row.names = FALSE)
  } else if(study == 2){
  write.table(output, file = paste0("../output/study2/k_value/GLRM/", missing_pattern, "_", missing_per, "_GLRM_k_value.txt"), quote = FALSE, row.names = FALSE)
  }}

# Find optimal value of k for kNN
kNN_k_idx <- function(data, study, missing_pattern, missing_prop, seed) {
  
  # Remove ID variables
  id_var <- data %>% select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`)
  data_no_id <- data %>% select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`)
  
  # Replace branching questions with separate variables for each group of people
  data <- replace_branching(data)
  
  # Index all cells with non-added missing data
  if(study == 2){
    org_missing_idx <- as.data.frame(which(is.na(data), arr.ind=TRUE)) 
  }
  
  # Add missing values
  missingness <- produce_NA(data_no_id, mechanism = missing_pattern, perc.missing = missing_prop, seed = seed)
  missingness <- cbind(id_var, missingness$data.incomp)
  
  # Replace branching questions with separate variables for each group of people
  data_missing <- replace_branching(missingness)
  
  # Create index of variable types for GLRM and conversion of data type
  var_types <- variables_types(data_missing)
  
  # Convert variables to correct class
  data <- correct_class(data, var_types)
  data_class <- correct_class(data_missing, var_types)
  
  # Index all cells with missing data
  missing_idx <- as.data.frame(which(is.na(data_class), arr.ind=TRUE)) 
  # Index only additional missing data
  if(study == 2){
    missing_idx <- anti_join(missing_idx, org_missing_idx)
  }
  
  # Extract the original values of all data points replaced with NA
  observed <- list()
  for(i in 1:nrow(missing_idx)) {
    observed[[i]] = data[missing_idx$row[i], missing_idx$col[i]]
  }
  observed = as.data.frame(unlist(observed))
  
  # KNN imputation
  knn_imp <- possibly(kNN, otherwise = NA)
  kNN_list <- list()
  for (i in 1:(ceiling(ncol(data_class)/2))){
    set.seed(seed)
    kNN_list[[i]] <- knn_imp(data_class, k = i, impNA = TRUE, imp_var = FALSE)
  }
  
  if(length(kNN_list) == 0) {
    output <- "The algorithm fails to converge."
  } else {
  
  # Extract the predicted values of all data points replaced with NA
  predict_list <- list()
  for(i in 1:length(kNN_list)) {
    predicted <- kNN_list[[i]]
    data_list <- list()
    for(j in 1:nrow(missing_idx)) {
      data_list[[j]] <- predicted[missing_idx$row[j], missing_idx$col[j]]
    }
    predict_list[[i]] <- as.data.frame(unlist(data_list))
  }
  
  # Bind predicted and observed values
  predict_list <- lapply(predict_list, setNames, nm = "predicted")
  predicted <- lapply(predict_list, function(x) as.numeric(as.character(x$predicted)))
  colnames(observed) <- "observed"
  observed <- observed %>% mutate(observed = as.numeric(as.character(observed)))
  predict_comp <- Map(cbind, predicted, observed)
  predict_comp <- lapply(predict_comp, function(x) as.data.frame(x))
  predict_comp <- lapply(predict_comp, setNames, nm = c("predicted", "observed"))
  
  # Find difference between predicted and actual values
  predict_diff <- lapply(predict_comp, function(x) {x$difference <- abs(x$observed - x$predicted); return(x)})
  predict_pcc <- lapply(predict_comp, function(x) sum(x$observed == x$predicted)/nrow(x))
  predict_pcc <- unlist(predict_pcc)
  
  # Calculate NRMSE using the difference between maximum and minimum
  numeric_data <- data %>% select(-ID, -elite, -non_elite, -religious_specialist, -start_date, -end_date) %>% mutate(across(everything(), as.character)) %>% mutate(across(everything(), as.numeric))
  predict_nrmse <- lapply(predict_comp, function(x) rmse(x$observed, x$predicted)/ (max(numeric_data, na.rm = T) - min(numeric_data, na.rm = T)))
  predict_nrmse <- unlist(predict_nrmse)
  predict_df <- data.frame(k = 1:length(predict_nrmse), nrmse = predict_nrmse, pcc = round(predict_pcc, 3)) %>%
    mutate(`K NRMSE PCC` = paste0(k, " ", nrmse, " ", pcc)) %>%
    select(`K NRMSE PCC`)
  
  # Find index of value of K with the highest accuracy
  k_idx <- which.min(predict_nrmse)
  
  # Prepare output
  output <- rbind(paste("Optimal value k =", k_idx, "\n\n", colnames(predict_df)), predict_df) 
  colnames(output) <- ""
  }
  
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/k_value/")) == FALSE) {
    dir.create(file.path("../output/study1/k_value/"))
  }
  if(dir.exists(file.path("../output/study1/k_value/kNN/")) == FALSE) {
    dir.create(file.path("../output/study1/k_value/kNN/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/k_value/")) == FALSE) {
    dir.create(file.path("../output/study2/k_value/"))
  }
  if(dir.exists(file.path("../output/study2/k_value/kNN/")) == FALSE) {
    dir.create(file.path("../output/study2/k_value/kNN/"))
  }
  missing_per = missing_prop * 100
  if(study == 1){
  write.table(output, file = paste0("../output/study1/k_value/kNN/", missing_pattern, "_", missing_per, "_kNN_k_value.txt"), quote = FALSE, row.names = FALSE)
  } else if(study == 2){
  write.table(output, file = paste0("../output/study2/k_value/kNN/", missing_pattern, "_", missing_per, "_kNN_k_value.txt"), quote = FALSE, row.names = FALSE)
}}

# Find optimal number of components for FAMD
FAMD_c_idx <- function(data, study, missing_pattern, missing_prop, seed) {
  
  # Remove ID variables
  id_var <- data %>% select(`Entry ID`, `Branching question`, `Start Date`, `End Date`, `Region ID`)
  data_no_id <- data %>% select(-`Entry ID`, -`Branching question`, -`Start Date`, -`End Date`, -`Region ID`)
  
  # Replace branching questions with separate variables for each group of people
  data <- replace_branching(data)
  
  # Index all cells with non-added missing data
  if(study == 2){
    org_missing_idx <- as.data.frame(which(is.na(data), arr.ind=TRUE)) 
  }
  
  # Add missing values
  missingness <- produce_NA(data_no_id, mechanism = missing_pattern, perc.missing = missing_prop, seed = seed)
  missingness <- cbind(id_var, missingness$data.incomp)
  
  # Replace branching questions with separate variables for each group of people
  data_missing <- replace_branching(missingness)
  
  # Create index of variable types for GLRM and conversion of data type
  var_types <- variables_types(data_missing)
  
  # Convert variables to correct class
  data <- correct_class(data, var_types)
  data_class <- correct_class(data_missing, var_types)
  
  # Index all cells with missing data
  missing_idx <- as.data.frame(which(is.na(data_class), arr.ind=TRUE)) 
  # Index only additional missing data
  if(study == 2){
    missing_idx <- anti_join(missing_idx, org_missing_idx)
  }
  
  # Extract the original values of all data points replaced with NA
  observed <- list()
  for(i in 1:nrow(missing_idx)) {
    observed[[i]] = data[missing_idx$row[i], missing_idx$col[i]]
  }
  observed = as.data.frame(unlist(observed))
  
  # FAMD imputation
  famd_imp <- possibly(imputeFAMD, otherwise = NA)
  FAMD_list <- list()
  for (i in 1:(ceiling(ncol(data_class)/2))){
    FAMD_imputed <- famd_imp(data_class, ncp = i, seed = seed)
    if(!is.logical(FAMD_imputed)) {
    FAMD_list[[i]] <- FAMD_imputed$completeObs %>%
      mutate(across(everything(), ~gsub("^[^_]*_", "", .))) %>%
      mutate(across(everything(), ~gsub("^[^_]*_", "", .))) 
  }}
  
  if(length(FAMD_list) == 0) {
    output <- "The algorithm fails to converge."
  } else {
  
  # Extract the predicted values of all data points replaced with NA
  predict_list <- list()
  for(i in 1:length(FAMD_list)) {
    predicted <- FAMD_list[[i]]
    data_list <- list()
    for(j in 1:nrow(missing_idx)) {
      data_list[[j]] <- predicted[missing_idx$row[j], missing_idx$col[j]]
    }
    predict_list[[i]] <- as.data.frame(unlist(data_list))
  }
  
  # Bind predicted and observed values
  predict_list <- lapply(predict_list, setNames, nm = "predicted")
  predicted <- lapply(predict_list, function(x) as.numeric(as.character(x$predicted)))
  colnames(observed) <- "observed"
  observed <- observed %>% mutate(observed = as.numeric(as.character(observed)))
  predict_comp <- Map(cbind, predicted, observed)
  predict_comp <- lapply(predict_comp, function(x) as.data.frame(x))
  predict_comp <- lapply(predict_comp, setNames, nm = c("predicted", "observed"))
  
  # Find difference between predicted and actual values
  predict_diff <- lapply(predict_comp, function(x) {x$difference <- abs(x$observed - x$predicted); return(x)})
  predict_pcc <- lapply(predict_comp, function(x) sum(x$observed == x$predicted)/nrow(x))
  predict_pcc <- unlist(predict_pcc)
  
  # Calculate NRMSE using the difference between maximum and minimum
  numeric_data <- data %>% select(-ID, -elite, -non_elite, -religious_specialist, -start_date, -end_date) %>% mutate(across(everything(), as.character)) %>% mutate(across(everything(), as.numeric))
  predict_nrmse <- lapply(predict_comp, function(x) rmse(x$observed, x$predicted)/ (max(numeric_data, na.rm = T) - min(numeric_data, na.rm = T)))
  predict_nrmse <- unlist(predict_nrmse)
  predict_df <- data.frame(k = 1:length(predict_nrmse), nrmse = predict_nrmse, pcc = round(predict_pcc, 3)) %>%
    mutate(`C NRMSE PCC` = paste0(k, " ", nrmse, " ", pcc)) %>%
    select(`C NRMSE PCC`)
  
  # Find index of value of c with the highest accuracy
  c_idx <- which.min(predict_nrmse)
  
  # Prepare output
  output <- rbind(paste("Optimal number of components =", c_idx, "\n\n", colnames(predict_df)), predict_df) 
  colnames(output) <- ""
  }
  
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/c_value/")) == FALSE) {
    dir.create(file.path("../output/study1/c_value/"))
  }
  if(dir.exists(file.path("../output/study1/c_value/FAMD/")) == FALSE) {
    dir.create(file.path("../output/study1/c_value/FAMD/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/c_value/")) == FALSE) {
    dir.create(file.path("../output/study2/c_value/"))
  }
  if(dir.exists(file.path("../output/study2/c_value/FAMD/")) == FALSE) {
    dir.create(file.path("../output/study2/c_value/FAMD/"))
  }
  missing_per = missing_prop * 100
  if(study == 1){
  write.table(output, file = paste0("../output/study1/c_value/FAMD/", missing_pattern, "_", missing_per, "_FAMD_c_value.txt"), quote = FALSE, row.names = FALSE)
  } else if(study == 2){
  write.table(output, file = paste0("../output/study2/c_value/FAMD/", missing_pattern, "_", missing_per, "_FAMD_c_value.txt"), quote = FALSE, row.names = FALSE)
}}

# missForest imputation 
rf_impute <- function(data, seed) {
  forest_data <- data %>%
    mutate(ID = as.numeric(ID), region = as.numeric(region))
  forest_imp <- missForest(forest_data, variablewise = TRUE) 
  forest_df <- forest_imp$ximp
}

# MICE imputation
mice_impute <- function(data, seed) {
  MICE_data <- data
  colnames(MICE_data) <- paste0("x_", colnames(MICE_data))
  MICE_imp <- mice::mice(MICE_data, print = FALSE, nnet.MaxNWts = 2500, seed = seed)
  MICE_imputed <- mice::complete(MICE_imp)
  MICE_imputed <- MICE_imputed %>%
    rename_all(~ sub("x_", "", names(MICE_imputed)))
}

# kNN imputation
knn_impute <- function(data, kNN_k, seed) {
  set.seed(seed)
  kNN_imputed <- kNN(data, k = kNN_k, impNA = TRUE, imp_var = FALSE)
}

# Factorial analysis for mixed data (FAMD) imputatuon
famd_impute <- function(data, FAMD_c, seed) {
  FAMD_imputed <- imputeFAMD(data, ncp = FAMD_c, seed = seed)
  FAMD_imputed_df <- FAMD_imputed$completeObs %>%
    mutate(across(everything(), ~gsub("^[^_]*_", "", .))) %>%
    mutate(across(everything(), ~gsub("^[^_]*_", "", .))) 
}

# GLRM imputation
glrm_impute <- function(data_missing, data_validation = NULL, var_types, GLRM_k, seed) {
  if(!is.null(data_validation)) {
  validation_data <- as.h2o(data_validation)
  }
  GLRM_data <- as.h2o(data_missing)
  GLRM_model <- h2o.glrm(training_frame = GLRM_data, validation_frame = , cols = 1:ncol(GLRM_data), k = GLRM_k,
                         loss = "Absolute", transform = "None", regularization_x = "None", regularization_y = "None",
                         multi_loss = "Categorical", loss_by_col = var_types$loss_func, loss_by_col_idx = c(0:(ncol(GLRM_data)-1)),
                         recover_svd = TRUE, seed = 5, ignore_const_cols = FALSE, max_iterations = 1000)
  GLRM_imputed <- as.data.frame(h2o.predict(GLRM_model, GLRM_data)) 
  GLRM_imputed <- GLRM_imputed %>%
    rename_all(~ sub("reconstr_", "", names(GLRM_imputed)))
}

# Format each run for datawig imputation
datawig_format <- function(data) {
  # Convert to character
  data <- data %>%
    mutate(across(where(is.numeric), as.character)) %>%
    mutate(across(where(is.factor), as.character)) 
  if(any(grep("5174", colnames(data))) == TRUE) {
    data <- data %>%
    # Change 5174 The society to which the religious group belongs is best characterized as (please choose one): answers
    mutate(`5174` = case_when(`5174` == "1" ~ "band", 
                              `5174` == "2" ~ "tribe",
                              `5174` == "3" ~ "chiefdom",
                              `5174` == "4" ~ "state",
                              `5174` == "5" ~ "empire",
                              `5174` == "0" ~ "other"))
  }
  # Convert to string
  data[data == "2"] = "yesno"
  data[data == "1"] = "yes"
  data[data == "0"] = "no"
  data
}

# Run imputation methods (random forest, MICE, kNN, GLRM)
run_imputation <- function(data, study, missing_pattern, missing_prop, kNN_k, GLRM_k, FAMD_c, seed) {

  # Add missing data
  data_missing <- add_NA(data, study, missing_pattern = missing_pattern, missing_prop = missing_prop)
  
  # Create index of variable types for GLRM and conversion of data type
  var_types <- lapply(data_missing, variables_types)
  
  # Convert variables to correct class
  data_class <- Map(correct_class, data_missing, var_types)
  
  # Calculate missing percentage
  missing_per = missing_prop * 100

  # missForest imputation 
  forest_imputation <- possibly(rf_impute, otherwise = "The algorithm failed to converge")
  forest_imp <- lapply(data_class, function(x) forest_imputation(data = x, seed = seed))
  names(forest_imp) <- seq(1:length(forest_imp))
  
  # Save missForest output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1"))
  }
  if(dir.exists(file.path("../output/study1/random_forest/")) == FALSE) {
    dir.create(file.path("../output/study1/random_forest/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2"))
  }
  if(dir.exists(file.path("../output/study2/random_forest/")) == FALSE) {
    dir.create(file.path("../output/study2/random_forest/"))
  }
  if(study == 1){
  mapply(write_csv, forest_imp, paste0("../output/study1/random_forest/rf_", missing_pattern, "_", missing_per, "_", names(forest_imp), '.csv'))
  } else if(study == 2){
  mapply(write_csv, forest_imp, paste0("../output/study2/random_forest/rf_", missing_pattern, "_", missing_per, "_", names(forest_imp), '.csv'))
  }
  # kNN imputation
  knn_imputation <- possibly(knn_impute, otherwise = "The algorithm failed to converge")
  knn_imp <- lapply(data_class, function(x) knn_imputation(data = x, kNN_k = kNN_k, seed = seed))
  names(knn_imp) <- seq(1:length(knn_imp))
  
  # Save kNN output
  if(dir.exists(file.path("../output/study1/kNN/")) == FALSE) {
    dir.create(file.path("../output/study1/kNN/"))
  }
  if(dir.exists(file.path("../output/study2/kNN/")) == FALSE) {
    dir.create(file.path("../output/study2/kNN/"))
  }
  if(study == 1){
    mapply(write_csv, knn_imp, paste0("../output/study1/kNN/kNN_", missing_pattern, "_", missing_per, "_", names(knn_imp),".csv"))
  } else if(study == 2){
    mapply(write_csv, knn_imp, paste0("../output/study2/kNN/kNN_", missing_pattern, "_", missing_per, "_", names(knn_imp),".csv"))
  }
  
  # Replace branching questions with separate variables for each group of people
  data <- replace_branching(data)
  data_complete_class <- correct_class(data, var_types[[1]])
  
  # GLRM imputation
  glrm_imputation <- possibly(glrm_impute, otherwise = "The algorithm failed to converge")
  if(study == 1){
  glrm_imp <- lapply(data_class, function(x) glrm_imputation(data_missing = x, data_validation = data_complete_class, var_types = var_types[[1]], GLRM_k = GLRM_k, seed = seed))
  } else if(study == 2){
  glrm_imp <- lapply(data_class, function(x) glrm_imputation(data_missing = x, var_types = var_types[[1]], GLRM_k = GLRM_k, seed = seed))
  }
  names(glrm_imp) <- seq(1:length(glrm_imp))
  
  # Save GLRM output
  if(dir.exists(file.path("../output/study1/GLRM/")) == FALSE) {
    dir.create(file.path("../output/study1/GLRM/"))
  }
  if(dir.exists(file.path("../output/study2/GLRM/")) == FALSE) {
    dir.create(file.path("../output/study2/GLRM/"))
  }
  if(study == 1){
    mapply(write_csv, glrm_imp, paste0("../output/study1/GLRM/GLRM_", missing_pattern, "_", missing_per, "_", names(glrm_imp), '.csv'))
  } else if(study == 2){
    mapply(write_csv, glrm_imp, paste0("../output/study2/GLRM/GLRM_", missing_pattern, "_", missing_per, "_", names(glrm_imp), '.csv'))
  }
  
  # Format data for datawig imputation
  datawig <- lapply(data_class, function(x) datawig_format(data = x))
  names(datawig) <- seq(1:length(datawig))
  
  # Save output
  if(dir.exists(file.path("../output/study1/datawig_data/")) == FALSE) {
    dir.create(file.path("../output/study1/datawig_data/"))
  }
  if(dir.exists(file.path("../output/study2/datawig_data/")) == FALSE) {
    dir.create(file.path("../output/study2/datawig_data/"))
  }
  if(study == 1){
    mapply(write_csv, datawig, paste0("../output/study1/datawig_data/", missing_pattern, "_", missing_per, "_", names(datawig), '.csv'))
  } else if(study == 2){
    mapply(write_csv, datawig, paste0("../output/study2/datawig_data/", missing_pattern, "_", missing_per, "_", names(datawig), '.csv'))
  }
  
  # FAMD imputation
  famd_imputation <- possibly(famd_impute, otherwise = "The algorithm failed to converge")
  famd_imp <- lapply(data_class, function(x) famd_imputation(data = x, FAMD_c = FAMD_c, seed = seed))
  names(famd_imp) <- seq(1:length(famd_imp))
  
  # Extract run imputations
  famd_imp_run <- Filter(function(x) length(x) > 1, famd_imp)
  
  # Extract imputations that didn't run
  famd_imp_not_run <- Filter(function(x) length(x) == 1, famd_imp)
  
  # Save FAMD output
  if(dir.exists(file.path("../output/study1/FAMD/")) == FALSE) {
    dir.create(file.path("../output/study1/FAMD/"))
  }
  if(dir.exists(file.path("../output/study2/FAMD/")) == FALSE) {
    dir.create(file.path("../output/study2/FAMD/"))
  }
  if(length(famd_imp_run) > 0) {
    if(study == 1){
      mapply(write_csv, famd_imp_run, paste0("../output/study1/FAMD/FAMD_", missing_pattern, "_", missing_per, "_", names(famd_imp_run), '.csv'))
    } else if(study == 2){
      mapply(write_csv, famd_imp_run, paste0("../output/study2/FAMD/FAMD_", missing_pattern, "_", missing_per, "_", names(famd_imp_run), '.csv'))
    }} 
  if(length(famd_imp_not_run) > 0) {
    if(study == 1){
      mapply(write.table, famd_imp_not_run, paste0("../output/study1/FAMD/FAMD_", missing_pattern, "_", missing_per, "_", names(famd_imp_not_run), '.txt'), quote = FALSE, row.names = FALSE)
    } else if(study == 2){
      mapply(write.table, famd_imp_not_run, paste0("../output/study2/FAMD/FAMD_", missing_pattern, "_", missing_per, "_", names(famd_imp_not_run), '.txt'), quote = FALSE, row.names = FALSE)
    }}
  # MICE imputation
  mice_imputation <- possibly(mice_impute, otherwise = "The algorithm failed to converge")
  mice_imp <- lapply(data_class, function(x) mice_imputation(data = x, seed = seed))
  names(mice_imp) <- seq(1:length(mice_imp))
  
  # Save MICE output
  if(dir.exists(file.path("../output/study1/MICE/")) == FALSE) {
    dir.create(file.path("../output/study1/MICE/"))
  }
  if(dir.exists(file.path("../output/study2/MICE/")) == FALSE) {
    dir.create(file.path("../output/study2/MICE/"))
  }
  if(study == 1){
    mapply(write_csv, mice_imp, paste0("../output/study1/MICE/MICE_", missing_pattern, "_", missing_per, "_", names(mice_imp), '.csv'))
  } else if(study == 2){
    mapply(write_csv, mice_imp, paste0("../output/study2/MICE/MICE_", missing_pattern, "_", missing_per, "_", names(mice_imp), '.csv'))
  }
}

# Load data per condition
load_data <- function(study, method, condition) {
  if(study == 1) {
  observed_values <- read_csv("../output/study1/complete_cases/complete_cases.csv")
  NA_files <- list.files(path = "../output/study1/additional_NA", pattern = "*.csv", full.names = T)
  NA_file_names <- gsub(".csv", "", list.files(path = "../output/study1/additional_NA", pattern = "*.csv"))
  impute_files <- list.files(path = paste0("../output/study1/", method), pattern = "*.csv", full.names = T)
  impute_file_names <- gsub(".csv", "", list.files(path = paste0("../output/study1/", method), pattern = "*.csv"))
  csv_files <- c(NA_files, impute_files)
  csv_files_names <- c(NA_file_names, impute_file_names)
  csv_files <- csv_files[grepl(condition, csv_files)]
  csv_files_list <- lapply(csv_files, read_csv)
  csv_files_names <- csv_files_names[grepl(condition, csv_files_names)]
  names(csv_files_list) <- csv_files_names
  observed_values <- list(observed_values)
  names(observed_values) <- "observed_values"
  csv_files_list <- c(observed_values, csv_files_list)
  } else if(study == 2) {
    observed_values <- list.files(path = "../output/study2/filtered", pattern = "*.csv", full.names = T)
    observed_value_names <- paste0(gsub(".csv", "", list.files(path = "../output/study2/filtered", pattern = "*.csv")), "_observed_values")
    NA_files <- list.files(path = "../output/study2/additional_NA", pattern = "*.csv", full.names = T)
    NA_file_names <- gsub(".csv", "", list.files(path = "../output/study2/additional_NA", pattern = "*.csv"))
    impute_files <- list.files(path = paste0("../output/study2/", method), pattern = "*.csv", full.names = T)
    impute_file_names <- gsub(".csv", "", list.files(path = paste0("../output/study2/", method), pattern = "*.csv"))
    csv_files <- c(observed_values, NA_files, impute_files)
    csv_files_names <- c(observed_value_names, NA_file_names, impute_file_names)
    csv_files <- csv_files[grepl(condition, csv_files)]
    csv_files_list <- lapply(csv_files, read_csv)
    csv_files_names <- csv_files_names[grepl(condition, csv_files_names)]
    names(csv_files_list) <- csv_files_names
  }
  csv_files_list
}

# Calculate overall NRMSE per condition
overall_nrmse_method <- function(study, method, condition){
  
  # List data for conditions
  file_list <- load_data(study = study, method = method, condition = condition)
  
  # Extract files
  if(study == 1){
    observed_values <- file_list$observed_values
  } else if (study == 2) {
    observed_values_name <-  names(file_list)[grepl("observed", names(file_list))]
    observed_values <- file_list[[observed_values_name]]
  }
  NA_file_names <- names(file_list)[grepl("NA", names(file_list))]
  impute_files_names <- names(file_list)[!grepl("observed", names(file_list)) & !grepl("NA", names(file_list))]
  NA_files <- file_list[NA_file_names]
  impute_files <- file_list[impute_files_names]
  
  # Extract number of runs
  runs <- gsub('.*_\\s*','', names(file_list))
  runs <- gsub("[^0-9.-]", "", runs)
  runs <- as.numeric(runs)
  runs <- max(runs, na.rm = TRUE)
  
  # Index all cells with non-added missing data
  if(study == 2){
    org_missing_idx <- as.data.frame(which(is.na(observed_values), arr.ind=TRUE)) 
  } 
  
  # Extract index of missing values
  missing_idx <- list()
  for (i in 1:runs) {
    missing_idx[[i]] <- as.data.frame(which(is.na(NA_files[[i]]), arr.ind=TRUE)) 
  }
  # Index only additional missing data
  if(study == 2){
    for (i in 1:runs) {
      missing_idx[[i]] <- anti_join(missing_idx[[i]], org_missing_idx)
    }
  }
  
  # Extract the original values of all data points replaced with NA
  observed_values <- replace_branching(observed_values)
  observed_list <- list()
  for(i in 1:length(missing_idx)) {
    idx <- missing_idx[[i]]
    data_list <- list()
    for(j in 1:nrow(idx)) {
      data_list[[j]] <- observed_values[idx$row[j], idx$col[j]]
    }
    observed_list[[i]] <- as.data.frame(unlist(data_list))
    colnames(observed_list[[i]])  <- "observed"
  }
  
  # Extract the predicted values of all data points replaced with NA
  predict_list <- list()
  for(i in 1:length(missing_idx)) {
    idx <- missing_idx[[i]]
    predicted <- impute_files[[i]]
    # Convert datawig format for comparison
    if(method == "datawig"){
      # Convert string to number
      predicted[predicted == "yesno"] = "2"
      predicted[predicted == "yes"] = "1"
      predicted[predicted == "no"] = "0"
      predicted[predicted == "band"] = "1"
      predicted[predicted == "tribe"] = "2"
      predicted[predicted == "chiefdom"] = "3"
      predicted[predicted == "state"] = "4"
      predicted[predicted == "empire"] = "5"
    }
    data_list <- list()
    for(j in 1:nrow(idx)) {
        data_list[[j]] <- predicted[idx$row[j], idx$col[j]]
    }
    predict_list[[i]] <- as.data.frame(unlist(data_list))
    colnames(predict_list[[i]])  <- "predicted"
  }
  
  # Bind predicted and observed values
  predict_comp <- list()
  for (i in 1:runs) {
    predict_comp[[i]] <- cbind(observed_list[[i]], predict_list[[i]])
    # Convert to numeric
    if(method == "datawig"){
      predict_comp[[i]] <- predict_comp[[i]] %>%
        mutate(predicted = as.numeric(predicted))
    }
  }
  # Calculate RMSE
  predict_rmse <- lapply(predict_comp, function(x) rmse(x$observed, x$predicted))
  # Calculate NRMSE using the difference between maximum and minimum
  numeric_data <- observed_values %>% select(-ID, -elite, -non_elite, -religious_specialist, -start_date, -end_date, -region) %>% mutate(across(everything(), as.character)) %>% mutate(across(everything(), as.numeric))
  predict_nrmse <- lapply(predict_rmse, function(x) x/(max(numeric_data, na.rm = T) - min(numeric_data, na.rm = T)))
    # Calculate percent correct classification (PCC)
  predict_pcc <- lapply(predict_comp, function(x) sum(x$observed == x$predicted)/nrow(x))
  # Create output 
  output <- list()
  for(i in 1:runs){
    output[[i]] <- data.frame(Condition = condition, Method = method, Run = i, NRMSE = predict_nrmse[[i]], PCC = predict_pcc[[i]])
  }
  output <- bind_rows(output)
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/overall_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study1/overall_nrmse/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/overall_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study2/overall_nrmse/"))
  }
  if(study == 1) {
    write_csv(output, paste0("../output/study1/overall_nrmse/", method, "_", condition, ".csv"))
  } else if(study == 2) {
    write_csv(output, paste0("../output/study2/overall_nrmse/", method, "_", condition, ".csv"))
  }
  # Calculate average NRMSE
  average <- output %>%
    select(-Run) %>%
    group_by(Condition, Method) %>%
    summarize(NRMSE = mean(NRMSE, na.rm = TRUE), PCC = mean(PCC, na.rm = TRUE), .groups = "keep")
}

# Combine overall NRMSE for each condition
overall_nrmse <- function(study, condition){
  random_forest <- overall_nrmse_method(study = study, method = "random_forest", condition = condition)
  MICE <- overall_nrmse_method(study = study, method = "MICE", condition = condition)
  kNN <- overall_nrmse_method(study = study, method = "kNN", condition = condition)
  FAMD <- overall_nrmse_method(study = study, method = "FAMD", condition = condition)
  GLRM <- overall_nrmse_method(study = study, method = "GLRM", condition = condition)
  datawig <- overall_nrmse_method(study = study, method = "datawig", condition = condition)
  output <- rbind(random_forest, MICE, kNN, FAMD, GLRM, datawig)
  
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/avg_overall_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study1/avg_overall_nrmse/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/avg_overall_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study2/avg_overall_nrmse/"))
  }
  if(study == 1){
    write_csv(output, paste0("../output/study1/avg_overall_nrmse/", condition, ".csv"))
  } else if(study == 2) {
    write_csv(output, paste0("../output/study2/avg_overall_nrmse/", condition, ".csv"))
  }
}

# Calculate NRMSE per variable per method
variable_nrmse_method <- function(study, method, condition){
  
  # List data for conditions
  file_list <- load_data(study = study, method = method, condition = condition)
  
  # Extract files
  if(study == 1){
    observed_values <- file_list$observed_values
  } else if (study == 2) {
    observed_values_name <-  names(file_list)[grepl("observed", names(file_list))]
    observed_values <- file_list[[observed_values_name]]
  }
  NA_file_names <- names(file_list)[grepl("NA", names(file_list))]
  impute_files_names <- names(file_list)[!grepl("observed", names(file_list)) & !grepl("NA", names(file_list))]
  NA_files <- file_list[NA_file_names]
  impute_files <- file_list[impute_files_names]
  
  # Extract number of runs
  runs <- gsub('.*_\\s*','', names(file_list))
  runs <- gsub("[^0-9.-]", "", runs)
  runs <- as.numeric(runs)
  runs <- max(runs, na.rm = TRUE)
  
  # Index all cells with non-added missing data
  if(study == 2){
    org_missing_idx <- as.data.frame(which(is.na(observed_values), arr.ind=TRUE)) 
  } 
  
  # Extract index of missing values
  missing_idx <- list()
  for (i in 1:runs) {
    missing_idx[[i]] <- as.data.frame(which(is.na(NA_files[[i]]), arr.ind=TRUE)) 
  }
  
  # Extract index of missing values
  missing_idx <- list()
  for (i in 1:runs) {
    missing <- as.data.frame(which(is.na(NA_files[[i]]), arr.ind=TRUE)) 
    if(study == 2){
      missing <- anti_join(missing, org_missing_idx)
    }
    # Index all columns
    column_idx <- data.frame(var = colnames(NA_files[[i]]), col = seq(1:ncol(NA_files[[i]])))
    # Add variable type
    missing_idx[[i]] <- missing %>% left_join(column_idx)
  }
  
  # Extract the original values of all data points replaced with NA
  observed_values <- replace_branching(observed_values)
  observed_list <- list()
  for(i in 1:length(missing_idx)) {
    idx <- missing_idx[[i]]
    data_list <- list()
    for(j in 1:nrow(idx)) {
      data_list[[j]] <- observed_values[idx$row[j], idx$col[j]]
    }
    observed_list[[i]] <- as.data.frame(unlist(data_list))
    colnames(observed_list[[i]])  <- "observed"
  }
  
  # Extract the predicted values of all data points replaced with NA
  predict_list <- list()
  for(i in 1:length(missing_idx)) {
    idx <- missing_idx[[i]]
    predicted <- impute_files[[i]]
    # Convert datawig format for comparison
    if(method == "datawig"){
      # Convert string to number
      predicted[predicted == "yesno"] = "2"
      predicted[predicted == "yes"] = "1"
      predicted[predicted == "no"] = "0"
      predicted[predicted == "band"] = "1"
      predicted[predicted == "tribe"] = "2"
      predicted[predicted == "chiefdom"] = "3"
      predicted[predicted == "state"] = "4"
      predicted[predicted == "empire"] = "5"
    }
    data_list <- list()
    for(j in 1:nrow(idx)) {
      data_list[[j]] <- predicted[idx$row[j], idx$col[j]]
    }
    predict_list[[i]] <- as.data.frame(unlist(data_list))
    colnames(predict_list[[i]])  <- "predicted"
  }
  
  # Bind predicted and observed values
  predict_comp <- list()
  for (i in 1:runs) {
    predict_comp[[i]] <- cbind(missing_idx[[i]], observed_list[[i]], predict_list[[i]])
    # Convert to numeric
    if(method == "datawig"){
      predict_comp[[i]] <- predict_comp[[i]] %>%
        mutate(predicted = as.numeric(predicted))
    }
  }
  # Calculate RMSE
  predict_rmse <- lapply(predict_comp, function(x) x %>%
                           group_by(var) %>%
                           summarize(rmse = rmse(observed, predicted)))
  # Calculate NRMSE using the difference between maximum and minimum
  numeric_data <- observed_values %>% 
    select(-ID, -elite, -non_elite, -religious_specialist, -start_date, -end_date, -region) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(across(everything(), as.numeric))
  # Find min and max per variable
  min_max <- as.data.frame(apply(numeric_data, 2, range, na.rm = TRUE))
  min_max <- as.data.frame(t(min_max)) 
  min_max <- min_max %>%
    mutate(var = row.names(min_max)) %>%
    rename(min = V1, max = V2) %>%
    mutate(var = gsub("X", "", var)) 
  # Calculate NRMSE
  predict_nrmse <- list()
  for(i in 1:length(predict_rmse)){
    predict_nrmse[[i]] <- predict_rmse[[i]] %>%
      mutate(Condition = rep(condition), Method = rep(method), Run = rep(i)) %>%
      select(Condition, Method, Run, everything()) %>%
      left_join(min_max, by = "var") %>%
      mutate(nrmse = rmse/(max - min)) %>% 
      select(-rmse, -max, -min) %>%
    pivot_wider(names_from = var, values_from = nrmse)
  }
  predict_nrmse <- bind_rows(predict_nrmse)
  
  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/variable_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study1/variable_nrmse/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/variable_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study2/variable_nrmse/"))
  }
  if(study == 1){
    write_csv(predict_nrmse, paste0("../output/study1/variable_nrmse/", method, "_", condition, ".csv"))
  } else if(study == 2) {
    write_csv(predict_nrmse, paste0("../output/study2/variable_nrmse/", method, "_", condition, ".csv"))
  }
  # Calculate average NRMSE
  output <- predict_nrmse %>%
    select(-Run) %>%
    summarize(across(-Condition:-Method, mean, na.rm = TRUE)) %>%
    mutate(Condition = condition, Method = method) %>%
    select(Condition, Method, everything())
}

# Combine NRMSE per variable for each condition
variable_nrmse <- function(study, condition){
  random_forest <- variable_nrmse_method(study = study, method = "random_forest", condition = condition)
  MICE <- variable_nrmse_method(study = study, method = "MICE", condition = condition)
  kNN <- variable_nrmse_method(study = study, method = "kNN", condition = condition)
  FAMD <- variable_nrmse_method(study = study, method = "FAMD", condition = condition)
  GLRM <- variable_nrmse_method(study = study, method = "GLRM", condition = condition)
  datawig <- variable_nrmse_method(study = study, method = "datawig", condition = condition)
  output <- rbind(random_forest, MICE, kNN, FAMD, GLRM, datawig)

  # Save output
  if(dir.exists(file.path("../output/study1/")) == FALSE) {
    dir.create(file.path("../output/study1/"))
  }
  if(dir.exists(file.path("../output/study1/avg_variable_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study1/avg_variable_nrmse/"))
  }
  if(dir.exists(file.path("../output/study2/")) == FALSE) {
    dir.create(file.path("../output/study2/"))
  }
  if(dir.exists(file.path("../output/study2/avg_variable_nrmse/")) == FALSE) {
    dir.create(file.path("../output/study2/avg_variable_nrmse/"))
  }
  if(study == 1) {
    write_csv(output, paste0("../output/study1/avg_variable_nrmse/", condition, ".csv"))
  } else if(study == 2){
    write_csv(output, paste0("../output/study2/avg_variable_nrmse/", condition, ".csv"))
  }
}
