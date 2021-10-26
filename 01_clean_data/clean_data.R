# Load packages
library(tidyverse)
library(data.table)
library(splitstackshape)
library(testthat)

# Load data
related_questions <- fread("../data/question_dictionary.csv")
v6_questions <- fread("../data/drh_v6_poll.csv") %>% rename(quest = Question, quest_desc = `Question description`)
data <- fread("../data/drh.csv") 

# Convert date ranges to numeric & Question ID to integer for joining
data <- data[, start_date := str_extract(`Date range`, "[^-]+")][
  , start_year := as.numeric(gsub("([0-9]+).*$", "\\1", start_date))][
  , start_year := if_else(grepl("B", start_date), -start_year, start_year)][
  , end_date := sub(".*-", "", `Date range`)][
  , end_year := as.numeric(gsub("([0-9]+).*$", "\\1", end_date))][
  , end_year := if_else(grepl("B", end_date), -end_year, end_year)][
  , c("start_date", "end_date") := NULL][
  , `Question ID` := as.integer(`Question ID`)]

# Convert questions from v5 poll into equivalent v6 poll questions
data_stand <- related_questions[data, on = "Question ID"][
  , `Question ID` := if_else(Poll == "Religious Group (v5)" & !is.na(`Related question ID`), `Related question ID`, `Question ID`)]
data_stand <- v6_questions[data_stand, on = "Question ID"][
  , Question := quest][
  , `Parent question` := `Parent Question`][
  , c("Poll", "quest", "quest_desc", "Parent Question", "Related question ID") := NULL]
data_stand <- unique(data_stand)

# One-hot encode nominal data which allows users to select all from list of answers

# 4765 Where is iconography present [select all that apply]:
# Correct entry 997 where answers are split into separate rows
icon <- data_stand[`Question ID` == "4765"][
  , Answers := ifelse(`Entry ID` == "997", "On persons; Some public spaces", Answers)][
  , `4765_1` := ifelse(grepl("On persons", Answers), "1",
                ifelse(grepl("Field doesn't know", Answers), "-1",
                ifelse(grepl("I don't know", Answers), "-2", "0")))][
  , `4765_2` := ifelse(grepl("At home", Answers), "1",
                ifelse(grepl("Field doesn't know", Answers), "-1",
                ifelse(grepl("I don't know", Answers), "-2", "0")))][
  , `4765_3` := ifelse(grepl("Only religious public space", Answers), "1",
                ifelse(grepl("Field doesn't know", Answers), "-1",
                ifelse(grepl("I don't know", Answers), "-2", "0")))][
  , `4765_4` := ifelse(grepl("Some public spaces", Answers), "1",
                ifelse(grepl("Field doesn't know", Answers), "-1",
                ifelse(grepl("I don't know", Answers), "-2", "0")))][
  , `4765_5` := ifelse(grepl("All public spaces", Answers), "1",
                ifelse(grepl("Field doesn't know", Answers), "-1",
                ifelse(grepl("I don't know", Answers), "-2", "0")))][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
icon <- melt(icon, measure.vars = c("4765_1", "4765_2", "4765_3", "4765_4", "4765_5"),
              variable.name = "Question ID", value.name = "Answer values")
icon <- unique(icon)
icon <- setcolorder(icon, colnames(data_stand))

# 5087 Moral norms apply to
# Correct entry 843 where answers are split into separate rows
moral <- data_stand[`Question ID` == "5087"][
  , `Answer values` := ifelse(`Entry ID` == "843", "1; 5", `Answer values`)][
  , `5087_1` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("1", `Answer values`), "1",
                ifelse(grepl("-2", `Answer values`), "-2","0")))][
  , `5087_2` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("2", `Answer values`), "1","0")))][
  , `5087_3` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("3", `Answer values`), "1","0")))][
  , `5087_4` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("4", `Answer values`), "1","0")))][
  , `5087_5` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("5", `Answer values`), "1","0")))][
  , `5087_6` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("6", `Answer values`), "1","0")))][
  , `5087_7` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("7", `Answer values`), "1","0")))][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
moral <- melt(moral, measure.vars = c("5087_1", "5087_2", "5087_3", "5087_4", "5087_5", "5087_6", "5087_7"),
              variable.name = "Question ID", value.name = "Answer values")
moral <- unique(moral)
moral <- setcolorder(moral, colnames(data_stand))

# 5227 Does the religious group in question provide food for themselves Please characterize the forms/level of food production [choose all that apply]:
# Answers of Other [specify in comments] are excluded
# Correct entry 977 where answers are split into separate rows
self_food <- data_stand[`Question ID` == "5227"][
  , `Answer values` := ifelse(`Entry ID` == "977", "7; 0", `Answer values`)][
  , `5227_1` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("1", `Answer values`), "1",
                ifelse(grepl("-2", `Answer values`), "-2","0")))][
  , `5227_2` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("2", `Answer values`), "1","0")))][
  , `5227_3` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("3", `Answer values`), "1","0")))][
  , `5227_4` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("4", `Answer values`), "1","0")))][
  , `5227_5` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("5", `Answer values`), "1","0")))][
  , `5227_6` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("6", `Answer values`), "1","0")))][
  , `5227_7` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("7", `Answer values`), "1","0")))][
                  , c("Question ID","Answer values"):=NULL][
                    , c("Answers","Note") := ""]
self_food <- melt(self_food, measure.vars = c("5227_1", "5227_2", "5227_3", "5227_4", "5227_5", "5227_6", "5227_7"),
              variable.name = "Question ID", value.name = "Answer values")
self_food <- unique(self_food)
self_food <- setcolorder(self_food, colnames(data_stand))

# 5229 Is food provided to the group’s adherents by an institution(s) other than the religious group in question Please characterize the forms/level of food production [choose all that apply]:
# Answers of Other [specify in comments] are excluded
other_food <- data_stand[`Question ID` == "5229"][
  , `5229_1` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("1", `Answer values`), "1",
                ifelse(grepl("-2", `Answer values`), "-2","0")))][
  , `5229_2` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("2", `Answer values`), "1","0")))][
  , `5229_3` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("3", `Answer values`), "1","0")))][
  , `5229_4` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("4", `Answer values`), "1","0")))][
  , `5229_5` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("5", `Answer values`), "1","0")))][
  , `5229_6` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("6", `Answer values`), "1","0")))][
  , `5229_7` := ifelse(grepl("-1", `Answer values`), "-1",
                ifelse(grepl("-2", `Answer values`), "-2",
                ifelse(grepl("7", `Answer values`), "1","0")))][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
other_food <- melt(other_food, measure.vars = c("5229_1", "5229_2", "5229_3", "5229_4", "5229_5", "5229_6", "5229_7"),
                  variable.name = "Question ID", value.name = "Answer values")
other_food <- unique(other_food)
other_food <- setcolorder(other_food, colnames(data_stand))

# Remove qualitative data (references) 
data_non_qual <- data_stand[`Data Type` != "Nominal - Multiple" & `Data Type` != "Qualitative"]
data_non_qual <- rbind(data_non_qual, icon, moral, self_food, other_food)

# Remove questions with [specify] answers
data_non_specify <- data_non_qual[!grepl("\\[specify\\]", Question)] 

# Remove Other [specify in comments] answers
data_non_specify <- data_non_specify[!grepl("\\[specify in comments\\]", Answers)]

# Split data by branching question answers
data_branch <- data_non_specify[
  , `Branching question` := gsub("\\(common people, general populace\\)", "", `Branching question`)][
  , `Branching question` := gsub("Status of Participants:", "", `Branching question`)][
  , `Branching question` := gsub("Non-elite", "N", `Branching question`)][
  , `Branching question` := gsub("Elite", "E", `Branching question`)][
  , `Branching question` := gsub("Religious Specialists", "R", `Branching question`)][
  , `Branching question` := strsplit(`Branching question`, ",")][
  , list(`Branching question` = as.character(unlist(`Branching question`))), by = setdiff(names(data_non_qual), "Branching question")]

# Split multiple answer values into multiple rows
# Paul the Apostle (355ER) The supreme high god can see you everywhere (in the dark, at home): - Answer values 1;-1 (Yes; Field doesn't know)
# Qumran Movement (176R) Originated from divine or semi-divine human beings: - Answer values 1; 0 (Yes; No)
# Qumran Movement (176R) Originated from non-divine human being: - Answer values 1; 0 (Yes; No)
data_multi_ans_row <- data_branch[
  , `Answer values` := gsub("1; -1", "1,, -1", `Answer values`)][
  , `Answer values` := gsub("1; 0", "1,, 0", `Answer values`)]
data_multi_ans_row_cor <- cSplit(data_multi_ans_row, "Answer values", ",,", "long")
data_multi_ans_row_cor <- data_multi_ans_row_cor[
  , `Answer values` := gsub('c\\(\\"1\\"', "1", `Answer values`)][
  , `Answer values` := gsub('\\" -1\\"\\)', "-1", `Answer values`)][
  , `Answer values` := gsub('\\" 0\\"\\)', "0", `Answer values`)]
data_multi_ans_row_cor <- unique(data_multi_ans_row_cor)  

# Replace answer values for specific questions
data_ans_value <- as_tibble(data_multi_ans_row_cor) %>%
  # Number of adherents of religious group within sample region (estimated population, numerical):	
  mutate(`Answer values` = ifelse(grepl("Estimated population, numeric", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Estimated population, numerical: ", "", `Answer values`)) %>%
  mutate(`Answer values` = gsub("Estimated population, numeric: ", "", `Answer values`)) %>%
  # Average interval [hours]: 
  mutate(`Answer values` = ifelse(grepl("Average interval", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Average interval \\[hours\\]: ", "", `Answer values`)) %>%
  # Estimated population, percentage of sample region:
  mutate(`Answer values` = ifelse(grepl("percentage of sample region", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Estimated population, percentage of sample region: ", "", `Answer values`)) %>%
  # Height, meters:
  mutate(`Answer values` = ifelse(grepl("Height, meters:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Height, meters: ", "", `Answer values`)) %>%
  # Height, square meters:
  mutate(`Answer values` = ifelse(grepl("Height, square meters:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Height, square meters: ", "", `Answer values`)) %>%
  # Hours: 
  mutate(`Answer values` = ifelse(grepl("Hours:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Hours: ", "", `Answer values`)) %>%
  # Number of participants:
  mutate(`Answer values` = ifelse(grepl("Number of participants:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Number of participants: ", "", `Answer values`)) %>%
  # Number of levels [numeric value]:
  mutate(`Answer values` = ifelse(grepl("Number of levels", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Number of levels \\[numeric value\\]: ", "", `Answer values`)) %>%
  # Percentage of area:
  mutate(`Answer values` = ifelse(grepl("Percentage of area:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Percentage of area: ", "", `Answer values`)) %>%
  # Percentage:
  mutate(`Answer values` = ifelse(grepl("Percentage:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Percentage: ", "", `Answer values`)) %>%
  # Square meters: 
  mutate(`Answer values` = ifelse(grepl("Square meters:", Answers), Answers, `Answer values`)) %>%
  mutate(`Answer values` = gsub("Square meters: ", "", `Answer values`)) 

# If an entry has multiple overlapping time periods but a single answer, combine these into 1 time period
data_comb_date <- data_ans_value %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, `Question ID`, `Answer values`) %>%
  mutate(other_start_year = lag(start_year), other_end_year = lag(end_year)) %>%
  mutate(other_start_year = ifelse(is.na(other_start_year), lead(start_year), NA), other_end_year = ifelse(is.na(other_end_year), lead(end_year), NA)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year & other_end_year >= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year & other_start_year <= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, Question, `Question ID`, `Answer values`, Answers, `Parent question`, start_year, end_year) %>%
  distinct() %>%
  mutate(other_start_year_lead = lead(start_year), other_end_year_lead = lead(end_year)) %>%
  mutate(other_start_year_lag = lag(start_year), other_end_year_lag = lag(end_year)) %>%
  mutate(other_start_year = other_start_year_lead, other_end_year = other_end_year_lead) %>%
  mutate(other_start_year = ifelse(is.na(other_start_year), other_start_year_lag, other_start_year), other_end_year = ifelse(is.na(other_end_year), other_end_year_lag, other_end_year)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  distinct() 

# If there is an overlap of a single year between answers, change start year to one year later
data_ans_ov_rm <- data_comb_date %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, `Question ID`) %>%
  mutate(new_start_year = ifelse(start_year == lead(end_year) | start_year == lag(end_year), start_year + 1, NA)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year)) %>%
  select(-new_start_year, -other_start_year_lead, -other_end_year_lead, -other_start_year_lag, -other_end_year_lag, -other_start_year, -other_end_year, -new_end_year) %>%
  distinct()

# Split data into individual years
data_all_years <- as.data.table(data_ans_ov_rm)[,list(Year = start_year:end_year),'`Entry ID`,`Branching question`,`Region ID`,Question,`Question ID`,`Answer values`,Answers,`Parent question`']

# Recombine data with a single answer per entry/time point
data_single_ans_ID <- data_all_years[, .N, by=.(`Entry ID`, `Branching question`, `Region ID`, Question, `Question ID`, `Parent question`, Year)][ N == 1 ]
data_single_ans <- data_all_years[data_single_ans_ID, on = c(`Entry ID`="`Entry ID`", `Branching question`="`Branching question`", `Region ID`="`Region ID`", `Question ID`="`Question ID`", Question="Question", `Parent question`="`Parent question`", Year="Year")][
    , c("N", "Answers") := NULL]

expect_equal(nrow(data_single_ans_ID), nrow(data_single_ans))

data_single_ans <- as_tibble(data_single_ans) %>%
  group_by(across(c(-Year))) %>%
  summarize("Start Date"=min(Year),
            "End Date"=max(Year), .groups = "keep") %>%
  ungroup() %>%
  select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, everything()) %>%
  distinct()

# Recombine data with multiple answers per entry/time point
data_multi_ans_ID <- data_all_years[, .N, by=.(`Entry ID`, `Branching question`,`Region ID`, Question, `Question ID`, `Parent question`, Year)][ N >= 2 ]
data_multi_ans_time <- data_all_years[data_multi_ans_ID, on = c(`Entry ID`="`Entry ID`", `Branching question`="`Branching question`", `Region ID`="`Region ID`", `Question ID`="`Question ID`", Question="Question", `Parent question`="`Parent question`", Year="Year")][
  , "N" := NULL]

expect_equal(sum(data_multi_ans_ID$N), nrow(data_multi_ans_time))

data_multi_ans_time <- as_tibble(data_multi_ans_time) %>%
  group_by(across(c(-Year))) %>%
  summarize("Start Date"=min(Year),
            "End Date"=max(Year), .groups = "keep") %>%
  ungroup() %>%
  select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, everything()) %>%
  distinct()

# Handle questions with different date ranges but answers that overlap

# If there are multiple answers to the same question, use the non "Field doesn't know" answer
# If there are yes and no answers treat this as a separate category
data_multi_ans_join <- data_multi_ans_time %>%
  filter(Question != "What is the average interval of time between performances (in hours):" & Question != "Number of adherents of religious group within sample region (estimated population, numerical):" & Question != "Estimate how many levels there are in the hierarchy of religious leadership:" & Question != "Number of adherents of religious group within sample region (estimated population, numerical):" & Question != "Estimate how many levels there are in the hierarchy of religious leadership:" & Question != "Number of adherents of religious group within sample region (% of sample region population, numerical):" & Question != "On average, for large-scale rituals how many participants gather in one location:") %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, Question, `Question ID`, `Start Date`, `End Date`) %>%
  # Replace "Yes [specify] ..." with Yes
  mutate(Answers = if_else(grepl("Yes \\[specify\\]", Answers), "Yes", Answers)) %>%
  # Yes and Field doesn't know treated as Yes
  mutate(answer_fdk_yes = ifelse(Answers == "Field doesn't know" & lead(Answers) == "Yes" | Answers == "Field doesn't know" & lag(Answers) == "Yes" | Answers == "Yes; Field doesn't know", "1", NA)) %>%
  # No and Field doesn't know treated as No
  mutate(answer_fdk_no = ifelse(Answers == "Field doesn't know" & lead(Answers) == "No" | Answers == "Field doesn't know" & lag(Answers) == "No", "0", NA)) %>%
  # Yes and I don't know treated as Yes 
  mutate(answer_idk_yes = ifelse(Answers == "I don't know" & lead(Answers) == "Yes" | Answers == "I don't know" & lag(Answers) == "Yes", "1", NA)) %>%
  # No and I don't know treated as No 
  mutate(answer_idk_no = ifelse(Answers == "I don't know" & lead(Answers) == "No" | Answers == "I don't know" & lag(Answers) == "No", "0", NA)) %>%
  # Yes and No treated as Yes or No 
  mutate(answer_yes_no = ifelse(Answers == "Yes" & lead(Answers) == "No" | Answers == "No" & lead(Answers) == "Yes" | Answers == "Yes" & lag(Answers) == "No" | Answers == "No" & lag(Answers) == "Yes" | Answers == "Yes; No", "2", NA)) %>%
  mutate(`Answer values` = ifelse(!is.na(answer_fdk_yes), answer_fdk_yes, 
                           ifelse(!is.na(answer_fdk_no), answer_fdk_no,
                           ifelse(!is.na(answer_idk_yes), answer_idk_yes, 
                           ifelse(!is.na(answer_idk_no), answer_idk_no,
                           ifelse(!is.na(answer_yes_no), answer_yes_no,`Answer values`)))))) 
# For non-yes/no categorical questions that require a single answer, but multiple, sample randomly from possible answers
set.seed(1)
data_multi_ans_cor <- data_multi_ans_join  %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  select(-answer_fdk_yes, -answer_fdk_no, -answer_idk_yes, -answer_idk_no, -answer_yes_no, -Answers) %>%
  distinct() %>%
  # Remove any row with an NA answer
  filter(!is.na(`Answer values`))

# For continuous values with ranges replace with the mean value of the range
data_cont_multi_ans <- data_multi_ans_time %>%
  ungroup() %>%
  filter(Question == "What is the average interval of time between performances (in hours):" | Question == "Number of adherents of religious group within sample region (% of sample region population, numerical):" ) %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, Question, `Question ID`, `Parent question`, `Start Date`, `End Date`) %>%
  summarise(`Answer values` = as.character(mean(as.numeric(`Answer values`))), .groups = "keep")
# For discrete values replace with the mean but round to the nearest integer
data_disc_multi_ans <- data_multi_ans_time %>%
  ungroup() %>%
  filter(Question == "Number of adherents of religious group within sample region (estimated population, numerical):" | Question == "Estimate how many levels there are in the hierarchy of religious leadership:" | Question == "On average, for large-scale rituals how many participants gather in one location:") %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, Question, `Question ID`, `Parent question`, `Start Date`, `End Date`) %>%
  summarise(`Answer values` = as.character(round(mean(as.numeric(`Answer values`))), 2), .groups = "keep")

# Recombine questions with single and multiple answers
data_comb <- bind_rows(data_single_ans, data_multi_ans_cor, data_cont_multi_ans, data_disc_multi_ans)

# Fill in missing questions
data_all_questions <- data_comb %>%
  complete(nesting(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`),
           nesting(Question, `Question ID`, `Parent question`),
           fill = list(value=0)) %>%
  filter(!is.na(`Question ID`)) %>%
  # remove duplicate rows
  distinct()

# Remove empty answers with date ranges that are covered by existing answers
data_dup_filt <- data_all_questions %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, `Question ID`) %>%
  rename(start_year = `Start Date`, end_year = `End Date`) %>%
  mutate(other_start_year = ifelse(is.na(`Answer values`), lag(start_year), NA)) %>%
  mutate(other_end_year = ifelse(is.na(`Answer values`), lag(end_year), NA)) %>%
  mutate(other_start_year = ifelse(is.na(`Answer values`) & is.na(other_start_year), lead(start_year), NA)) %>%
  mutate(other_end_year = ifelse(is.na(`Answer values`) & is.na(other_end_year), lead(end_year), NA)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year & other_end_year >= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year & other_start_year <= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  select(-other_start_year, -other_end_year, -new_start_year, -new_end_year) %>%
  distinct() %>%
  mutate(other_start_year = ifelse(is.na(`Answer values`), lead(start_year), NA), other_end_year = ifelse(is.na(`Answer values`), lead(end_year), NA)) %>%
  mutate(other_start_year_lag = ifelse(is.na(`Answer values`), lag(start_year), NA),  other_end_year_lag = ifelse(is.na(`Answer values`), lag(end_year), NA)) %>%
  mutate(other_start_year = ifelse(is.na(`Answer values`) & is.na(other_start_year), other_start_year_lag, other_start_year), other_end_year = ifelse(is.na(`Answer values`) & is.na(other_end_year), other_end_year_lag, other_end_year)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year & other_end_year >= end_year, other_start_year, start_year)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year & other_end_year >= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year & other_start_year <= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  select(-other_start_year, -other_end_year, -other_start_year_lag, -other_end_year_lag, -new_start_year, -new_end_year) %>%
  distinct() %>%
  ungroup() 
# Find questions with more than one answer
multi_ans <- data_dup_filt %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, `Question ID`, start_year, end_year) %>%
  tally() %>%
  filter(n > 1) 
data_dup_filt_all <- data_dup_filt %>%
  left_join(multi_ans) %>%
  # Mark rows for filtering
  mutate(filter_row = ifelse(is.na(`Answer values`) & !is.na(n), 1, NA)) %>%
  filter(is.na(filter_row)) %>%
  select(-n, -filter_row) %>%
  rename(`Start Date` = start_year, `End Date` = end_year)

# If a question unanswered and not categorical, it should not be filled in with the answer to the parent question and should instead be removed
questions <- v6_questions %>% 
  mutate(`Question ID` = as.factor(`Question ID`))
unans_non_cat <- data_dup_filt_all %>%
  left_join(questions) %>%
  filter(is.na(`Answer values`) & `Data Type` != "Nominal") %>%
  select(-quest, -quest_desc, -`Data Type`, -`Parent Question`) 
data_unans_filt <- anti_join(data_dup_filt_all, unans_non_cat)

# Extract parent questions with negative/ field doesn't know answers 
parent_question <- unique(data_unans_filt$`Parent question`)

# If parent question has a negative/ field doesn't know/ I don't know answer replace the answer of the corresponding child question
data_parent_question <- data_unans_filt %>%
  filter(Question %in% parent_question) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, Question, `Answer values`) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`) %>%
  filter(panswervalue != 1 & panswervalue != 2 & !is.na(panswervalue)) %>%
  right_join(data_unans_filt) %>% 
  mutate(`Answer values` = ifelse(is.na(`Answer values`), panswervalue, `Answer values`)) %>%
  select(-panswervalue) %>%
  distinct()

expect_equal(nrow(data_parent_question), nrow(data_unans_filt))

# Remove questions without answers
data_non_na <- data_parent_question %>%
  filter(!is.na(`Answer values`)) 

# Split data into individual years

# Entry 592 causes errors as it has multiple time points with the same answer with a gap between them
# This is handled by splitting the dataset into two, with the different time periods in different datasets
data_592_11 <- data_non_na %>% filter(`Entry ID` == '592' & `Start Date` == "11")
data_non_na_sub <- data_non_na %>% filter(`Entry ID` == '592' & `Start Date` != "11" | `Entry ID` != '592')
data_sub_all_years_clean <- data.table(data_non_na_sub)[,list(Year = `Start Date`:`End Date`),'`Entry ID`,`Branching question`,`Region ID`,`Question ID`,`Answer values`']
data_592_all_years_clean <- data.table(data_592_11)[,list(Year = `Start Date`:`End Date`),'`Entry ID`,`Branching question`,`Region ID`,`Question ID`,`Answer values`']
data_all_years_clean <- rbind(data_sub_all_years_clean, data_592_all_years_clean)
data_all_years_clean <- unique(data_all_years_clean)

# Transpose question and answer data (splitting into 2 for faster computation)
entries <- unique(data_all_years_clean$`Entry ID`)
entries_length <- round(length(entries)/2)
data_split1 <- data_all_years_clean[`Entry ID` %in% entries[1:entries_length]]
data_split2 <- data_all_years_clean[`Entry ID` %in% entries[(entries_length+1):length(entries)]]
expect_equal((nrow(data_split1) + nrow(data_split2)),  nrow(data_all_years_clean))
data_t1 <- dcast(data_split1, `Entry ID` + `Branching question` + `Region ID` + Year ~ `Question ID`, value.var = "Answer values") 
data_t2 <- dcast(data_split2, `Entry ID` + `Branching question` + `Region ID` + Year ~ `Question ID`, value.var = "Answer values") 
data_t <- rbind(data_t1, data_t2, fill = TRUE)
data_t <- unique(data_t)

# Recombine data into time periods
data_time_period <- data_t[, by = setdiff(names(data_t), "Year"), 
                           .(`Start Date` = min(Year), `End Date` = max(Year))]

# Recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
data_group_comb <- as_tibble(data_time_period) %>%
  # Remove additional white space from branching questions
  mutate(`Branching question` = str_trim(`Branching question`)) %>%
  group_by(across(c(-`Branching question`))) %>%
  summarise(`Branching question` = paste(unique(`Branching question`), collapse=","), .groups = "keep") %>% 
  ungroup() %>%
  select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`, everything()) %>%
  arrange(`Start Date`) %>%
  arrange(`Entry ID`)

# If a question has multiple time periods with the same set of answers, these are combined into a single time period
# Split time periods
mark_overlap_time <- data_group_comb %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`) %>%
  mutate(overlap = ifelse(`End Date` > lead(`End Date`) & `Start Date` < lead(`End Date`), T, NA)) %>%
  select(overlap, everything())
# Overlapping ranges 
overlap_time <- rbind(mark_overlap_time, mark_overlap_time) %>% 
  filter(overlap == T) %>% 
  group_by(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`) %>%
  mutate(id = row_number()) %>%
  select(id, everything())
rm_overlap <- mark_overlap_time %>%
  filter(is.na(overlap)) %>%
  mutate(id = NA) %>%
  select(id, everything()) 
# Correct incorrect ranges
range_cor <- bind_rows(overlap_time, rm_overlap) %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`) %>%
  arrange(`Start Date`) %>%
  arrange(`Entry ID`) %>%
  mutate(new_end_date = ifelse(id == "2", lead(`Start Date`) - 1, NA)) %>%
  mutate(`End Date` = ifelse(!is.na(new_end_date), new_end_date, `End Date`)) %>%
  arrange(`End Date`) %>%
  arrange(`Entry ID`) %>%
  mutate(new_start_date = ifelse(id == "1", lag(`End Date`) + 1, NA)) %>%
  mutate(`Start Date` = ifelse(!is.na(new_start_date), new_start_date, `Start Date`)) %>%
  select(-new_start_date, -new_end_date, -id, -overlap) %>% 
  arrange(`Start Date`) %>%
  arrange(`Entry ID`)

# Save data
write_csv(range_cor, file = "../data/drh_t_unknown.csv")

# Split into ID and other variables
id_var <- range_cor %>% select(`Entry ID`, `Branching question`, `Region ID`, `Start Date`, `End Date`)
other_var <- range_cor %>% ungroup() %>% select(-`Entry ID`, -`Branching question`, -`Region ID`, -`Start Date`, -`End Date`)

# Replace Field doesn't know and I don't know with unknown (missing)
other_var[other_var == "-1"] = NA
other_var[other_var == "-2"] = NA

# Recombine variables
cor_data_t <- cbind(id_var, other_var)

# Save data
write_csv(cor_data_t, file = "../data/drh_t.csv")

rm(list = ls())
