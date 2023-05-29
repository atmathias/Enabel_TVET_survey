
# checks for data collection
library(supporteR)
library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel(path = "inputs/Enabel_tool.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.location = individual_residence_district,
         # check presence of hh_id or individual_id
         i.check.hh_id = hh_id,
         start = as_datetime(start),
         end = as_datetime(end), 
         individual_age = as.numeric(individual_age),
         num_children_school_aged = as.numeric(num_children_school_aged))  

# check the naming of sample data
df_sample_data <- read_csv("inputs/xxxx.csv") %>% 
  janitor::clean_names() %>% 
  rename(unique_hhid_number = id)


# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_uuid")

# checks on hhids ----------------------------------------------------------

sample_hhid_nos <- df_sample_data %>% 
  pull(unique_hhid_number) %>% 
  unique()

# data_hh_nos <- df_grop_hh_no_data %>% 
#   pull(unique_group_hh_no) %>% 
#   unique()

# duplicate point numbers
df_c_duplicate_hhid_nos <- check_duplicate_hhid_numbers(input_tool_data = df_tool_data,
                                                        input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_hhid_nos")

# pt id does not exist in sample
df_c_hhid_not_in_sample <- check_hhid_number_not_in_samples(input_tool_data = df_tool_data, 
                                                            input_sample_hhid_nos_list = sample_hhid_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_hhid_not_in_sample")


# data not meeting minimum requirements -----------------------------------

# no_consent
df_no_consent <- df_tool_data %>% 
  filter(consent == "no_consent") %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "consent",
         i.check.current_value = as.character(consent),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_no_consent",
         i.check.issue = "no_consent",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent")



# testing_data
df_testing_data <- df_tool_data %>%
  filter(i.check.start_date < as_date("2023-06-01") | str_detect(string = hh_id, pattern = fixed('test', ignore_case = TRUE))) %>% 
    mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_m_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_testing_data")

#
# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")

# check the time between surveys
# min_time_btn_surveys <- 5
# 
# df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
#                                                         input_min_time = min_time_btn_surveys)
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_time_btn_survey")


# others checks -----------------------------------------------------------

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)
add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")


# Logical checks












