
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


# no_consent_not_hoh
df_no_consent_not_hoh <- df_tool_data %>% 
  filter(hoh == "no") %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "hoh",
         i.check.current_value = as.character(hoh),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_no_consent_not_hoh",
         i.check.issue = "no_consent_not_hoh",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent_not_hoh")

# below_age
df_respondents_not_of_age <- df_tool_data %>% 
  filter(age < 18) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "age",
         i.check.current_value = as.character(age),
         i.check.value = "",
         i.check.issue_id = "logic_m_requirement_below_age",
         i.check.issue = "below_age",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondents_not_of_age")

# testing_data
df_testing_data <- df_tool_data %>% 
  filter(i.check.uuid %in% c("bd91c99d-1f24-47c4-b18e-1dcba74ce5f3", "9e4810d2-567b-4661-9cc8-7eb3bf6fa7f3", 
                             "4cb86dd0-dc6a-4f1c-887d-2e96ade7ada1", "a4ff310a-cf23-451f-b6e5-8e6d145e0980", 
                             "79bea2f0-0828-4787-a1a6-ba495220be59", "d6d40d8a-dbc5-4e27-88f7-0ac89a9cfdc9",  
                             "32d2ce30-05fd-48bb-9118-6586d7cc94b4", "e5b128a2-a11c-4750-a900-137e5a0b05dc", 
                             "a533ab62-646b-4312-b4e1-0dd3856240ab", "f7342acf-e5ae-4f0b-b9f0-955eb5a2f75b", 
                             "2fa4b845-ba90-40cf-96c0-3dfe4fee93da",  "da82f173-8e1e-4435-8ff1-02646c0f712b", 
                             "865e2735-1c5b-4507-9f9a-4897bd17cc0c",  "6d2a6f23-25e6-4dbb-b766-e6078668fb00", 
                             "81e3befe-9d40-4269-b161-248489840102",  "b1fdd63d-a33b-4906-bb55-176d34cad4ba",
                             "afcdd957-a34a-47c9-97a8-b97f4d240f75")) %>%  
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

# un_reliable_enumerator
# df_un_reliable_enumerator <- df_tool_data %>% 
#   filter(enumerator_id %in% c("110")) %>% 
#   mutate(i.check.type = "remove_survey",
#          i.check.name = "enumerator_id",
#          i.check.current_value = as.character(enumerator_id),
#          i.check.value = "",
#          i.check.issue_id = "logic_m_un_reliable_enumerator",
#          i.check.issue = "un_reliable_enumerator",
#          i.check.other_text = "",
#          i.check.checked_by = "",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = "", 
#          i.check.reviewed = "1",
#          i.check.adjust_log = "",
#          i.check.uuid_cl = "",
#          i.check.so_sm_choices = "") %>% 
#   dplyr::select(starts_with("i.check")) %>% 
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
# 
# add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_un_reliable_enumerator")


# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 15
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_time_btn_survey")


# others checks -----------------------------------------------------------

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)
add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_others_data")