
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

# Respondent reports majority of the communities get along, but reports refugees can not be trusted. i.e.
# idi_reporting_people_get_on_well = "yes" but host_trusting_refugee_community = "no"
df_people_get_along_well_refugee_1 <- df_tool_data %>% 
  filter(idi_reporting_people_get_on_well %in% c("yes"), 
           host_trusting_refugee_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "host_trusting_refugee_community", 
         i.check.current_value = host_trusting_refugee_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_people_get_along_well_refugee_1",
         i.check.issue = glue("idi_reporting_people_get_on_well: {idi_reporting_people_get_on_well}, 
                              host_trusting_refugee_community: {host_trusting_refugee_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_people_get_along_well_refugee_1")


# Respondent reports majority of the communities get along, but reports host community can not be trusted. i.e.
# idi_reporting_people_get_on_well = "yes" but refugee_trusting_host_community = "no"
df_people_get_along_well_host_2 <- df_tool_data %>% 
  filter(idi_reporting_people_get_on_well %in% c("yes"), 
         refugee_trusting_host_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "refugee_trusting_host_community", 
         i.check.current_value = refugee_trusting_host_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_people_get_along_well_host_2",
         i.check.issue = glue("idi_reporting_people_get_on_well: {idi_reporting_people_get_on_well}, 
                              refugee_trusting_host_community: {refugee_trusting_host_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_people_get_along_well_host_2")


# Refugee graduate reports having been trying to learn a host community language, but the majority of people cannot be trusted. i.e.
# refugee_graduate_learning_local_language = "yes" but refugee_trusting_host_community = "no" 
df_refugee_graduate_learning_local_language_3 <- df_tool_data %>% 
  filter(refugee_graduate_learning_local_language %in% c("yes"), 
         refugee_trusting_host_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "refugee_trusting_host_community", 
         i.check.current_value = refugee_trusting_host_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_refugee_graduate_learning_local_language_3",
         i.check.issue = glue("refugee_graduate_learning_local_language: {refugee_graduate_learning_local_language}, 
                              refugee_trusting_host_community: {refugee_trusting_host_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_refugee_graduate_learning_local_language_3")


# Host graduate reports having been trying to learn a refugee language, but the majority of people cannot be trusted. i.e.
# host_graduate_learning_refugee_language = "yes" but host_trusting_refugee_community = "no"
df_host_graduate_learning_local_language_4 <- df_tool_data %>% 
  filter(host_graduate_learning_refugee_language %in% c("yes"), 
         refugee_trusting_host_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "refugee_trusting_host_community", 
         i.check.current_value = refugee_trusting_host_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_host_graduate_learning_local_language_4",
         i.check.issue = glue("host_graduate_learning_refugee_language: {host_graduate_learning_refugee_language}, 
                              refugee_trusting_host_community: {refugee_trusting_host_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_host_graduate_learning_local_language_4")


# Refugee graduate reports having been trying to learn a host community language, but the majority of the two communities do not get along. i.e.
# refugee_graduate_learning_local_language = "yes" but idi_reporting_people_get_on_well = "no"
df_refugee_graduate_learning_local_language_5 <- df_tool_data %>% 
  filter(refugee_graduate_learning_local_language %in% c("yes"), 
         idi_reporting_people_get_on_well %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "idi_reporting_people_get_on_well", 
         i.check.current_value = idi_reporting_people_get_on_well,
         i.check.value = "", 
         i.check.issue_id = "logic_c_refugee_graduate_learning_local_language_5",
         i.check.issue = glue("refugee_graduate_learning_local_language: {refugee_graduate_learning_local_language}, 
                              idi_reporting_people_get_on_well: {idi_reporting_people_get_on_well}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_refugee_graduate_learning_local_language_5")


# Host graduate reports having been trying to learn a refugee language, but the majority of the two communities do not get along. i.e.
# host_graduate_learning_refugee_language = "yes" but idi_reporting_people_get_on_well = "no"
df_host_graduate_learning_refugee_language_6 <- df_tool_data %>% 
  filter(host_graduate_learning_refugee_language %in% c("yes"), 
         idi_reporting_people_get_on_well %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "idi_reporting_people_get_on_well", 
         i.check.current_value = idi_reporting_people_get_on_well,
         i.check.value = "", 
         i.check.issue_id = "logic_c_host_graduate_learning_refugee_language_6",
         i.check.issue = glue("host_graduate_learning_refugee_language: {host_graduate_learning_refugee_language}, 
                              idi_reporting_people_get_on_well: {idi_reporting_people_get_on_well}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_host_graduate_learning_refugee_language_6")


# Refugee graduate reports having joined a mixed savings group, but the majority of the host community cannot be trusted. i.e.
# idi_joined_mixed_savings_group = "yes" but refugee_trusting_host_community = "no"
df_refugee_graduate_joined_mixed_savings_group_7 <- df_tool_data %>% 
  filter(idi_joined_mixed_savings_group %in% c("yes"), 
         refugee_trusting_host_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "refugee_trusting_host_community", 
         i.check.current_value = refugee_trusting_host_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_refugee_graduate_joined_mixed_savings_group_7",
         i.check.issue = glue("idi_joined_mixed_savings_group: {idi_joined_mixed_savings_group}, 
                              refugee_trusting_host_community: {refugee_trusting_host_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_refugee_graduate_joined_mixed_savings_group_7")


# host graduate reports having joined a mixed savings group, but the majority of the refugee community cannot be trusted. i.e.
# idi_joined_mixed_savings_group = "yes" but host_trusting_refugee_community = "no"
df_host_graduate_joined_mixed_savings_group_8 <- df_tool_data %>% 
  filter(idi_joined_mixed_savings_group %in% c("yes"), 
         host_trusting_refugee_community %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "host_trusting_refugee_community", 
         i.check.current_value = host_trusting_refugee_community,
         i.check.value = "", 
         i.check.issue_id = "logic_c_host_graduate_joined_mixed_savings_group_8",
         i.check.issue = glue("idi_joined_mixed_savings_group: {idi_joined_mixed_savings_group}, 
                              host_trusting_refugee_community: {host_trusting_refugee_community}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_host_graduate_joined_mixed_savings_group_8")


# Host graduate reports refugees are really part of this community, but the majority of both communities generally do not get along. i.e.
# host_feeling_refugees_as_part_of_community = "yes" but idi_reporting_people_get_on_well = "no"
df_host_graduate_feels_refugees_part_of_community_9 <- df_tool_data %>% 
  filter(host_feeling_refugees_as_part_of_community %in% c("yes"), 
         idi_reporting_people_get_on_well %in% c("no")) %>% 
  mutate(i.check.deviceid = deviceid,
         i.check.type = "change_response",
         i.check.name = "idi_reporting_people_get_on_well", 
         i.check.current_value = idi_reporting_people_get_on_well,
         i.check.value = "", 
         i.check.issue_id = "logic_c_host_graduate_feels_refugees_part_of_community_9",
         i.check.issue = glue("host_feeling_refugees_as_part_of_community: {host_feeling_refugees_as_part_of_community}, 
                              idi_reporting_people_get_on_well: {idi_reporting_people_get_on_well}"),
         i.check.other_text = "",
         i.check.checked_by = "MT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_host_graduate_feels_refugees_part_of_community_9")



