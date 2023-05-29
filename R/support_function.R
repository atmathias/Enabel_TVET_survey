
# add checks data to a list of checks  ------------------------------------

add_checks_data_to_list <- function(input_list_name, input_df_name) {
  if(exists(input_list_name) & exists(input_df_name)){
    # get the current values of these objects
    global_list_data <- get(input_list_name, envir = .GlobalEnv)
    global_df_data <- get(input_df_name, envir = .GlobalEnv)
    # check if the dataframe of interest has data
    if(nrow(global_df_data) > 0){
      # append the data frame to the list
      global_list_data[[input_df_name]] <-  global_df_data
      # assign the data to the global environment
      assign(x = input_list_name, value = global_list_data, envir = .GlobalEnv)
    }
  } else{
    message("given objects not in the global environment.")
  }
}

# extract others checks ---------------------------------------------------

extract_other_data <- function(input_tool_data, input_survey, input_choices) {
  
  # add and rename some columns
  df_data <- input_tool_data %>% 
    rename(uuid = `_uuid`) %>% 
    mutate(start_date = as_date(start))
  
  # get questions with other
  others_colnames <-  df_data %>% 
    select(ends_with("_other"), -contains("/")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  df_other_response_data <- data.frame()
  
  for (cln in others_colnames) {
    
    current_parent_qn = str_replace_all(string = cln, pattern = "_other$", replacement = "")
    
    df_filtered_data <- df_data %>% 
      select(-contains("/")) %>% 
      select(uuid, start_date, enumerator_id, location, hh_id, other_text = cln, current_value = current_parent_qn) %>% 
      filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
      mutate( other_name = cln, 
              int.my_current_val_extract = ifelse(str_detect(current_value, "\\bother\\b"), str_extract_all(string = current_value, pattern = "\\bother\\b|\\w+_other\\b"), current_value),
              value = "",
              parent_qn = current_parent_qn)
    df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
  }
  
  # arrange the data
  df_data_arranged <- df_other_response_data %>% 
    arrange(start_date, uuid)
  
  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)
  
  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    rename(name = parent_qn)
  
  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>% 
    left_join(df_grouped_choices, by = "list_name") %>% 
    mutate(issue_id = "other_checks",
           issue = "",
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>% 
    filter(str_detect(string = current_value, pattern = "\\bother\\b|\\w+_other\\b"))
  
  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_one|select one"))) %>% 
    mutate(type = "change_response")
  
  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_multiple|select multiple")))
  
  select_mu_add_option <- select_mu_data %>% 
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>% 
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))
  
  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>% 
    arrange(uuid, start_date, enumerator_id, name)
  
  # merge other checks
  merged_other_checks <- bind_rows(output) %>% 
    mutate(uuid_cl = "",
           so_sm_choices = choice_options) %>% 
    select(uuid,
           start_date,
           enumerator_id,
           location,
           hh_id,
           type,
           name,
           current_value,
           value,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           uuid_cl,
           so_sm_choices)
}

# check duplicate uuid ----------------------------------------------------------

check_duplicates_by_uuid <- function(input_tool_data) {
  input_tool_data %>% 
    group_by(i.check.uuid) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank > 1) %>%  
    mutate(
      i.check.type = "remove_survey",
      i.check.name = "hh_id",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "duplicate_uuid",
      i.check.issue = "The uuid: {i.check.uuid} is duplicate in the data",
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "", 
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.uuid_cl = "",
      i.check.so_sm_choices = "")%>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check for duplicate hhid numbers
check_duplicate_hhid_numbers <- function(input_tool_data, input_sample_hhid_nos_list) {
  input_tool_data %>% 
    mutate(unique_hhid_number = hh_id) %>% 
    group_by(i.check.location, i.check.hh_id) %>% 
    filter(n() > 1, unique_hhid_number %in% input_sample_hhid_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_id",
           i.check.current_value = hh_id,
           i.check.value = "",
           i.check.issue_id = "hhid_c_duplicate_hhid_no",
           i.check.issue = glue("hh_id: {hh_id} is duplicated: check that its not a repeated survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check for point number not being in samples
check_hhid_number_not_in_samples <- function(input_tool_data, input_sample_hhid_nos_list) {
  input_tool_data %>% 
    mutate(unique_hhid_number = hh_id) %>% 
    filter(!unique_hhid_number %in% input_sample_hhid_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "hh_id",
           i.check.current_value = hh_id,
           i.check.value = "",
           i.check.issue_id = "hhid_c_hhid_no_not_in_sample",
           i.check.issue = glue("hh_id: {hh_id} not in samples"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# survey time check -------------------------------------------------------

# check survey time against expected minimum time and maximum time of the survey
check_survey_time <- function(input_tool_data, input_min_time, input_max_time) {
  input_tool_data %>% 
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval),
           i.check.type = "remove_survey",
           i.check.name = "hh_id",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = case_when(
             int.survey_time_interval < input_min_time ~ "less_survey_time",
             int.survey_time_interval > input_max_time ~ "more_survey_time",
             TRUE ~ "normal_survey_time" ),
           i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "")%>% 
    filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check interval between surveys by the same enumerator
check_time_interval_btn_surveys <- function(input_tool_data, input_min_time) {
  input_tool_data %>% 
    group_by(i.check.start_date, i.check.enumerator_id) %>%
    filter(n()>1) %>% 
    arrange(start, .by_group = TRUE) %>%
    mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
           int.time_between_survey = ceiling(int.time_between_survey)) %>%
    filter(int.time_between_survey != 0 & int.time_between_survey < input_min_time) %>%
    mutate(i.check.type = "remove_survey",
           i.check.name = "hh_id",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "less_time_btn_surveys",
           i.check.issue = glue("{int.time_between_survey} min taken between surveys"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# Outliers ----------------------------------------------------------------
check_outliers <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit) {
  input_tool_data %>% 
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

check_outliers_repeats <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit, input_sheet_name) {
  input_tool_data %>% 
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>% 
    mutate(i.check.sheet = input_sheet_name,
           i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "NA",
           i.check.index = `_index`,
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# cleaning support for different datasets -----------------------

implement_cleaning_support <- function(input_df_raw_data, input_df_survey, input_df_choices, input_df_cleaning_log) {
  
  # find all new choices to add to choices sheet
  
  # gather choice options based on unique choices list
  df_grouped_choices<- input_df_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : "))
  
  # get new name and choice pairs to add to the choices sheet
  new_vars <- input_df_cleaning_log %>% 
    filter(type %in% c("change_response", "add_option")) %>% 
    left_join(input_df_survey, by = "name") %>% 
    filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
    separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = value ) ) %>%
    rename(choice = value ) %>%
    select(name, choice) %>%
    distinct() %>% # to make sure there are no duplicates
    arrange(name)
  
  # create kobold object
  
  kbo <- kobold::kobold(survey = input_df_survey, 
                        choices = input_df_choices, 
                        data = input_df_raw_data, 
                        cleaning = input_df_cleaning_log)
  
  # modified choices for the survey tool
  df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)
  
  # special treat for variables for select_multiple, we need to add the columns to the data itself
  df_survey_sm <- input_df_survey %>% 
    mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                              str_detect(string = type, pattern = "select_one|select one") ~ "so",
                              TRUE ~ type)) %>% 
    select(name, q_type)
  
  # construct new columns for select multiple
  new_vars_sm <- new_vars %>% 
    left_join(df_survey_sm, by = "name") %>% 
    filter(q_type == "sm") %>% 
    mutate(new_cols = paste0(name,"/",choice))
  
  # add new columns to the raw data
  df_raw_data_modified <- input_df_raw_data %>% 
    butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )
  
  # make some cleanup
  kbo_modified <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                                 choices = df_choises_modified, 
                                 data = df_raw_data_modified, 
                                 cleaning = input_df_cleaning_log)
  kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)
  
  # handling Personally Identifiable Information(PII)
  input_vars_to_remove_from_data <- c("complainant_name",
                                      "complainant_id",
                                      "respondent_telephone",
                                      "name_pers_recording",
                                      "geopoint",
                                      "_geopoint_latitude",
                                      "_geopoint_longitude",
                                      "_geopoint_altitude",
                                      "_geopoint_precision")
  
  df_handle_pii <- kbo_cleaned$data %>% 
    mutate(across(any_of(input_vars_to_remove_from_data), .fns = ~na_if(., .)))
  
  # handling added responses after starting data collection and added responses in the cleaning process
  
  sm_colnames <-  df_handle_pii %>% 
    select(contains("/")) %>% 
    colnames() %>% 
    str_replace_all(pattern = "/.+", replacement = "") %>% 
    unique()
  
  df_handle_sm_data <- df_handle_pii
  
  for (cur_sm_col in sm_colnames) {
    df_updated_data <- df_handle_sm_data %>% 
      mutate(
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
      )
    df_handle_sm_data <- df_updated_data
  }
  
  df_final_cleaned_data <- df_handle_sm_data
}
