.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(tidyverse)
library(magrittr)
library(rio)
library(assertthat)
library(httr)
library(jsonlite)
library(openxlsx)
library(dplyr)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/bbmR")
source("G:/Budget Publications/automation/0_data_prep/bookDataPrep/R/scorecard.R")


scorecard <- list(
  services = get_sc_services())

scorecard$pm <- map(scorecard$services$sc.service.id, get_sc_pm) %>%
  compact() %>%
  bind_rows() %>%
  left_join(scorecard$services) %>%
  mutate_all(as.character) %>%
  filter(Type %in% c("Outcome", "Effectiveness", "Efficiency", "Output"))

scorecard$value <- map(scorecard$pm$sc.pm.id, get_sc_value) %>%
  data.table::rbindlist(fill = TRUE) %>%
  left_join(scorecard$pm) %>%
  rename(Actual = actualValue, Target = targetValue) %>%
  # historically we haven't differentiated between FY and calendar year PMs;
  # this gets rid of that distinction in the table
  mutate(timePeriod = as.numeric(gsub("FY ", "", timePeriod))) %>%
  select(-starts_with("sc.")) %>%
  pivot_wider(names_from = timePeriod,
              values_from = c("Actual", "Target"), 
              names_sep = " ", values_fn = list(Actual = mean, Target = mean))

scorecard_export <- scorecard$value%>%
  select(`Service Name`, `Service ID`, `Type`, `Measure`, `Target 2021`, `Actual 2021`, `Target 2022`, `Target 2023`) %>%
  mutate(`Service Name` = gsub(".*:", "", `Service Name`)) %>%
  filter(!is.na(`Service ID`))

scorecard_pivot <- scorecard_export %>% 
  group_split(`Service Name`) %>% 
  as.list(name = `Service Name`) %>% 
  map(pivot_wider)

names_list <- unique(as.numeric(scorecard_export$`Service ID`))
#names_list[order(names_list)]

scorecard_df <- scorecard_export %>%
  #data.table::rbindlist() %>%
  group_split(`Service Name`) %>%
  map(pivot_wider) %>%
  map(as.data.frame)
  

params = list(svc_id = scorecard_df$`Service ID`, file_path = "G:/Analyst Folders/Lillian/exp_planning_year/2a_proposal_scorecard_prep/outputs/", name = paste0(scorecard_df$`Service Name`, " PM for Results Teams.xlsx"))

for (i in scorecard_df) {
  df <- as.data.frame(scorecard_df[[i]])
  tab <- scorecard_df[[i]][[2]][[1]]
  export_excel_tabs(df = df, tab_name = tab, file_name = paste0(params$file_path, params$name), "existing")
  }

write.xlsx(x = scorecard_df, file = paste0(params$file_path, "Test.xlsx"))

