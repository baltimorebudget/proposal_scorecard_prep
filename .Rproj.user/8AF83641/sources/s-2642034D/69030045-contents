.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(tidyverse)
library(magrittr)
library(rio)
library(assertthat)
library(httr)
library(jsonlite)
library(openxlsx)
library(dplyr)
library(scales)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/bbmR")
source("G:/Budget Publications/automation/0_data_prep/bookDataPrep/R/scorecard.R")
source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/formatting.R")

##position data ===============

positions_22 <- import("G:/Fiscal Years/Fiscal 2022/Projections Year/1. July 1 Prepwork/Positions/FY22 Position File Reset to DHR.xlsx", which = "FY22 All Positions") %>%
  filter(Funding == "Funded" & !is.na(`Job  Number`)) %>%
  mutate(Fund = case_when(`Fund ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2022) %>%
  group_by(FY, `Agency ID`, `Agency Name`, `Program ID`, `Program Name`, `Fund`) %>%
  summarise(`FY22 Positions` = n())

positions_23 <- import("G:/Fiscal Years/Fiscal 2023/Projections Year/1. July 1 Prepwork/Positions/Fiscal 2023 Appropriation File_Change_Tables.xlsx") %>%
  filter(FUNDING == "Funded" & !is.na(`JOB NUMBER`)) %>%
  mutate(Fund = case_when(`FUND ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2023) %>%
  group_by(FY, `AGENCY ID`, `AGENCY NAME`, `PROGRAM ID`, `PROGRAM NAME`, `Fund`) %>%
  summarise(`FY23 Positions` = n())

positions_24 <- import("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/2. Position Reports/PositionsSalariesOpcs_2022-10_14_CLS.xlsx", which = "FY24 CLS") %>%
  filter(!is.na(`JOB NUMBER`)) %>%
  mutate(Fund = case_when(`FUND ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2024) %>%
  group_by(FY, `AGENCY ID`, `AGENCY NAME`, `PROGRAM ID`, `PROGRAM NAME`, `Fund`) %>%
  summarise(`FY24 Positions` = n())

positions <- positions_24 %>% 
  full_join(positions_23, by = c("FY", "AGENCY ID", "AGENCY NAME", "PROGRAM ID", "PROGRAM NAME", "Fund")) %>%
  full_join(positions_22, by = c("FY", "AGENCY ID" = "Agency ID", "AGENCY NAME" = "Agency Name", 
                                 "PROGRAM ID" = "Program ID", "PROGRAM NAME" = "Program Name",
                                 "Fund")) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  mutate(Type = "Positions",
         Total = `FY24 Positions` + `FY23 Positions` + `FY22 Positions`) %>%
  relocate(`Type`, .before = `FY`) %>%
  select(-`FY24 Positions`, -`FY23 Positions`, -`FY22 Positions`)

colnames(positions)<- c("Type", "FY", "Agency ID", "Agency Name", "Program ID", "Program Name", "Fund", "Total")

##expenditure data ========================
##two fiscal years ago
actuals <- import("G:/Fiscal Years/Fiscal 2022/Projections Year/2. Monthly Expenditure Data/Month 12_June Projections/Expenditure 2022-06_Run7.xlsx",
                  which = "CurrentYearExpendituresActLevel") %>%
  mutate(Fund = case_when(`Fund ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2022,
         Type = "Expenditures") %>%
  group_by(Type, FY, `Agency ID`, `Agency Name`, `Program ID`,  `Program Name`, `Fund`) %>%
  summarise(`FY22 Actual` = replace_na(sum(`BAPS YTD EXP`, na.rm = TRUE), 0)) 
# pivot_wider(names_from = Fund, values_from = `FY22 Actual`) %>%
# rename(`FY22 Actual General Fund` = `General Fund`, `FY22 Actual Other Funds` = `Other Funds`)

#current budget and prior adopted
cls <- import("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/1. Line Item Reports/line_items_2022-10-26 - mmk in progress - do not touch.xlsx",
              which = "Line Items") %>%
  mutate(Fund = case_when(`Fund ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2024,
         Type = "Expenditures") %>%
  group_by(Type, FY, `Agency ID`, `Agency Name`, `Program ID`, `Program Name`, `Fund`) %>%
  summarise(`FY24 CLS` = replace_na(sum(`FY24 CLS`, na.rm = TRUE), 0)) 

budget_23 <- import("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/1. Line Item Reports/line_items_2022-10-26 - mmk in progress - do not touch.xlsx",
              which = "Line Items") %>%
  mutate(Fund = case_when(`Fund ID` == 1001 ~ "General Fund",
                          TRUE ~ "Other Funds"),
         FY = 2023,
         Type = "Expenditures") %>%
  group_by(Type, FY, `Agency ID`, `Agency Name`, `Program ID`, `Program Name`, `Fund`) %>%
  summarise(`FY23 Budget` = replace_na(sum(`FY23 Adopted`, na.rm = TRUE), 0)) 

##join datasets ====================
expend <- cls %>% full_join(budget_23, by = c("Type", "FY", "Agency ID", "Agency Name", "Program ID", "Program Name", "Fund")) %>%
  full_join(actuals, by = c("Type", "FY", "Agency ID", "Agency Name", "Program ID", "Program Name", "Fund")) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  mutate(Total = `FY24 CLS` + `FY23 Budget` + `FY22 Actual`) %>%
  relocate(`Type`, .before = `FY`) %>%
  select(-`FY24 CLS`, -`FY23 Budget`, -`FY22 Actual`)

df <- expend %>% rbind(positions) %>%
    pivot_wider(names_from = Fund, values_from = c(Total)) %>%
  arrange(`Agency ID`, `Program ID`, Type, FY) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  filter(!is.na(`Agency ID`)) %>%
  mutate(`General Fund` = format_number(`General Fund`, accuracy = 1),
         `Other Funds` = format_number(`Other Funds`, accuracy = 1))

export_excel(df, tab_name = "FY22-FY24", "outputs/FY24 Budget Data.xlsx")