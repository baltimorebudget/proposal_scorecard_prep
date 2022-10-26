# Used to compare performance measure target values between two points in time.
# Typically done to ensure that agencies have not changed their targets at
# times when they are not supposed to.

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

agency <- import("G:/Fiscal Years/Fiscal 2023/Projections Year/1. July 1 Prepwork/Appropriation File/Fiscal 2023 Appropriation File_With_Positions_WK_Accounts.xlsx",
                 which = "FY23 Appropriation File") %>%
  set_colnames(rename_cols(.)) %>%
  mutate_at(vars(ends_with("ID")), as.character) %>%
  distinct(`Agency ID`, `Agency Name`, `Service ID` = `Program ID`)

target <- list(old = import("inputs/2019-05 PM Target Value Pivot.csv"),
               new = import("inputs/2019-10 PM Target Value Pivot.csv")) %>%
  map(select, `Service ID`:`Priority Measure`, `2019`, `FY 2019`) %>%
  map(filter, Type %in% c("Outcome", "Effectiveness", "Efficiency", "Output"),
         !grepl("(Copy)|(MOSS)", `Priority Measure`),
         !grepl("[No Longer Reported]", `Priority Measure`, fixed = TRUE),
         !grepl("(Copy)|(MOSS)", `Service Name`)) %>% 
  map(mutate_all, funs(gsub(",", "", .))) %>%
  map(mutate_at, vars(c("2019", "FY 2019")), as.numeric) %>%
  map(mutate,
        `Service ID` = str_extract(`Service Name`, "[[:digit:]]{3}"),
        `Service Name` = gsub("Service .*\\:", "", `Service Name`))

df <- full_join(target$old, target$new,
                by = c("Service ID", "Type", "PMID"),
                suffix = c(".old", ".new")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(`Diff` = `2019.old` != `2019.new`,
         `Diff FY` = `FY 2019.old` != `FY 2019.new`) %>%
  # filter(`Diff` == TRUE | `Diff FY` == TRUE,
  #        !is.na(`Service ID`)) %>%
  left_join(agency)

export_excel(df, "Diff Targets", "Target PM Comparison.xlsx", "new")
