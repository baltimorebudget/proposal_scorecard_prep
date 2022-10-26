# Budget and Position Information in HTML

# This script is used to generate HTML tables with budget and position 
# information by service. The resulting csv is then sent to ClearImpact. The 
# company imports the HTML tables into each service's page, so that analysts do
# not have to update this info manually in preparation for budget proposals.

params <- list(fy = 23)

library(tidyverse)
library(rio)
library(bbmR)
library(httr)
library(jsonlite)
library(magrittr)

recategorize_fund_service <- function(df) {
  # 385, 493, 590 are cases where Scorecard is organized differently 
  # (ex: 493a, 493b, etc) from how the budget is organized (by activity)
  df %>%
  mutate(`Fund Name` = ifelse(`Fund Name` == "General", "General", "Other"),
         `Service ID` = case_when(
           `Service ID` == "385" & grepl("Pre and Postnatal", `Activity Name`) ~ "385a",
           `Service ID` == "385" & grepl("Legal Aid", `Activity Name`) ~ "385b",
           `Service ID` == "493" & grepl("Baltimore Museum", `Activity Name`) ~ "493a",
           `Service ID` == "493" & grepl("Walters Art", `Activity Name`, `Activity Name`) ~ "493b",
           `Service ID` == "493" & grepl("Baltimore Symphony", `Activity Name`) ~ "493c",
           `Service ID` == "493" & grepl("Maryland Zoo", `Activity Name`) ~ "493d",
           `Service ID` == "590" & grepl("Baltimore Public Markets", `Activity Name`) ~ "590a",
           `Service ID` == "590" & grepl("Lexington Market", `Activity Name`) ~ "590b",
           `Service ID` == "590" & grepl("Baltimore Heritage Area", `Activity Name`) ~ "590c",
           TRUE ~ `Service ID`)) 
}

# dollars <- read_rds("G:/Budget Publications/automation/0_data_prep/outputs/fy23_cls/expenditure.Rds") %>%
#   recategorize_fund_service() %>%
#   group_by(`Agency Name`, `Service ID`, `Service Name`, `Fund Name`) %>%
#   summarize_at(vars(`FY21 Actual`, `FY22 Budget`, `FY23 CLS` = `FY23 Budget`), sum, na.rm = TRUE) %>%
#   filter_at(vars(`FY22 Budget`, `FY23 CLS`), any_vars(. != 0))

###
# mira provided cleaned FY21 actuals for the FY23 planning year, so using this instead of the usual process
cleaned <- list()
cleaned$raw <- import("inputs/FY21-FY22-FY23.xlsx")

cleaned$prior <- cleaned$raw %>%
  pivot_longer(starts_with("FY21"), names_to = "Fund Name", values_to = "FY21 Actual") %>%
  mutate(`Fund Name` = ifelse(grepl("General", `Fund Name`, fixed = TRUE), "General", "Other")) %>%
  select(`Agency ID`, `Service ID` = `Program ID`, 
         `Fund Name`, `FY21 Actual`)

cleaned$projection <- cleaned$raw %>%
  pivot_longer(starts_with("FY22"), names_to = "Fund Name", values_to = "FY22 Budget") %>%
  mutate(`Fund Name` = ifelse(grepl("General", `Fund Name`, fixed = TRUE), "General", "Other")) %>%
  select(`Agency ID`, `Service ID` = `Program ID`, 
         `Fund Name`, `FY22 Budget`)

cleaned$planning <- cleaned$raw %>%
  pivot_longer(starts_with("FY23"), names_to = "Fund Name", values_to = "FY23 CLS") %>%
  mutate(`Fund Name` = ifelse(grepl("General", `Fund Name`, fixed = TRUE), "General", "Other")) %>%
  select(starts_with("Agency"), `Service ID` = `Program ID`, `Service Name` = `Program Name`, 
         `Fund Name`, `FY23 CLS`)

cleaned$final <- cleaned$planning %>%
  left_join(cleaned$projection) %>%
  left_join(cleaned$prior) %>%
  relocate(`FY22 Budget`, .before = `FY23 CLS`) %>%
  relocate(`FY21 Actual`, .before = `FY22 Budget`) %>%
  mutate_at(vars(ends_with("ID")), as.character)

dollars <- cleaned$final

###

positions <- read_rds("G:/Budget Publications/automation/0_data_prep/outputs/fy23_cls/positions.Rds") %>%
  map(recategorize_fund_service) %>%
  map(group_by, `Agency Name`, `Service ID`, `Service Name`, `Fund Name`) %>%
  map(count)

positions <- positions$planning %>%
  rename(`FY23 CLS` = n) %>%
  left_join(positions$projection %>%
              rename(`FY22 Budget` = n),
            by = c("Service ID", "Fund Name"), suffix = c("", " - Current")) %>%
  left_join(positions$prior %>%
              rename(`FY21 Actual` = n),
            by = c("Service ID", "Fund Name"), suffix = c("", " - Last")) %>%
  select(-ends_with(c("Name - Last", "Name - Current"))) %>%
  select(`Agency Name`, `Service ID`, `Service Name`, `Fund Name`, 
         `FY21 Actual`, `FY22 Budget`, `FY23 CLS`)

# used to confirm numbers are correct prior to upload 
file_name <- paste0("outputs/FY", params$fy, " Scorecard Numbers Check.xlsx")

export_excel(dollars, "dollars", file_name, "new")
export_excel(positions, "positions", file_name, "existing")

# prepare data in format ClearImpact needs for uploading to Scorecard 

dollars <- dollars %>%
  ungroup() %>%
  select(-`Agency Name`, -`Service Name`)

positions <- positions %>%
  ungroup() %>%
  select(-`Agency Name`, -`Service Name`)

services <- httr::POST(
    "https://api.resultsscorecard.com/api/programs/list", 
    body = 
      list(siteCode = "OutcomeStat",
           apiKey = Sys.getenv("SC_API_KEY")),
    encode = "json") %>%
  content() %>%
  toJSON() %>%
  fromJSON()

final <- services %>%
  select(-(createDate:modifiedBy), -primaryTag) %>% 
  filter(!(grepl("(MOSS)|(Copy)", title)),
         grepl("Service [0-9]{3}[a-z]{0,1}:", title)) %>%
  mutate(`Service ID` = str_extract(title, "[0-9]{3}")) %>%
  full_join(dollars, by = "Service ID") %>%
  left_join(positions, by = c("Service ID", "Fund Name"), suffix = c("", " Pos")) %>%
  filter(!is.na(`Fund Name`)) %>%
  mutate_at(vars(c("id", "title")), as.character) %>%
  # add missing rows, if a service doesn't have General or "Other" funds show 0
  complete(nesting(`Service ID`, id, title), `Fund Name`) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  filter(id != "NULL")

total <- final %>%
  ungroup() %>%
  group_by(`Service ID`, id, title) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(`Fund Name` = "Total")

final %<>%
  bind_rows(total) %>%
  mutate_if(is.numeric, scales::comma, accuracy = 1L)

x <- final %>%
  distinct(`Service ID`) %>%
  extract2("Service ID")

out <- map(x, function(x) {
  
  df <- final %>%
    filter(`Service ID` == x)
  
  out <- df %>%
    distinct(id, title, `Service ID`) %>%
    mutate(HTML = paste0(
      "<p><em>For your reference, the Fiscal 20", params$fy - 2 , 
      " Actual, Fiscal 20", params$fy - 1 , " Adopted, and Fiscal 20", params$fy, 
      " Current Level of Service (CLS) expenditure and position information
      have been populated below.</em></p>",
      
      "<h4><strong>Fiscal 20", params$fy - 2 , " Actual</strong></h4>
      
      <table><tbody>
        <tr style='text-align:right'>
          <td>&nbsp;</td>
          <td><strong>General Fund</strong></td>
          <td><strong>Other Funds</strong></td>
          <td><strong>Total</strong></td>
        </tr>
        <tr>
          <td><strong>Expenditures</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual")]][df$`Fund Name` == "Total"], "</td>
        </tr>
        <tr>
          <td><strong>Funded Full Time Positions</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual Pos")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual Pos")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 2, " Actual Pos")]][df$`Fund Name` == "Total"], "</td>
        </tr>
      </tbody></table>
      
      <h4><strong>Fiscal 20", params$fy - 1, " Adopted Budget</strong></h4>
      
      <table><tbody>
        <tr style='text-align:right'>
          <td>&nbsp;</td>
          <td><strong>General Fund</strong></td>
          <td><strong>Other Funds</strong></td>
          <td><strong>Total</strong></td>
        </tr>
        <tr>
          <td><strong>Expenditures</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget")]][df$`Fund Name` == "Total"], "</td>
        </tr>
        <tr>
          <td><strong>Funded Full Time Positions</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget Pos")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget Pos")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy - 1, " Budget Pos")]][df$`Fund Name` == "Total"], "</td>
        </tr>
      </tbody></table>
      
      <h4><strong>Fiscal 20", params$fy, " Current Level of Service Budget</strong></h4>
      
      <table><tbody>
        <tr style='text-align:right'>
          <td>&nbsp;</td>
          <td><strong>General Fund</strong></td>
          <td><strong>Other Funds</strong></td>
          <td><strong>Total</strong></td>
        </tr>
        <tr>
          <td><strong>Expenditures</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS")]][df$`Fund Name` == "Total"], "</td>
        </tr>
        <tr>
          <td><strong>Funded Full Time Positions</strong></td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS Pos")]][df$`Fund Name` == "General"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS Pos")]][df$`Fund Name` == "Other"], "</td>
          <td style='text-align:right'>", df[[paste0("FY", params$fy, " CLS Pos")]][df$`Fund Name` == "Total"], "</td>
      </tr></tbody></table>"))
  }
)

out <- bind_rows(out)

export(out, paste0("outputs/fy", params$fy, "_scorecard_info.csv"))
