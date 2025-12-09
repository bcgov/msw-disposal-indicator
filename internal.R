# Copyright 2025 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(readr)
library(dplyr)
library(ggplot2)
library(bcdata)
library(stringdist)
library(stringi)

source("R/helpers.R")

# Get current data from BC Data Catalogue:
# web link: "https://catalogue.data.gov.bc.ca/dataset/d21ed158-0ac7-4afd-a03b-ce22df0096bc/resource/d2648733-e484-40f2-b589-48192c16686b/download/bc_municipal_solid_waste_disposal.csv"

old_msw <- bcdc_get_data("d21ed158-0ac7-4afd-a03b-ce22df0096bc")

# Add new data -----------------------------------------------------------
## Data obtained from program area and put in 'data/' folder

data_2023 <- read_csv("data/2023-MSW-Disposal.csv") %>%
  mutate(Year = 2023,
         Member = recode(Member, "Comox Valley Regional District (Strathcona)" = "Comox-Strathcona"),
         Member = gsub("^Regional District( of)? | Regional (District|Municipality)$", "", Member)) %>%
  rename("Regional_District" = Member) %>%
  filter(Regional_District != "TOTAL BC") %>%
  arrange(stri_trans_totitle(Regional_District))

data_2023 <- data_2023 %>%
  rowwise() %>%
  mutate(diff = list(data.frame(dist = stringdist(Regional_District,unique(old_msw$Regional_District)), 
                                rd = unique(old_msw$Regional_District)))) %>%
  mutate(new_name = list(diff %>% filter(dist == min(dist)) %>% select(rd))) %>%
  ungroup()%>%
  mutate("Regional_District" = unlist(new_name,use.names = F)) %>%
  select(Regional_District,
         Year, 
         "Population" = `2023 Population`, 
         "Total_Disposed_Tonnes" = `2023 Total Disposal (Tonnes)`)

msw <- bind_rows(old_msw, data_2023)

## Combine Comox and Strathcona -----------------------------------------------

msw <- msw %>% 
  mutate(Regional_District = ifelse(grepl("Comox|Strathcona", Regional_District),
                                    "Comox-Strathcona", Regional_District)) %>%
  group_by(Regional_District, Year) %>%
  summarise(Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE),
            Population = sum(Population)) %>%
  arrange(Regional_District, Year) %>% 
  ## Remove Stikine
  filter(Regional_District != "Stikine")

# ----------------------------------------------------------------------------
## Calculate provincial totals and calculate per-capita disposal in kg, and fill in years

msw <- msw %>% 
  group_by(Year) %>%
  filter(n() > 25) %>% # Only calculate prov totals when more than 25 RDs reported
  summarise(Regional_District = "British Columbia",
            Population = sum(Population, na.rm = TRUE),
            Total_Disposed_Tonnes = sum(Total_Disposed_Tonnes, na.rm = TRUE)) %>%
  mutate(Total_Disposed_Tonnes = case_when (
    Year == 2023 ~ (Total_Disposed_Tonnes + 50000), # Adjust 2023 data by 50k. Note: there is a 50K tonnes of unclaimed waste (FV claimed it’s from MV) that has been added to the total but it’s not reflected in any RD’s tonnage.
    TRUE ~ Total_Disposed_Tonnes
  )) %>%
  bind_rows(msw, .) %>%
  ungroup() %>%
  mutate(Year = as.integer(Year),
         Population = as.integer(round(Population)),
         Disposal_Rate_kg = as.integer(round(Total_Disposed_Tonnes / Population * 1000))) %>%
  fill_years() %>%
  select(Regional_District, Year, Total_Disposed_Tonnes, Population, Disposal_Rate_kg)

msw %>%
  filter(Regional_District != "British Columbia") %>%
  ggplot(aes(x = Year, y = Disposal_Rate_kg)) +
  geom_point(aes(size = log10(Population))) +
  facet_wrap(~Regional_District)

# ----------------------------------------------------------------------------
## Join RD data such as Waste Management Plan links etc. for the visualization
reports.df <- read_csv('data/rd_report_links.csv')

## Format names and select columns, check links to MSW report pages
reports.df <- reports.df %>%
  rename(Regional_District = Local_Govt_Name) %>%
  select(Regional_District, swmPlan, wComposition) %>%
  mutate(swmPlan_check = check_url(swmPlan),
         wComposition_check = check_url(wComposition))

dir.create("out", showWarnings = FALSE)

## Remove BC totals for the DataBC version
msw %>%
  filter(Regional_District != "British Columbia") %>%
  write_csv(file = "out/BC_Municipal_Solid_Waste_Disposal.csv", na = "")

## Write out a file for use in d3 dataviz on the web.
msw %>% 
  left_join(reports.df, by = "Regional_District") %>% 
  select(-ends_with("_check")) %>%
  write_csv(file = "out/BC_Municipal_Solid_Waste_Disposal_viz.csv")
