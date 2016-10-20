# Copyright 2016 Province of British Columbia
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

# load, tidy and save required data

# ensure required packages are loaded
source("header.R")

# read in data
# stations is list of stations for analysis
# variables is list of stations and variables for 2013-2015 analysis
stations <- read_csv("meta/BC_WQI_Appendix_2016.csv")
variables <- read_csv("meta/variables-by-station.csv")

# rename and select specific columns from stations
stations %<>% select(Station_Number = `Station Number`, 
                 Station_Name = `Station Name`, 
                 EMS_ID = `EMS ID`)

# drop uninformative columns (formatting artefact)
stations %<>% filter(!is.na(EMS_ID))

# rename Water Body as Station_Name and fill in missing values
variables %<>% rename(Station_Name = `Water Body`) %>% fill(Station_Name)

# edit Station_Name using regular expressions so consistent with Station_Name
# values in stations data
variables$Station_Name %<>%
  str_replace(" R ", " River ") %>%
  str_replace(" R$", " River") %>%
  str_replace(" L$", " Lake") %>%
  str_replace("U[/]S", "upstream of") %>%
  str_replace("^Chilcotin River$", "Chilcotin River upstream of Christie Road Bridge") %>%
  str_replace("^Dean River$", "Dean River upstream of Lodge Creek") %>%
  str_replace("^Horsefly River$", "Horsefly River above Quesnel lake") %>%
  str_replace("^Iskut River$", "Iskut River below Johnson") %>%
  str_replace("^Nicola River near Thompson River$", "Nicola River near mouth at Thompson River") %>%
  str_replace("^North Alouette River$", "North Alouette River at 132nd and Edge Street") %>%
  str_replace("^Tsolum River$", "Tsolum River below Murex Creek")

# check variables Station_Name values consistent with stations data
stopifnot(all(variables$Station_Name %in% stations$Station_Name))

# convert variables from wide to long format and sort by Station_Name and Variable
variables %<>% gather(Key, Variable, -Station_Name) %>% select(-Key) %>% arrange(Station_Name, Variable)

# throw away Variable that has yet to be collected
variables %<>% filter(!Variable %in% c("DO in future"))

# edit Variable so that full names for matching with Variables in wqbc
variables$Variable %<>% 
  str_replace("^Ag$", "Silver") %>%
  str_replace("^Al [(]d[)]$", "Aluminium Dissolved") %>%
  str_replace("^As$", "Arsenic Total") %>%
  str_replace("^Ba$", "Barium") %>%
  str_replace("^Cd$", "Cadmium Dissolved") %>%
  str_replace("^CN$", "Cyanide") %>%
  str_replace("^Cr$", "Chromium") %>%
  str_replace("^Cu$", "Copper Total") %>%
  str_replace("^Cu [(]d[)]$", "Copper Dissolved") %>%
  str_replace("^DO$", "Oxygen Dissolved") %>%
  str_replace("^E. (c|C)oli$", "E. coli") %>%
  str_replace("^F$", "Fluoride Total") %>%
  str_replace("^Mn$", "Manganese") %>%
  str_replace("^Mo$", "Molybdenum Total") %>%
  str_replace("^NH3$", "Ammonia") %>%
  str_replace("^Ni$", "Nickel") %>%
  str_replace("^NO2$", "Nitrite") %>%
  str_replace("^Pb$", "Lead") %>%
  str_replace("^Se$", "Selenium Total") %>%
  str_replace("^SO4$", "Sulphate") %>%
  str_replace("^TDN$", "Nitrogen Total Dissolved") %>%
  str_replace("^TDP$", "Phosphorus Total Dissolved") %>%
  str_replace("^Temp$", "Temperature") %>%
  str_replace("^Tl$", "Thallium") %>%
  str_replace("^TP$", "Phosphorus Total") %>%
  str_replace("^Pb$", "Lead") %>%
  str_replace("^Zn$", "Zinc Total")

# ensure ph, hardness, chloride and methyl mercury included if available
# as some limits depend on their values
extra_variables <- expand.grid(Station_Name = unique(variables$Station_Name), 
            Variables = c("Chloride Total", "Hardness Total", "Mercury Methyl", "pH"),
            stringsAsFactors = FALSE)
variables %<>% bind_rows(extra_variables) %>% unique()

# lookup codes from Variable names 
variables$Code <- lookup_codes(variables$Variable)

# throw away those variables that are not recognised by wqbc
variables %<>% filter(!is.na(Code))

# get ems data using rems functions
try(download_historic_data())
ems_historic <- read_historic_data(stations$EMS_ID)
ems_current <- get_ems_data()
ems_current %<>% filter_ems_data(stations$EMS_ID)
ems <- bind_rows(ems_current, ems_historic)

# add matching stations data to ems data 
ems %<>% inner_join(stations, by = c("EMS_ID"))

# renames and select specific ems columns
ems %<>% select(Station_Number, Date = COLLECTION_START, Variable = PARAMETER,
                Value = RESULT, Units = UNIT, 
                DetectionLimit = METHOD_DETECTION_LIMIT, Code = PARAMETER_CODE, 
                EMS_ID, Station_Name, Latitude = LATITUDE, Longitude = LONGITUDE, 
                QA_INDEX_CODE)

# convert ems$Date from POSIX to Date
ems$Date %<>% date()

# ensure just variables of interest are included
ems %<>% filter(Code %in% variables$Code)

# ensure units etc consistent
ems %<>% standardize_wqdata()

# convert to a tibble for nice printing
ems %<>% as.tbl()

# ensure out folder exists to save data
dir.create("out", showWarnings = FALSE)

# save data to out folder so only need to run this script once
saveRDS(ems, "out/load.rds")
saveRDS(stations, "out/stations.rds")
saveRDS(variables, "out/variables.rds")
