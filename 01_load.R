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
# get ems data using rems functions
limits <- read_csv("meta/2015-16 CESI Parameters and Guideline_BC.csv")
stations <- read_csv("meta/BC_WQI_Appendix_2016.csv")
variables <- read_csv("meta/variables-by-station.csv")

# necessary hack to tidy data!
limits %<>% rename(Station_Name = `Station Name`, Station_Number = `Station Number`)
colnames <- colnames(limits)[c(1:2,seq(4,ncol(limits),by = 2))]
limits <- limits[3:nrow(limits),c(1:2,seq(3,ncol(limits) - 1,by = 2))]
colnames(limits) <- colnames
limits %<>% gather(Variable, Value, -Station_Name, -Station_Number, na.rm = TRUE)
limits %<>% filter(Value != "formula")
limits$Units <- limits$Value
limits$Units %<>% str_replace("(.*)((m|u)g[/]L)$", "\\2") %>%
  str_replace("(.*)(C)$", "\\2")
limits$Units[limits$Station_Name == "Thompson River at Spences Bridge" & limits$Variable == "chromium"] <- "ug/L"
limits$Units[limits$Station_Name == "Kootenay River at Creston" & limits$Variable == "phosphorus"] <- "mg/L"
limits$Units[limits$Station_Name == "Kettle River at Carson" & limits$Variable == "flouride"] <- "mg/L"
limits$Units[limits$Station_Name %in% c("Cowichan River 1 km downstream of Somenos Creek", "Nechako River at Prince George") & limits$Variable == "dissolved oxygen"] <- "mg/L"
limits$Units[limits$Variable == "pH"] <- "pH"
limits$Value %<>% str_replace_all("[:alpha:]|[/]|\\s","") %>%
  str_replace("[,]", "-")
limits$LowerLimit <- str_replace_all(limits$Value, "(.*)([-].*)", "\\1") 
limits$UpperLimit <- str_replace_all(limits$Value, "(.*[-])(.*)", "\\2") 
is.na(limits$LowerLimit[limits$LowerLimit == limits$UpperLimit]) <- TRUE
limits$LowerLimit %<>% as.numeric()
limits$UpperLimit %<>% as.numeric()
limits %<>% select(Station_Number, Variable, LowerLimit, UpperLimit, Units)

warning("the resolution of variable names from station limits table needs checking")
limits$Variable %<>% str_to_title() %>%
  str_replace("^Ph$", "pH") %>%
  str_replace("^Arsenic$", "Arsenic Total") %>%
  str_replace("^Cadmium$", "Cadmium Dissolved") %>%
  str_replace("^Chloride$", "Chloride Total") %>%
  str_replace("^Copper$", "Copper Total") %>%
  str_replace("^Dissolved Oxygen$", "Oxygen Dissolved") %>%
  str_replace("^Flouride$", "Fluoride Total") %>%
  str_replace("^Iron$", "Iron Total") %>%
  str_replace("^Molybdenum$", "Molybdenum Total") %>%
  str_replace("^Nitrate$", "Nitrate Total") %>%
  str_replace("^Phosphorus$", "Phosphorus Total") %>%
  str_replace("^Selenium$", "Selenium Total") %>%
  str_replace("^Water Temperature$", "Temperature") %>%
  str_replace("^Zinc$", "Zinc Total")
  
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

# removed because TriStar report states that
# We found that there were generally adequate data available for all stations except the Peace River at Alces.
variables %<>% filter(Station_Name != "Peace River above Alces River")

# convert variables from wide to long format and sort by Station_Name and Variable
variables %<>% gather(Key, Variable, -Station_Name, na.rm = TRUE) %>% select(-Key) %>% arrange(Station_Name, Variable)

# throw away Variable that has yet to be collected
variables %<>% filter(!Variable %in% c("DO in future"))

# edit Variable so that full names for matching with Variables
warning("the resolution of variable names from station variables table needs checking")
variables$Variable %<>% 
  str_replace("^Ag$", "Silver") %>%
  str_replace("^Al [(]d[)]$", "Aluminium Dissolved") %>%
  str_replace("^As$", "Arsenic Total") %>%
  str_replace("^Ba$", "Barium") %>%
  str_replace("^Cd$", "Cadmium Dissolved") %>%
  str_replace("^CN$", "Cyanide Weak Acid Dissociable") %>%
  str_replace("^Cr$", "Chromium") %>%
  str_replace("^Cu$", "Copper Total") %>%
  str_replace("^Cu [(]d[)]$", "Copper Dissolved") %>%
  str_replace("^DO$", "Oxygen Dissolved") %>%
  str_replace("^E. (c|C)oli$", "E. coli") %>%
  str_replace("^F$", "Fluoride Total") %>%
  str_replace("^Mn$", "Manganese") %>%
  str_replace("^Mo$", "Molybdenum Total") %>%
  str_replace("^NH3$", "Ammonia") %>%
  str_replace("^Ni$", "Nickel Total") %>%
  str_replace("^NO2$", "Nitrite") %>%
  str_replace("^Pb$", "Lead") %>%
  str_replace("^Se$", "Selenium Total") %>%
  str_replace("^SO4$", "Sulphate") %>%
  str_replace("^TDN$", "Nitrogen Total Dissolved") %>%
  str_replace("^TDP$", "Phosphorus Dissolved") %>%
  str_replace("^Temp$", "Temperature") %>%
  str_replace("^Tl$", "Thallium Total") %>%
  str_replace("^TP$", "Phosphorus Total") %>%
  str_replace("^Pb$", "Lead") %>%
  str_replace("^Zn$", "Zinc Total")

sort(unique(variables$Variable))
# ensure ph, hardness, chloride included if available
# as some limits depend on their values
extra_variables <- expand.grid(Station_Name = unique(variables$Station_Name), 
            Variable = c("Chloride Total", "Hardness Total", "pH"),
            stringsAsFactors = FALSE)
variables %<>% bind_rows(extra_variables) %>% unique() %>% arrange(Station_Name, Variable)
rm(extra_variables)

variables %<>% inner_join(stations, by = "Station_Name")
variables %<>% select(Station_Number, Variable)

warning("almost non overlaps between limits and stations and variables")
variables %<>% left_join(limits, by = c("Station_Number", "Variable"))
rm(limits)

warning("a bunch of variables not in wqbc (and without limits)")
# lookup codes from Variable names 
variables$Code <- lookup_codes(variables$Variable)

# throw away those variables that are not recognised by wqbc
variables %<>% filter(!is.na(Code))

try(download_historic_data())
ems_historic <- read_historic_data(stations$EMS_ID)
ems_current <- get_ems_data()

#print out all ems codes
#ems_codes <- select(ems, Variable = PARAMETER, Code = PARAMETER_CODE) %>% unique() %>% arrange(Variable) %>% filter(!is.na(Variable)) %>% print()
#print(ems_codes$Variable)
#
ems_current %<>% filter_ems_data(stations$EMS_ID)
ems <- bind_rows(ems_current, ems_historic)
rm(ems_current, ems_historic)

# add matching stations data to ems data 
ems %<>% inner_join(stations, by = c("EMS_ID"))
rm(stations)

# renames and select specific ems columns
ems %<>% select(Station_Number, Date = COLLECTION_START, Code = PARAMETER_CODE,  
                Value = RESULT, Units = UNIT, 
                DetectionLimit = METHOD_DETECTION_LIMIT, Variable = PARAMETER, 
                EMS_ID, Station_Name, Latitude = LATITUDE, Longitude = LONGITUDE, 
                QA_INDEX_CODE)

warning("not passing upper and lower limits because very few defined")
# ensure just variables of interest are included
ems %<>% semi_join(variables, by = c("Code"))

# keep if station not in variables - otherwise only keep if codes match station codes
ems %<>% filter(!Station_Number %in% variables$Station_Number | 
                  str_c(Station_Number, Code) %in% str_c(variables$Station_Number, variables$Code))
rm(variables)

# convert ems$Date from POSIX to Date
ems$Date %<>% date()

# ensure units etc consistent
warning("need to check if detection limits are adjusted")
ems %<>% standardize_wqdata()

# convert to a tibble for nice printing
ems %<>% as.tbl()

# ensure out folder exists to save data
dir.create("out", showWarnings = FALSE)

station <- select(ems, Station = Station_Number, EMS_ID, Station_Name, Latitude, Longitude) %>%
  unique()

variable <- select(ems, Code, Variable) %>% unique()

ems %<>% rename(Station = Station_Number) %>% select(-Variable, -EMS_ID, -Station_Name, -Latitude, -Longitude)

# save data to out folder so only need to run this script once
saveRDS(ems, "out/load.rds")
saveRDS(station, "out/station.rds")
saveRDS(station, "out/variable.rds")
