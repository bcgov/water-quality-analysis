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

# ensure required packages are loaded etc
source("header.R")

set_sub("input")

# load all objects in output/object/input to the workspace
load_object()

######## work up station limits ############
# necessary hack to tidy data!
limits %<>% rename(Station_Name = `Station Name`, Station = `Station Number`)
colnames <- colnames(limits)[c(1:2,seq(4,ncol(limits),by = 2))]
limits <- limits[3:nrow(limits),c(1:2,seq(3,ncol(limits) - 1,by = 2))]
colnames(limits) <- colnames
limits %<>% gather(Variable, Value, -Station_Name, -Station, na.rm = TRUE)
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
limits %<>% select(Station, Variable, LowerLimit, UpperLimit, Units)

# then map chemical names to specific variables
limits$Variable %<>% str_to_title() %>%
  str_replace("^Ph$", "pH") %>%
  str_replace("^Alkalinity$", "Alkalinity Total") %>%
  str_replace("^Arsenic$", "Arsenic Total") %>%
  str_replace("^Cyanide$", "Cyanide Weak Acid Dissociable") %>%
  str_replace("^Cadmium$", "Cadmium Dissolved") %>%
  str_replace("^Chloride$", "Chloride Total") %>%
  str_replace("^Chromium$", "Chromium Total") %>%
  str_replace("^Copper$", "Copper Total") %>%
  str_replace("^Dissolved Oxygen$", "Oxygen Dissolved") %>%
  str_replace("^Flouride$", "Fluoride Total") %>%
  str_replace("^Iron$", "Iron Total") %>%
  str_replace("^Molybdenum$", "Molybdenum Total") %>%
  str_replace("^Nitrate$", "Nitrate Total") %>%
  str_replace("^Nitrogen$", "Nitrogen Total") %>%
  str_replace("^Phosphorus$", "Phosphorus Total") %>%
  str_replace("^Selenium$", "Selenium Total") %>%
  str_replace("^Thallium$", "Thallium Total") %>%
  str_replace("^Uranium$", "Uranium Total") %>%
  str_replace("^Water Temperature$", "Temperature") %>%
  str_replace("^Zinc$", "Zinc Total")

# get codes that recognised in wqbc
limits$Code <- lookup_codes(limits$Variable)

limits$Code[limits$Variable == "Chromium"] <- "CR-T" 
limits$Code[limits$Variable == "Oxygen Dissolved"] <- "0014" 
limits$Code[limits$Variable == "Temperature"] <- "0013" 

# print head of limits which is station specific limits by variable
print(limits)
print(sort(unique(limits$Variable)))

############## work up station names ##############

# drop uninformative columns (formatting artefact)
stations %<>% filter(!is.na(`EMS ID`))
# drop station with missing data
stations %<>% filter(`Station Name` != "Peace River above Alces River")
stations %<>% mutate(Provincial = str_detect(`Core Contract Deliverables`, "Data Tidying [&] QC"),
                     StartYear = str_replace(`Station Data Time Series (complete calendar years)`, "(^\\d{4,4})(.*)", "\\1"),
                     EndYear = str_replace(`Station Data Time Series (complete calendar years)`, "(^\\d{4,4}-)(\\d{4,4})(.*)", "\\2"))

# rename and select specific columns from stations
stations %<>% select(Station = `Station Number`, 
                 Station_Name = `Station Name`, 
                 EMS_ID = `EMS ID`,
                 StartYear, EndYear, Provincial)

stations$Federal <- stations$Station %in% limits$Station


print(stations)

########## work up variables ###########

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

# removed because TriStar report states that
# We found that there were generally adequate data available for all stations except the Peace River at Alces.
variables %<>% filter(Station_Name != "Peace River above Alces River")

# check variables Station_Name values consistent with stations data
stopifnot(all(variables$Station_Name %in% stations$Station_Name))

# convert variables from wide to long format and sort by Station_Name and Variable
variables %<>% gather(Key, Variable, -Station_Name, na.rm = TRUE) %>% select(-Key) %>% arrange(Station_Name, Variable)

# throw away Variable that has yet to be collected
variables %<>% filter(!Variable %in% c("DO in future"))

# edit Variable so that full names for matching with Variables
variables$Variable %<>% 
  str_replace("^Ag$", "Silver Total") %>%
  str_replace("^Al [(]d[)]$", "Aluminium Dissolved") %>%
  str_replace("^As$", "Arsenic Total") %>%
  str_replace("^Ba$", "Barium Total") %>%
  str_replace("^Cd$", "Cadmium Dissolved") %>%
  str_replace("^CN$", "Cyanide Weak Acid Dissociable") %>%
  str_replace("^Cr$", "Chromium Total") %>%
  str_replace("^Cu$", "Copper Total") %>%
  str_replace("^Cu [(]d[)]$", "Copper Dissolved") %>%
  str_replace("^DO$", "Oxygen Dissolved") %>%
  str_replace("^E. (c|C)oli$", "E. coli") %>%
  str_replace("^F$", "Fluoride Total") %>%
  str_replace("^Mn$", "Manganese Total") %>%
  str_replace("^Mo$", "Molybdenum Total") %>%
  str_replace("^NH3$", "Ammonia Dissolved") %>%
  str_replace("^Ni$", "Nickel Total") %>%
  str_replace("^NO2$", "Nitrite Dissolved") %>%
  str_replace("^(NO2[/]NO3)|(NO2[+]NO3)$", "Nitrate Nitrite") %>%
  str_replace("^Pb$", "Lead Total") %>%
  str_replace("^Se$", "Selenium Total") %>%
  str_replace("^SO4$", "Sulphate Total") %>%
  str_replace("^TDN$", "Nitrogen Total Dissolved") %>%
  str_replace("^TDP$", "Phosphorus Dissolved") %>%
  str_replace("^Temp$", "Temperature") %>%
  str_replace("^Tl$", "Thallium Total") %>%
  str_replace("^TP$", "Phosphorus Total") %>%
  str_replace("^Zn$", "Zinc Total")

sort(unique(variables$Variable))
# ensure ph, hardness, chloride included if available
# as some limits depend on their values
extra_variables <- expand.grid(Station_Name = unique(variables$Station_Name), 
            Variable = c("Chloride Total", "Hardness Total", "pH"),
            stringsAsFactors = FALSE)
variables %<>% bind_rows(extra_variables) %>% unique() %>% arrange(Station_Name, Variable)
rm(extra_variables)

# just get those station variables that for stations of interest
variables %<>% inner_join(stations, by = "Station_Name")
variables %<>% select(Station, Variable)

# lookup codes from Variable names - Failed to substitute 'Colour', 'E. coli', 'Fecal coliforms', 'Temperature' and 'Turbidity'.
variables$Code <- lookup_codes(variables$Variable)

# discard those variables that are not recognised by wqbc
variables %<>% filter(!is.na(Code))

set_sub("wrangled")

# save all data frames in the workspace to output/wrangled/object
save_object()
