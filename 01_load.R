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

source("header.R")

stations <- read_csv("data/BC_WQI_Appendix_2016.csv")

stations %<>% select(Station_Number = `Station Number`, 
                 Station_Name = `Station Name`, 
                 EMS_ID = `EMS ID`)

stations %<>% filter(!is.na(EMS_ID))

variables <- c("Arsenic Total", "Copper Total", "Cadmium Dissolved", "Lead", "Silver",
               "Zinc Total", "pH", "Chloride Total", "Hardness Total", "Mercury Methyl")
#DO, TP, NH3, E. coli

variables <- data_frame(Variables = variables)
variables$Codes <- lookup_codes(variables$Variables)

try(download_historic_data())
ems_historic <- read_historic_data(stations$EMS_ID)
ems_current <- get_ems_data()
ems_current %<>% filter_ems_data(stations$EMS_ID)

ems <- bind_rows(ems_current, ems_historic)

ems %<>% filter(PARAMETER_CODE %in% variables$Codes)
warning('no "Chloride Total", "Hardness Total" or "Mercury Methyl"')

ems %<>% inner_join(stations, by = "EMS_ID")

ems %<>% select(Station_Number, Date = COLLECTION_START,
                Value = RESULT, Units = UNIT, 
                DetectionLimit = METHOD_DETECTION_LIMIT, Code = PARAMETER_CODE, 
                EMS_ID, Station_Name, Latitude = LATITUDE, Longitude = LONGITUDE, QA_INDEX_CODE)

ems$Date %<>% date()

ems %<>% standardize_wqdata()

ems %<>% select(Station_Number, Date, Variable, everything())

ems %<>% as.tbl()

saveRDS(ems, "load.rds")
