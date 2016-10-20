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

library(rems)
library(wqbc)
library(magrittr)
library(dplyr)

station_ids <- NULL # c("0920125", "E237496")
parameter_codes <- NULL # lookup_codes(c("Aluminium Dissolved", "Arsenic Total", "Cadmium Dissolved"))

try(download_historic_data())
ems_historic <- read_historic_data(station_ids, parameter_codes)
ems_current <- get_ems_data()
ems_current %<>% filter_ems_data() #station_ids, parameter_codes)

ems <- bind_rows(ems_current, ems_historic)

saveRDS(ems, "load.rds")
