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

set_sub("downloaded")

load_object()

ems %<>% tidy_rems_data()

# ensure units etc consistent
ems %<>% standardize_wqdata()

# convert to a tibble for nice printing
ems %<>% as.tbl()

# station <- select(ems, Station, EMS_ID, Station_Name, Provincial, Federal, Latitude, Longitude) %>%
#   unique()
# 
# ems %<>% select(-Variable, -EMS_ID, -Station_Name, -Latitude, -Longitude, -Provincial, -Federal)

# ensure out folder exists to save data
dir.create("output", showWarnings = FALSE)

saveRDS(ems, "output/values.rds")
saveRDS(station, "output/stations.rds")
saveRDS(limits, "output/federal_station_variables_limits.rds")
saveRDS(variables, "output/provincial_station_variables.rds")


values %<>% mutate(Variable = lookup_variables(Code))

saveRDS(values, "output/values.rds")
saveRDS(stations, "output/stations.rds")
