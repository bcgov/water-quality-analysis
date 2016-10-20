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

ems <- readRDS("out/clean.rds")
variables <- readRDS("out/variables.rds")
stations <- readRDS("out/stations.rds")

# ems %<>% calc_limits(by = "Station_Number")
# 
# ems %<>% inner_join(stations, by = c("Station_Number"))
# 
# ems1315 <- filter(ems, year(Date) %in% 2013:2015)
# ems1315 %<>% filter(Station_Name %in% variables$Station_Name)
# 
# ems1315 %<>% calc_wqi(by = "Station_Name")



