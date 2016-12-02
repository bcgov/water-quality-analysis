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

values <- readRDS("output/values_clean.rds")
limits <- readRDS("output/federal_station_variables_limits.rds")
stations <- readRDS("output/stations.rds")

values %<>% inner_join(limits, by = c("Station", "Variable"))

values %<>% mutate(Year = year(Date))

values %<>% calc_wqi(by = c("Station", "Year"))

plot_wqis(values, x = "Year") + facet_wrap(~Station)

ggsave("output/wqi_site_specific.png", width = 6, height = 8)
