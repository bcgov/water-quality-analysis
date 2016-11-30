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

ems <- readRDS("out/load.rds")
provincial <- readRDS("out/provincial.rds")

# for now just work on provincial data
ems %<>% semi_join(provincial, by = c("Station_Number", "Code"))

ems$Variable <- lookup_variables(ems$Code)

# group by station
ems %<>% group_by(Station_Number, Code)

# drop data time series with insufficient data (less than 10 values)
ems %<>% filter(n() >= 10)

## identify outliers
ems %<>% identify_outliers(by = c("Station_Number", "Code"))

pdf("ems_outliers.pdf")
plot_timeseries(ems, by = c("Station_Number", "Variable", "Code", "Units"))
dev.off()

## remove outliers and drop Outlier column
ems %<>% filter(!Outlier) %>% select(-Outlier)

## clean
ems %<>% clean_wqdata(by = c("Station_Number"))

ems %<>% as.tbl()

saveRDS(ems, "out/clean.rds")

pdf("ems_clean.pdf")
plot_timeseries(ems, by = c("Station_Number", "Variable", "Units"))
dev.off()
