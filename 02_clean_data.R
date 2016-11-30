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

provincial <- readRDS("output/load.rds")

# for now just work on provincial data
provincial %<>% semi_join(readRDS("output/provincial.rds"), by = c("Station", "Code"))

provincial %<>% mutate(Variable = lookup_variables(Code))

# group by station
provincial %<>% group_by(Station, Code)

# drop data time series with insufficient data (less than 10 values)
provincial %<>% filter(n() >= 10)

## identify outliers
provincial %<>% identify_outliers(by = c("Station", "Code"))

pdf("output/provincial_outliers.pdf")
plot_timeseries(provincial, by = c("Station", "Variable", "Code", "Units"))
dev.off()

## remove outliers and drop Outlier column
provincial %<>% filter(!Outlier) %>% select(-Outlier)

## clean
provincial %<>% clean_wqdata(by = c("Station"))

provincial %<>% as.tbl()

saveRDS(provincial, "output/provincial_clean.rds")

pdf("output/provincial_clean.pdf")
plot_timeseries(provincial, by = c("Station", "Variable", "Units"))
dev.off()
