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

values <- readRDS("output/values.rds")

# filter those with less than 10 values
values %<>% group_by(Station, Code) %>% filter(n() >= 10)

## identify outliers
values %<>% identify_outliers(by = c("Station", "Code"), sds = 6)

pdf("output/outliers.pdf")
plot_timeseries(values, by = c("Station", "Variable", "Code", "Units"))
dev.off()

## remove outliers and drop Outlier column
values %<>% filter(!Outlier) %>% select(-Outlier)

## clean
values %<>% clean_wqdata(by = c("Station"))

values %<>% as.tbl()

saveRDS(values, "output/values_clean.rds")

pdf("output/values_clean.pdf")
plot_timeseries(values, by = c("Station", "Variable", "Units"))
dev.off()
