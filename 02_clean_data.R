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

set_sub("standardized")

load_object()

ems %<>% as.tbl()
ecd %<>% as.tbl()

ems %<>% clean_wqdata(by = c("Station"))
ecd %<>% clean_wqdata(by = c("Station"))

## plot time series
pdf("output/outliers_ems.pdf")
plot_timeseries(ems, by = c("Station", "Variable", "Units"), size = 2)
dev.off()

pdf("output/outliers_ecd.pdf")
plot_timeseries(ecd, by = c("Station", "Variable", "Units"))
dev.off()

## remove outliers and drop Outlier column
ems %<>% filter(!Outlier) %>% select(-Outlier)
ecd %<>% filter(!Outlier) %>% select(-Outlier)

set_sub("cleansed")

save_object()
