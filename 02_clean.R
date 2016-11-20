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

# we need to eliminate clearly erroneous data before calling clean_wqdata.
# we should also entirely eliminate variables at a site with insufficient data.
# note we should not eliminate ph, hardness or chloride as required for limits on other variables.
# Joe needs to check that detection limits are correct


ems %<>% dlply(c("Station_Number", "Code"))

# drop data with less than 10 rows
ems <- ems[vapply(ems, function(x) nrow(x) >= 10, TRUE)]

# drop data with more than 10% outliers
ems <- ems[vapply(ems, function(x) sum(x$is_outlier) / nrow(x) <= 0.1, TRUE)]

ems %<>% ldply()

## identify outliers
ems %<>% outlier_id(by = c("Station_Number", "Code"))

ems$Variable <- lookup_variables(ems$Code)

pdf()
plot_timeseries_by(ems, by = c("Station_Number", "Variable", "Code", "Units"), color = "is_outlier")
dev.off()


# convert data to list of data frames by station number and code




# remove problematic time series to short or too many identical?
ems %<>% filter(!Station_Number == "BC08HB0018" & Code == "PB???T")

## remove outliers and drop is_outler column
ems %<>% filter(!is_outlier) %>% select(-is_outlier)



## clean
ems %<>% clean_wqdata(by = c("Station_Number"))

ems %<>% as.tbl()

saveRDS(ems, "out/clean.rds")

