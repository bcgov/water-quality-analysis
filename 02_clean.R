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

# remove outliers
ems <- ems %>% group_by(Code, Station_Number) %>% do(outlier_removal_by(.,debug=TRUE)) %>% ungroup

#
ems %<>% clean_wqdata(by = c("Station"))

ems %<>% as.tbl()

saveRDS(ems, "out/clean.rds")

