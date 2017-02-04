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

set_sub("limits")

load_object()

soe %<>% calc_wqi(by = c("Station_Name", "Station", "Year"))

cesi %<>% calc_wqi(by = c("Station_Name", "Station", "Year"))

set_sub("wqi")

plot_wqis(cesi, x = "Year") + facet_wrap(~Station_Name)

save_plot("cesi")
