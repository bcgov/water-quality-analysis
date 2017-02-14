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

set_sub("wqi")

soe %<>% mutate(Year = year(soe$Date)) %>% filter(Year %in% 2013:2015)

soe %<>% calc_wqi(by = c("Station_Name", "Station"))

open_window(width = 6, height = 6)
plot_wqis(soe, x = "Station_Name") + xlab("Station Name") +   
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

save_plot("soe", caption = "The Water Quality Indices for the SoE stations based on daily long-term upper limits.")

soe$Station <- NULL
soe$Category <- NULL
save_table(soe, caption = "The Water Quality Index table for the SoE stations based on daily long-term upper limits.")

