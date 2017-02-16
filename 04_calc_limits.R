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

set_sub("cleansed")

load_object()

soe %<>% inner_join(ecd, by = c("Station", "Variable")) %>%
  inner_join(select(stations, Station, Station_Name), by = "Station")

soe %<>% mutate(Year = year(Date),
                Dayte = Date)

year(soe$Dayte) <- 1972

values <- wqbc:::estimate_variable_values(soe, by = c("Station"))

open_window(8,8)

gp <- ggplot(data = filter(soe, Variable == "Hardness Total"), aes(x = Dayte, y = Value, group = Year, color = Year)) +
  facet_wrap(~Station, scales = "free_y") + 
  geom_point() +
  geom_line(data = filter(values, Variable == "Hardness Total")) +
  scale_x_date(date_labels = "%b") +
  scale_y_continuous(name = "Hardness Total")

print(gp)

set_sub("seasonal")

save_plot("Hardness Total", report = FALSE)

open_window(8,8)

gp <- ggplot(data = filter(soe, Variable == "pH"), aes(x = Dayte, y = Value, group = Year, color = Year)) +
  facet_wrap(~Station, scales = "free_y") + 
  geom_point() +
  geom_line(data = filter(values, Variable == "pH")) +
  scale_x_date(date_labels = "%b") +
  scale_y_continuous(name = "pH")

print(gp)

save_plot("pH", report = FALSE)

soe %<>% calc_limits(by = c("Station", "Station_Name"), term = "long-daily", estimate_variables = TRUE)

set_sub("limits")

save_object()
