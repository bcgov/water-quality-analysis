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

# load cleaned data
values <- readRDS("output/values_clean.rds")
# load stations
stations <- readRDS("output/stations.rds")

provincial <- readRDS("output/provincial_station_variables.rds")
federal <- readRDS("output/federal_station_variables_limits.rds")

trends <- test_trends(values)

trends %<>% filter(Years >= 4)

trends %<>% mutate(Significant = Significance < 0.05)

trends %<>% inner_join(stations, by = "Station")

provincial %<>% inner_join(trends, by = c("Station", "Variable"))
federal %<>% inner_join(trends, by = c("Station", "Variable"))

ggplot(data = provincial, aes(x = EMS_ID, y = Tau)) +
  facet_wrap(~Variable) +
  geom_hline(yintercept = 0) +
  geom_point(aes(alpha = Significant, size = Years)) +
  scale_alpha_discrete(range = c(1/3,1), drop = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("output/trends_provincial.png", width = 8, height = 8)

ggplot(data = federal, aes(x = EMS_ID, y = Tau)) +
  facet_wrap(~Variable) +
  geom_hline(yintercept = 0) +
  geom_point(aes(alpha = Significant, size = Years)) +
  scale_alpha_discrete(range = c(1/3,1), drop = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("output/trends_federal.png", width = 8, height = 8)
