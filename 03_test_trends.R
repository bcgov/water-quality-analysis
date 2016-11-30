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
provincial <- readRDS("output/provincial_clean.rds")

provincial %<>% test_trends()

provincial %<>% filter(Years >= 4)

provincial %<>% mutate(Significant = Significance < 0.05)

ggplot(data = provincial, aes(x = Station, y = Tau)) +
  facet_wrap(~Variable) +
  geom_hline(yintercept = 0) +
  geom_point(aes(alpha = Significant, size = Years)) +
  scale_alpha_discrete(range = c(1/3,1), drop = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("output/trends.png", width = 8, height = 8)
