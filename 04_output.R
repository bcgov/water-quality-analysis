# Copyright 2018 Province of British Columbia
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

library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(envreportutils)
bc_nested <- readRDS("tmp/bc_nested_results.rds")
bc_stations <- readRDS("data/bc_stations.rds")
categories <- sort(unique(bc_nested$Category))

sites <- unique(bc_nested$SITE_NO)

breaker <- function(x) seq(round(x[1]), round(x[2]), by = 0.5)
labeller <- function(x) format(x, drop0trailing = TRUE)

data <- bc_nested %>% filter(SITE_NO == sites[1], sig)

site_name <- to_titlecase(bc_stations$SITE_NAME[bc_stations$SITE_NO == data$SITE_NO[1]])

(
  p <- ggplot(data, aes(x = PARAM_SHORT_NAME, y = ann_percent_change, 
                        fill = Category)) + 
    scale_fill_ipsum(breaks = rev(categories)) + 
    geom_col() +
    scale_y_continuous(breaks = breaker, 
                       minor_breaks = NULL, 
                       labels = labeller) + 
    coord_flip() + 
    labs(title = site_name,
         subtitle = data$SITE_NO[1], 
         x = "Parameter", 
         y = "Average Annual Percent Change") + 
    theme_ipsum()
)

