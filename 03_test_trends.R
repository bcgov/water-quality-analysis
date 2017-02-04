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
library(cccharts)

set_sub("cleansed")

load_object()

# just work on soe stations and variables
ecd %<>% inner_join(select(soe, Station, Variable), by = c("Station", "Variable"))

# drop variables which highly variable
ecd %<>% filter(!Variable %in% c("Temperature", "Colour True", "Oxygen Dissolved"))

# only consider period 2003 to 2015 due to changes in procedures
ecd %<>% filter(year(Date) >= 2003 & year(Date) <= 2015)

# set undetected valuse to half detection limit
undetected <- ecd$Value == 0 & !is.na(ecd$DetectionLimit) & ecd$DetectionLimit > 0
ecd$Value[undetected] <- ecd$DetectionLimit[undetected] / 2

# prewhiten zhang
trends <- test_trends(ecd, prewhiten = "zhang")

trends %<>% filter(!is.na(sen_slope))

# plot overview of trend significance and direction
trends$Significant <- trends$sen_slope_sig
trends$Direction <- factor("Stable", levels = c("Decreasing", "Stable", "Increasing"))
trends$Direction[trends$sen_slope < 0] <- "Decreasing"
trends$Direction[trends$sen_slope > 0] <- "Increasing"

trends %<>% inner_join(stations, by = "Station")

gp <- ggplot(data = trends, aes(x = Station_Name, y = Variable)) +
  geom_point(aes(shape = Direction, alpha = Significant)) +
  scale_shape_manual(values = c(25,21,24)) +
  scale_alpha_manual(values = c(1/4,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(gp)

set_sub("trends")

save_plot("trends", width = 6, height = 6)

trends %<>% filter(Significant)

# get raw data used for analysis
ecd %<>% summarise_for_trends()

# add raw data and slope values
trends %<>% inner_join(ecd, by = c("Station", "Variable", "Units", "Month"))

# plot data and slopes
trends %<>% nest(-Station_Name, -Variable, -Units, -Month)

plot_data <- function(data) {
  intercept <- data$sen_intercept[1]
  slope <- data$sen_slope[1]
  
  intercept <- intercept - slope * min(data$Year)
  
  gp <- ggplot(data = data, aes(x = Year, y = Value)) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope) +
    expand_limits(y = 0)
  gp
}

trends %<>% mutate(Plot = map(data, plot_data))

# save to pdf
pdf("output/trends.pdf")
for (i in 1:nrow(trends))
  print(trends$Plot[[i]] + ggtitle(trends$Station_Name[i]) + ylab(str_c(trends$Variable[i], " (", trends$Units[i],")")))
dev.off()
