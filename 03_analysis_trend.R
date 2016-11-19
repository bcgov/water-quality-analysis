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
ems <- readRDS("out/clean.rds")

## analyse trends for the 10-12 water quality parameters measured at 
## 14 Federal-Provincial water quality monitoring stations using the 
## Mann-Kendall Sen Slope analysis implemented in the 'zyp' 
## R package. 

# work on subset
data <- filter(ems, Variable == "Copper Total")
data <- ems

# nest
ne_data <- data %>% nest(Date, Value, .key = Data)

trend_test <- function(df) {
  # if there are less than three observations do not run test
  if (nrow(df) < 3) {
    return(NULL)
  }
  # convert to monthly time series
  df$Month <- month(df$Date)
  df$Year <- year(df$Date)
  df_ts <- df %>% with(., tapply(Value, list(Year, Month), mean)) %>% as.ts()
  # test for monotonic trend in the presence of seasonality
  SeasonalMannKendall(df_ts)
}

ne_data %<>% mutate(Trend = map(Data, trend_test),
                    p_value = map(Trend, function(x) x$tau))
unnest(ne_data, p_value)

