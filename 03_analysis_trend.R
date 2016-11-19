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
trend_test <- function(df, use.median = TRUE) {
  # convert to monthly time series
  df$Month <- month(df$Date)
  df$Year <- year(df$Date)
  # if there are less than four years do not run test
  if (length(unique(df$Year)) < 4) {
    return(NULL)
  }
  # aggregate to monthly observations
  # use the median monthly observation as default
  if (use.median) {
    func <- median
  } else {
    func <- mean
  }
  df_ts <- df %>% with(., tapply(Value, list(Year, Month), func)) %>% as.ts()
  # test for monotonic trend in the presence of seasonality
  SeasonalMannKendall(df_ts)
}


# nest data
nest_ems <- ems %>% nest(Date, Value, .key = Data)

# run test on each time series and extract the p-value
nest_ems %<>% mutate(Trend = map(Data, trend_test),
                    p_value = map(Trend, function(x) x$tau))

# produce summary table of results
ems_ptable <- unnest(nest_ems, p_value)


