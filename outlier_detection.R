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

# This script contains functions for identifying a certain type of outlier
# 
# The method is based on Hampels Mean Absolute Deviation approach for time series
#
# The method is a combination of two teqniques:
#  1. remove large outliers when compared to the MAD of the whole time series
#  2. 
#     a. fit a reasonably flexible time series model to identify 
#        trend in the data using a robust regression approach by iteratively reweighting
#     b. delete observations with very large residuals from the robust fit
#     c. repeat this procedure until all outliers have been removed.
#

# try some rudimentary cleaning
outlier_removal_by <- function(wk, w_threshold=6, theshold=10, debug=FALSE) {

  # track how many points are being removed
  wk_orig <- wk
  ndata <- nrow(wk)  
  
  # do by mad - finds extreme values at head and tail of time series
  wk %<>% do(outlier_removal2_by(., threshold = theshold))
  ndata <- c(ndata, nrow(wk))
  
  if (ndata[1] - ndata[2] > 20 | ndata[2]/ndata[1] < 0.9) {
    # do not proceed! and return origional data
    warning(wk$Station_Number[1], " ", wk$Code[1], ": A large amount of data would be removed as outliers - please check!")
    return(wk_orig)
  }
    
  # then fit a time series model
  # monitor changes
  orig <- nrow(wk)
  change <- TRUE
  while(change) {
    # identify outliers and drop
    wk %<>% do(outlier_removal1_by(., w_threshold = w_threshold, debug = debug))
    ndata <- c(ndata, nrow(wk))
    change <- nrow(wk) < orig
    orig <- nrow(wk)
  }

  # assess the removals?
  
  wk
}


outlier_removal2_by <- function(wk, threshold = 10) {

  # if data is constant then no outliers
  if (nrow(wk) == 1 || sd(wk$Value) == 0) {
    return(wk)
  }
  
  if (mad(log(wk$Value)) == 0 && length(unique(wk$Value)) > 1) {
      # only warn if mad is 0 and values are not all the same
      # if values are all the same, then there are no outliers to remove
      # and setting madx = 0.2 has no consequence.
      message(wk$Station_Number[1], "-", wk$Code[1], ": Median absolute deviation is zero, increasing to 0.2.")
  }
    
  # first identify scale problems
  # work on the log scale as posotive observations tend to have constant CV behaviour
  mad_resids <- function(x) {
    x <- log(x)
    madx <- mad(x)
    if (madx == 0) {
      madx <- 0.2
    } 
    (x - median(x))/madx
  }
  wk$Value %>% mad_resids
  # what threshold to choose?
  # 10 on the log scale?
  wk %<>% filter(mad_resids(Value) < threshold)
  
  # return cleaned data
  wk
}


outlier_removal1_by <- function(wk,   w_threshold=5, debug=FALSE) {

  # if data is constant then no outliers
  if (nrow(wk) == 1 || sd(wk$Value) == 0) {
    return(wk)
  }
  
  if (debug) {
    cat("doing Station", wk$Station_Number[1], "Code", wk$Code[1], "\n")
  }
  
  # re-weighting function
  w <- function(x, k) {
    ifelse(abs(x) <= k, 1, k / abs(x))
  }
  
  # residual function
  w_resid <- function (mod) {
    # uing the prediction standard deviation
    # means we keep observations that out on thier own...
    sd.y <- predict(mod, se.fit = TRUE)$se.fit + sqrt(mod$sig2)
    res <- (mod$y - fitted(mod)) / sd.y
    res
  }
  
  # select a transformation
  # note - log transform runs into trouble due to
  # bimdodal natre of some data - very small vs positive
  trans <- function(x) x^.5
  wk$.Value <- wk$Value
  if (any(wk$.Value == 0)) {
    wk$.Value[wk$Value == 0] <- min(wk$Value[wk$Value>0])/2
  }
  
  # initialise weights
  weights <- rep(1, nrow(wk))
  
  # convergence criteria
  change <- 1
  iterations <- 0
  change_thresh <- 0.001
  iter_max <- 50
  
  # setup model
  ndays <- wk$Date %>% year %>% table
  nyears <- wk$Date %>% year %>% table %>% length
  nseasonal <- floor(mean(ndays))
  nunique <- length(unique(wk$Value))
  
  # if too few data, then this method won't work
  if (all(ndays < 9)) {
    message(wk$Station_Number[1], "-", wk$Code[1], ": Too few data data to find outliers")
    return(wk)
  }
  
  # choose model
  ns <- min(6, floor(nseasonal/2))
  nt <- min(9, ceiling(nyears/2)+1)
  formula <- 
    if (nyears == 1) {
      sprintf("trans(.Value) ~ s(decimal_date(Date), m=1, k=%i)", ns)
    } else if (nyears == 2) {
      sprintf("trans(.Value) ~ s(yday(Date), bs='cc', m=1, k=%i) + decimal_date(Date)", ns)
    } else {
      sprintf("trans(.Value) ~ s(yday(Date), bs='cc', m=1, k=%i) + s(decimal_date(Date), m=1, k=%i)", ns, nt)
    }
  formula <- as.formula(formula)
  
  # redo until weights stabalise
  # or too many iterations
  while (change > change_thresh && iterations < iter_max) {
    # fit a timeseries model
    mod <- mgcv::gam(formula, 
                     data = wk,
                     weights = weights)
    res <- w_resid(mod)
    # calculate re-weighting
    # weight using residuals with a cut off at threshold
    old_weights <- weights
    weights <- w(res, w_threshold)
    change <- sum(abs(weights - old_weights))
  }
  
  # now drop residuals larger than w_threshold
  # we could set a differnt value here ...
  out <- wk %>% filter(res < w_threshold) %>% select(-.Value)
  
  # return cleaned data
  out
}


if (FALSE) {
  plot(mod, pages = 1, all = TRUE)
  xyplot(trans(Value) ~ Date, wk)
  xyplot(res ~ Date, wk)
  range(res)
  range(weights)
}
