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

# ensure required packages are loaded (in correct order)
library(lubridate)
library(magrittr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(subfoldr)
library(yesno)

library(plyr)
library(dplyr)

library(rems)
library(wqbc)

# clear up workspace
rm(list = ls())

source("functions.R")

options(subfoldr.ask = FALSE)

.force <- TRUE
.ask <- TRUE

# because Joe has already forced new downloads and doesn't need to be asked
if (user() == "joe") {
  .force <- FALSE
  .ask <- FALSE
}

# ensure output folder exists
if (!dir.exists("output")) {
  if (!.ask || yesno("Create folder 'output' in working directory to store output?")) {
   dir.create("output") 
  }
}


