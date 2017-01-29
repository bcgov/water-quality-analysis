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

# load, tidy and save required data

# ensure required packages are loaded etc
source("header.R")

######## read in data ########
# stations is list of stations for analysis
# limits is federal stations and variable limits
limits <- read_csv("input/2015-16 CESI Parameters and Guideline_BC.csv")
# stations is list of all stations of interest
stations <- read_csv("input/BC_WQI_Appendix_2016.csv")
# variables is list of provincial stations and variables to look at
variables <- read_csv("input/variables-by-station.csv")

# set subfolder in output to input
set_sub("input")

# save all three data frames in the workspace to output/object/input
save_object()
