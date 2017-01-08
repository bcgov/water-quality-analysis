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

# ensure required packages are installed from CRAN and GitHub
# this script only needs to be run once
install_packages <- function(pkgs) {
  stopifnot(is.character(pkgs))
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg) 
    }
  }
}

install_packages("lubridate") 
install_packages("magrittr") 
install_packages("readr") 
install_packages("stringr") 
install_packages("tidyr") 
install_packages("purrr") 
install_packages("ggplot2") 

install_packages("plyr") 
install_packages("dplyr") 

install_packages("devtools") 

devtools::install_github("bcgov/envreportutils")
devtools::install_github("bcgov/bcmaps")
devtools::install_github("bcgov/rems")
devtools::install_github("bcgov/wqbc@outlier_detection")
