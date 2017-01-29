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

source("00_install_packages.R")

source("01_input_data.R")
source("01b_wrangle_inputs.R")
source("01c_download_data.R", encoding = "latin1")
source("01d_standardize_data.R")
source("02_clean_data.R")
source("03_test_trends.R")
source("04_calc_limits.R")
source("05_calc_wqi.R")
source("06_calc_wqi_site_specific.R")
