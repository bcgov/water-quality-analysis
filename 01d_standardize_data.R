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

set_sub("downloaded")

load_object()

# tidy up data
ems %<>% tidy_ems_data()

ecd %<>% tidy_ec_data()

# ensure just known codes included
# get lookup table of EMS and EC codes
codes <- mutate(wqbc::codes, Code = compress_ems_codes(Code))

codes %<>% filter(Code %in% unique(c(limits$Code, variables$Code)))

# ensure just variables of interest are included
ems %<>% filter(Code %in% codes$Code)

ecd %<>% inner_join(select(wqbc::vmv_codes, -Variable), by = c(Code = "VMV_Code"))

ecd %<>% filter(EC_Code %in% codes$EC_Code) %>% select(-Code, -Variable)

ecd %<>% inner_join(select(codes, Code, EC_Code), by = "EC_Code")

ecd %<>% select(-EC_Code)

# ensure units etc consistent
ems %<>% standardize_wqdata()
ecd %<>% standardize_wqdata()

set_sub("standardized")

save_object()
