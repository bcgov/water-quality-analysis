# Copyright 2017 Province of British Columbia
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

set_sub("wrangled")

load_object()

####### get water quality data from ems

download_historic_data(force = .force, ask = .ask)
ems_historic <- read_historic_data(stations$EMS_ID)
ems_current <- get_ems_data(force = .force, ask = .ask)

ems_current %<>% filter_ems_data(stations$EMS_ID)
ems <- bind_rows(ems_current, ems_historic)
rm(ems_current, ems_historic)

# add matching stations data to ems data 
ems %<>% inner_join(stations, by = c("EMS_ID"))

# ensure just variables of interest are included
ems %<>% filter(PARAMETER_CODE %in% unique(c(limits$Code, variables$Code)))


####### get water quality data from environment canada

locale <- locale(encoding = "latin1")

col_types <- cols(
  SITE_NO = col_character(),
  DATE_TIME_HEURE = col_character(),
  FLAG_MARQUEUR = col_character(),
  VALUE_VALEUR = col_double(),
  SDL_LDE = col_double(),
  MDL_LDM = col_double(),
  VMV_CODE = col_integer(),
  UNIT_UNITÉ = col_character(),
  VARIABLE = col_character(),
  VARIABLE_FR = col_character(),
  STATUS_STATUT = col_character()
)

data_ec <- list()

data_ec$fraser <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/fraser-river-long-term-water-quality-monitoring-data/Water-Qual-Eau-Fraser-2000-2015v1.csv", 
                           col_types = col_types, locale = locale)

data_ec$columbia <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/columbia-river-basin-long-term-water-quality-monitoring-data/Water-Qual-Eau-Columbia-2000-2015v1.csv", 
                             col_types = col_types, locale = locale)

data_ec$okanagan <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/okanagan-similkameen-river-basin-long-term-water-quality-monitoring-data/Water-Qual-Eau-Okanagan-Similkameen-2000-2015v1.csv", 
                             col_types = col_types, locale = locale)

data_ec$coastal <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/pacific-coastal-basin-long-term-water-quality-monitoring-data/Water-Qual-Eau-Pacific-Coastal-Cote-Pacifique-2000-2015v1.csv", 
                            col_types = col_types, locale = locale)

data_ec$peace <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/peace-athabasca-river-basin-long-term-water-quality-monitoring-data/Water-Qual-Eau-Peace-Athabasca-2000-2015v1.csv", 
                          col_types = col_types, locale = locale)

# combine list of data frames into a single data frame
data_ec %<>% bind_rows()

# select just those stations that require
data_ec %<>% semi_join(stations, by = c(SITE_NO = "Station"))

# read in variable values
variables_ec <- read_csv("http://donnees.ec.gc.ca/data/substances/monitor/national-long-term-water-quality-monitoring-data/Water-Qual-Eau-VariableInfo.csv", 
                         locale = locale)

set_sub("downloaded")
save_object()
