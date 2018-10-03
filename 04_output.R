# Copyright 2018 Province of British Columbia
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

library(tidyverse)
library(hrbrthemes)
library(envreportutils)
library(leaflet)
library(mapview)

bc_nested <- readRDS("tmp/bc_nested_results.rds")
bc_stations <- readRDS("data/bc_stations.rds")
categories <- sort(unique(bc_nested$Category))

sites <- unique(bc_nested$SITE_NO)

breaker <- function(x) seq(round(x[1]), round(x[2]), by = 1)
labeller <- function(x) format(x, drop0trailing = TRUE)

sum_plot <- function(data, title, subtitle) {
  data <- filter(data, sig)
  site_name <- to_titlecase(bc_stations$SITE_NAME[bc_stations$SITE_NO == data$SITE_NO[1]])
  
  ggplot(data, aes(x = PARAM_SHORT_NAME, y = ann_percent_change, 
                   fill = Category)) + 
    scale_fill_ipsum(breaks = rev(categories)) + 
    geom_col() +
    scale_y_continuous(# breaks = breaker, 
                       minor_breaks = NULL, 
                       labels = labeller) + 
    coord_flip() + 
    labs(title = title,
         subtitle = subtitle, 
         x = "Parameter", 
         y = "Average Annual Percent Change") + 
    theme_ipsum(base_size = 12, axis_title_size = 14)
}

bc_results <- bc_nested %>% 
  select(SITE_NO, PARAM_SHORT_NAME, ann_percent_change, Category, sig) %>% 
  nest(-SITE_NO) %>% 
  left_join(select(bc_stations, SITE_NO, SITE_NAME, LATITUDE, LONGITUDE, PEARSEDA), 
            by = "SITE_NO") %>% 
  mutate(
    percent_changing = map_dbl(data, ~ round(sum(.x$sig) / n() * 100), 1),
    sum_plot = pmap(list(data, SITE_NAME, SITE_NO), ~ {
    sum_plot(..1, to_titlecase(..2), ..3)
  }))

pal <- colorNumeric("viridis", domain = range(bc_results$percent_changing), 
                    reverse = TRUE)

bc_extent <- raster::extent(sp::spTransform(bcmaps::bc_bound("sp"), 
                                            sp::CRS("+init=epsg:4326")))

leaflet(bc_results) %>% 
  setView(lng = -126.5, lat = 54.5, zoom = 5) %>% 
  addHomeButton(bcmaps::bc_bbox("raster"), 
                layer.name = "Home", position = "topleft") %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, 
                   color = ~pal(percent_changing),
                   label = ~paste0(to_titlecase(SITE_NAME), " (", 
                                   percent_changing, "%)"),
                   popup = popupGraph(bc_results$sum_plot, width = 600, 
                                               height = 500), 
                   popupOptions = popupOptions(maxWidth = 800)
                   ) %>% 
  addLegend("bottomright", pal = pal, values = ~percent_changing,
            title = "% of Parameters<br>showing significant<br>change",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)
