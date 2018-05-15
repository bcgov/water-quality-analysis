library(canwqdata)
library(dplyr)
library(lubridate)
library(ggplot2)

bc_sites <- wq_sites() %>% 
  filter(PROV_TERR == "BC", )

all_bc_data <- wq_site_data(bc_sites$SITE_NO) %>% 
  left_join(select(bc_sites, SITE_NO, SITE_NAME))

site_date_summary <- all_bc_data %>% 
  group_by(SITE_NO, SITE_NAME) %>% 
  summarise(min_date = min(DATE_TIME_HEURE, na.rm = TRUE), 
            max_date = max(DATE_TIME_HEURE, na.rm = TRUE), 
            n_years = year(max_date) - year(min_date),
            n_variables = n_distinct(VARIABLE))

site_param_summary <- all_bc_data %>% 
  group_by(SITE_NO, SITE_NAME, VARIABLE) %>% 
  summarise(min_date = min(as.Date(DATE_TIME_HEURE), na.rm = TRUE), 
            max_date = max(as.Date(DATE_TIME_HEURE), na.rm = TRUE), 
            n_years = year(max_date) - year(min_date),
            n_readings = n(), 
            average_period = round(mean(diff(as.Date(DATE_TIME_HEURE)), na.rm = TRUE))) %>% 
  filter(max_date >= as.Date("2013-01-01")) %>% 
  filter(n_years >= 10) %>% 
  ungroup() %>% 
  select(SITE_NAME, VARIABLE) %>% 
  distinct() %>% 
  group_by(VARIABLE) %>% 
  summarise(n_stations = n()) %>% 
  left_join(wq_params() %>% 
              select(VARIABLE_COMMON_NAME, VARIABLE_TYPE) %>% 
              distinct(), 
            by = c("VARIABLE" = "VARIABLE_COMMON_NAME")) %>% 
  mutate(
    state = factor(case_when(
      grepl("TOTAL DISSOLVED", VARIABLE) ~ "Total Dissolved",
      grepl("DISSOLVED", VARIABLE) ~ "Dissolved", 
      grepl("TOTAL", VARIABLE) ~ "Total",
      TRUE ~ "Other"), 
      levels = c("Total Dissolved", "Dissolved", "Total", "Other")), 
    VARIABLE_TYPE = ifelse(is.na(VARIABLE_TYPE), "Other", 
                           tools::toTitleCase(tolower(VARIABLE_TYPE))),
    VARIABLE = tools::toTitleCase(tolower(VARIABLE))
  )


ggplot(site_param_summary, aes(x = n_stations, y = VARIABLE, colour = state)) + 
  facet_grid(rows = vars(VARIABLE_TYPE), scales = "free_y", 
             space = "free_y", switch = "y") + 
  geom_point() + 
  scale_colour_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Number of Fed/Prov stations reporting on different parameters for > 10 years", 
       x = "Number of Stations", 
       y = "Parameter (by Type)", 
       colour = "Sample State") + 
  theme(strip.placement = "outside", strip.text.y = element_text(angle = 180), 
        plot.title = element_text(hjust = 1)) 

ggsave("FedProv_stations_by_param.pdf", height = 13, width = 8)
