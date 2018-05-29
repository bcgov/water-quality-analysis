plot_station_vars <- function(data, station, x = "DateTime", y = "Value", 
                              var = "PARAM_SHORT_NAME", which = "all", ...) {
  
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  var <- rlang::sym(var)
  
  data <- data %>% 
    filter(SITE_NO == station) %>% 
    mutate(censored = !is.na(ResultLetter))
  
  if (!which == "all") {
    data <- filter(data, !!var %in% which)
  }
  
  
  ggplot(data, aes(x = !!x, y = !!y)) + 
    facet_wrap(vars(!!var), scales = "free_y") + 
    geom_point(aes(colour = censored),size = 0.2) + 
    geom_smooth(..., size = 0.2) + 
    ggtitle(paste(data$PEARSEDA[1], station, sep = ": ")) + 
    theme(strip.text = element_text(size = rel(0.6)))
}
