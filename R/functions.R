plot_station_vars <- function(data, title, x = "DateTime", y = "Value", 
                              var = "PARAM_SHORT_NAME", which = "all", ...) {
  
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  var <- rlang::sym(var)
  
  if (!which == "all") {
    data <- filter(data, !!var %in% which)
  }
  
  
  ggplot(data, aes(x = !!x, y = !!y)) + 
    facet_wrap(vars(!!var), scales = "free_y") + 
    geom_point(aes(colour = .data$censored),size = 0.2) + 
    geom_smooth(..., size = 0.2) + 
    labs(title = paste(data$PEARSEDA[1], title, sep = ": "), 
         colour = "Censored") + 
    theme(strip.text = element_text(size = rel(0.6)))
}

make_censor <- function(x) {
  case_when(
    is.na(x) ~ "none", 
    x == "<" ~ "left", 
    x == ">" ~ "right"
  )
}

plot_smk <- function(data, smk, yvar, datevar) {
  if (!inherits(smk, "htest")) 
    stop("smk must be the output of EnvStats::kendallSeasonalTrendTest", 
         call. = FALSE)
  
  
  slope <- smk$estimate["slope"] / 365
  
  ## non-seasonal - to get an intercept estimate. 
  ## A bit of a hack, but don't know a better way
  ken <- kendallTrendTest(data[[yvar]], as.numeric(data[[datevar]]))
  int <- ken$estimate["intercept"] -
    (slope * min(as.numeric(data[[datevar]])))
  
  # int <- smk$estimate["intercept"] - 
  #   (slope * min(as.numeric(data[[datevar]])))
  
  ggplot(data, aes(x = .data$Date, y = !!sym(yvar))) + 
    geom_point() + 
    geom_abline(slope = slope, intercept = int)
}

