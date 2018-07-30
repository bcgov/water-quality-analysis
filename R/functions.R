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
    geom_step(aes(y = .data$DetectionLimit), colour = "green") + 
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

avg_censored <- function(x, censor, censored_val = c("left", "right"), stat = mean, ...) {
  if (length(x) != length(censor)) {
    stop("x and censor must be the same length", call. = FALSE)
  }
  if (all(censor %in% censored_val)) {
    return(NA_real_)
  }
  stat(x[!censor %in% censored_val], ...)
}

plot_smk <- function(data, smk, yvar, datevar) {
  if (!inherits(smk, "htest")) 
    stop("smk must be the output of smwrStats$seaken", 
         call. = FALSE)
  
  
  slope <- smk$estimate[["slope"]] / 365
  
  # Calculate intercept from median of data
  int <- smk$estimate[["median.data"]] - (smk$estimate[["median.time"]] * 
    smk$estimate[["slope"]])
  
  # Scale intercept for date scale
  int <- int - slope * min(as.numeric(data[[datevar]]))
  
  p_val <- smk$p.value
  sig <- p_val <= 0.05
  
  ggplot(data, aes(x = .data$Date, y = !!sym(yvar))) + 
    geom_point() + 
    geom_abline(slope = slope, intercept = int, colour = if (sig) "red" else "black") + 
    labs(caption = paste0("Slope: ",  
                          if (sig) paste0(slope * 365, " units per year") else "NS", 
                          "; P-value: ", round(p_val, 3)))
}

