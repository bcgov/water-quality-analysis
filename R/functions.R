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


#' Take average of parameter values containing censored data
#' 
#' If there are both censored and non-censored values, just take the average of 
#' non-censored values. (This is probably not a great idea)
#'
#' @param x vector of values
#' @param censor vector of whether values are censored or not (same length as `x`)
#' @param censored_val what denotes censoring (e.g, `TRUE`, `"left"`, `"<"`, etc)
#' @param stat function to use for averaging (default `mean`)
#' @param censor_treatment what do do with censored values when aggregating. Options are
#'  use the value of x (`"value"`; the default), use half the value (`"half"`), or `"skip"`
#' @param ... values passed on to the `stat` function
#'
#' @return a length-one numeric vector that is the average of x. If all values 
#' are censored, returns`NA`
#' @export
#'
#' @examples
avg_censored <- function(x, censor, censored_val = c("left", "right"), stat = median, 
                         censor_treatment  = c("value", "half", "skip"), ...) {
  censor_treatment <- match.arg(censor_treatment)
  if (length(x) != length(censor)) {
    stop("x and censor must be the same length", call. = FALSE)
  }
  if (all(censor %in% censored_val)) {
    return(NA_real_)
  }
  if (censor_treatment == "skip") {
    return(stat(x[!censor %in% censored_val], ...))
  } else if (censor_treatment == "half") {
    x[censor %in% censored_val] <- x[censor %in% censored_val] / 2
  } 
  stat(x, ...)
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

month_complete <- function(x) {
  x %>% 
    group_by(year, month) %>% 
    # for each month, keep reading(s) with lowest detection limit,
    # or when there is no detection limit (i.e., for pH)
    filter((DetectionLimit == min(DetectionLimit, na.rm = TRUE)) | 
             all(is.na(DetectionLimit))) %>% 
    summarise(month_avg = avg_censored(Value, censored, na.rm = TRUE), 
              n = n(),
              sd = sd(Value, na.rm = TRUE), 
              cv = (sd / month_avg) * 100,
              n_censored = sum(censored != "none", na.rm = TRUE),
              censored = is.na(month_avg) & n_censored,
              detection_limit = unique(DetectionLimit),
              month_avg = ifelse(!censored, month_avg, 
                                 detection_limit),
              n_dl = n_distinct(DetectionLimit), 
              units = first(Units)) %>% 
    ungroup() %>% 
    complete(year = full_seq(year, 1), month = full_seq(month, 1)) %>% 
    mutate(Date = as.Date(paste(year, month, "15", sep = "-"))) %>% 
    arrange(Date)
}
