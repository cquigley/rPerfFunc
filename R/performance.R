#' #' returns from any index

ret_from_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  # df = df[complete.cases(df)]
  df = na.locf(df, na.rm = FALSE)
  df = (df/lag.xts(df, k = -1, na.pad = TRUE)-1)
  result = df
  result
}

#' returns from to index, starting at 100

ret_to_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  df[is.na(df)] <- 0
  df = cumprod(1+df/100)*100
  result = df
  result
}

