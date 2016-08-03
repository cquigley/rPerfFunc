#' returns from any index

ret_from_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  # df = df[complete.cases(df)]
  df = na.locf(df, na.rm = FALSE)
  df = (df/lag.xts(df, k = -1, na.pad = TRUE)-1)
  result = df
  result
}

#' second returns from any index

ret_from_idx2 <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  # df = df[complete.cases(df)]
  df = na.locf(df, na.rm = FALSE)
  df = (df/lag.xts(df, k = -1, na.pad = TRUE)-1)
  result = df
  result
}
