#' returns from to index, starting at 100

ret_to_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  df[is.na(df)] <- 0
  df = cumprod(1+df/100)*100
  result = df
  result
}

