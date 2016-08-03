#' @export returns from any index
ret_from_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  # df = df[complete.cases(df)]
  df = na.locf(df, na.rm = FALSE)
  df = (df/lag.xts(df, k = -1, na.pad = TRUE)-1)
  result = df
  result
}

#' @export returns from to index, starting at 100
ret_to_idx <- function(df){
  options(xts.compat.zoo.lag = TRUE)
  df[is.na(df)] <- 0
  df = cumprod(1+df/100)*100
  result = df
  result
}

#' quarterly series
#' @export
quarter_fun <- function(Start, End){ 
  Vec <- as.Date(levels(cut(seq.Date(Start, End, by = "month"), breaks = "quarter"))) 
  Vec[-1] - 1 
} 

#' daily series
#' @export
daily_fun <- function(Start, End){ 
  Vec <- as.Date(levels(cut(seq.Date(Start, End, by = "day"), breaks = "day"))) 
  Vec[-1] - 1 
} 

#' force round to 2 decimals
#' @export
forceround2= function(n) (sprintf("%.2f", as.numeric(n)))

#' force round to 1 decimals
#' @export
forceround1= function(n) (sprintf("%.1f", as.numeric(n)))

#' force round to 0 decimals
#' @export
forceround0= function(n) (sprintf("%.0f", as.numeric(n)))

#' round to n number of digits, 2 by default
#' @export
rounder <- function(x, digits = 2) format(round(x, digits), nsmall = digits)

#' replace NAs
#' @export
forceround2= function(n) (sprintf("%.2f", as.numeric(n)))

#' force round to 1 decimals
forceround1= function(n) (sprintf("%.1f", as.numeric(n)))

#' force round to 0 decimals
forceround0= function(n) (sprintf("%.0f", as.numeric(n)))

#' round to n number of digits, 2 by default
rounder <- function(x, digits = 2) format(round(x, digits), nsmall = digits)

#' replace NAs
naplace <- function(df, repl = 0){
  df2 = apply(df, 2, function(x) {x[is.na(x)] <- repl; x})
  reclass(df2, df)
}

#' replace NAs for a zoo series
#' @export
naplace_zoo <- function(df, repl = 0){
  
  if(is.null(dim(df))) {
    
    df[is.na(df)] <- repl
    result <- df
    
  } else {
    
    df2 <- apply(df, 2, function(x) {x[is.na(x)] <- repl; x})
    result <- zoo(df2, order.by = index(df))
    
  }
  
  result
}

#' benchmark returns calculator
#' eq_rets = zoo of equity returns 
#' fi_rets = zoo of fixed income returns
#' mkt_pct = weight of the equity
#' 1 -mkt_pct is the weight of the fixed income
#' @export
bench_fun <- function(eq_rets, fi_rets, mkt_pct, start_date = "2005-10-31"){
  options(xts.compat.zoo.lag = TRUE)
  #   eq_rets = ret_to_idx(IA$IA_SP50)
  #   fi_rets = ret_to_idx(IA$IA_LTG)
  #   mkt_pct = 0.5
  #   start_date = "1925-12-31"
  dim(eq_rets) <- NULL
  dim(fi_rets) <- NULL
  rets = merge(eq_rets, fi_rets)
  rets = rets[index(rets)>= start_date]
  rets = rets[complete.cases(rets), ]
  rets = na.locf(rets, na.rm = TRUE)
  rets$mkt = (rets$eq_rets/lag.xts(rets$eq_rets, k = -1, na.pad = TRUE) -1)
  rets$agg = (rets$fi_rets/lag.xts(rets$fi_rets, k = -1, na.pad = TRUE) -1)
  rets = merge(mkt = rets$mkt, agg = rets$agg)
  
  quarters = index(rets[endpoints(index(rets), on = 'quarters')])
  quarters = c(index(rets[start(rets)]), quarters)
  if (quarters[1] == quarters[2]) quarters = quarters[2:length(quarters)]
  val = rets
  val$mkt = NA
  val$agg = NA
  val$mkt[1] = mkt_pct*100
  val$agg[1] = (1-mkt_pct)*100
  wts = xts(t(matrix(rep(c(mkt_pct, (1-mkt_pct)), length(quarters)), nrow = 2)), order.by = quarters)
  colnames(wts) = paste(colnames(rets), sep = "_")
  
  for(row in 2:nrow(val)){
    if(dim(wts[index(val[row-1])])[1] == 0)
      val[row, ] = as.xts(t(as.vector(val[index(rets[row-1])])*
                              as.vector(1+rets[row])), order.by = index(rets[row]), colnames = c("mkt", "wgt"))
    else
      val[row, ] = (t(as.vector(wts[index(rets[row-1])]*sum(val[row-1]))*as.vector(1+rets[row])))
  }
  
  val$idx = period.apply(val, endpoints(val, 'days'), sum)
  val$ret = val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  result = zoo(val$ret)*100
  result
}

#' create return series from a model
#' vsn is for ETF, NAV, price or index-based returns; returns should be called "<type>_rets"
#' modeller is a zoo of the weights
#' custom_rets is for a non-loaded return series
#' @export
model_fun <- function(vsn, modeller, namer = 'ret', custom_rets = NULL){
  
  options(xts.compat.zoo.lag = TRUE)
  
  remover <- as.vector(apply(modeller, 2, sum))
  modeller <- modeller[, remover != 0]
  
  quarters <- quarter_fun(index(modeller[1]), Sys.Date())
  wgt <- matrix(data = NA, nrow = length(quarters), ncol = length(colnames(modeller)), 
                dimnames = list(quarters, colnames(modeller)))
  rownames(wgt) <- format(quarters, "%Y-%m-%d")
  wgts <- zoo(wgt, order.by = as.Date(rownames(wgt)))
  rownames(modeller) <- format(index(modeller), "%Y-%m-%d")
  wgts <- wgts[is.na(match(index(wgts), index(modeller))), ]
  weights <- rbind(modeller, wgts, deparse.level = 0)
  weights <- na.locf(weights)
  
  if (index(weights)[1] == index(weights)[2]) weights = weights[2:length(weights), ]
  
  if (is.null(custom_rets)) {
    picker <- function(vsn){
      switch(vsn, 
             ETF = "ETF_rets", 
             NAV = "nav_rets", 
             IDX = "index_rets", 
             PRC = "price_rets")
    }
    retser <- get(picker(vsn)) 
    
  } else {
    
    retser <- custom_rets
    
  }
  
  rets <- retser[, match(colnames(weights), colnames(retser))]
  rets <- rets[, !is.na(colnames(rets))]
  rets <- data.frame(date = index(rets), rets,
                     check.names = FALSE)
  
  idx_avail <- apply(rets, 2, FUN <- function(x) min(which(!is.na(x))))
  idx_avail <- data.frame(idx_avail,
                          check.names = FALSE)
  idx_avail$idx_avail <- rets$date[idx_avail$idx_avail]
  
  wts <- modeller
  wts <- data.frame(apply(wts, 2, FUN <- function(x) ifelse(x == 0, NA, x)),
                    check.names = FALSE)
  wts <- data.frame(date = index(modeller), wts,
                    check.names = FALSE)
  
  inv_avail <- apply(wts, 2, FUN <- function(x) min(which(!is.na(x))))
  inv_avail <- data.frame(inv_avail,
                          check.names = FALSE)
  inv_avail$inv_avail <- wts$date[inv_avail$inv_avail]
  
  date_test <- data.frame(idx = idx_avail$idx_avail, 
                          inv = inv_avail[match(rownames(idx_avail), 
                                                rownames(inv_avail)), ],
                          check.names = FALSE)
  date_test$good <- ifelse(date_test$idx<= date_test$inv, 1, 0)
  date_test$tix <- rownames(idx_avail)
  
  date_test = date_test[complete.cases(date_test$inv), ]
  
  rets[is.na(rets)] <- 0
  rets <- zoo(rets[, 2:ncol(rets)], order.by = rets$date)
  rets <- as.xts(rets)
  if (sum(grep('cash', colnames(weights), ignore.case = TRUE) != 0)) {
    
    rets$CASH <- 0
    
  }
  rets <- rets / 100
  
  ### start added code to match contribution code
  miss_mat <- matrix(data = 0, nrow = length(index(weights)[!index(weights) %in% index(rets)]), 
                     ncol = length(colnames(weights)))
  missing <- data.frame(miss_mat, check.names = FALSE)
  colnames(missing) <- colnames(weights)
  missing.z <- xts(missing, order.by = index(weights)[!index(weights) %in% index(rets)])
  if (length(index(missing.z)) > 0){
    
    rets <- rbind(rets, missing.z)
    
  }
  
  ### end added code to match contribution code
  rets <- rets[index(rets) >= start(weights), ]
  weights <- weights[index(weights) >= start(rets), ]
  
  val <- rets
  val[1:length(val[, 1]), ] <- NA
  val <- val[, order(colnames(val))]
  weights <- weights[, order(colnames(weights))]
  rets <- rets[, order(colnames(rets))]
  val[1, ] <- weights[1, match(toupper(colnames(val)), toupper(colnames(weights)), nomatch = NA)] * 100
  
  for(row in 2:nrow(val)){
    
    if (dim(weights[index(val[row - 1])])[1] == 0) {
      val[row, ] <- 
        as.xts(t(as.vector(val[row - 1, ]) * 
                   as.vector(1 + rets[row, ])), 
               order.by = index(rets[row, ]), colnames = colnames(val))
    } else {
      
      val[row, ] <- 
        (t(as.vector(weights[index(val[row - 1, ]), ] * 
                       sum(val[row - 1, ])) * 
             as.vector(1 + rets[row, ])))
    }
    
  }
  
  val$idx <- period.apply(val, endpoints(val, 'days'), sum)
  val$ret <- val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  
  result <- zoo(val$ret) * 100
  
  if (sum(date_test$good)!= length(date_test$good)){
    
    result <- print("NOT ENOUGH INDEX HISTORY!")
    result <- print(date_test)
    
  }
  
  names(result) <- namer
  result
  
}

#' identical to model_fun, except does not rebalance
#' @export
model_fun_no_rebal <- function(vsn, model, namer = 'ret', custom_rets = NULL){
  #   i = 6
  #   vsn = "ETF"
  #   model = get(paste(composites$STRATs[i], "_ETF_wgt", sep = ""))
  #   starter = as.Date(index(xts::first(model)))
  #   ender = as.Date(Sys.Date() - 1)
  options(xts.compat.zoo.lag = TRUE)
  rownames(model) = format(index(model), "%Y-%m-%d")
  weights = na.locf(model)
  if (index(weights)[1] == index(weights)[2]) weights = weights[2:length(weights), ]
  
  if (is.null(custom_rets)) {
    picker <- function(vsn){
      switch(vsn, 
             ETF = "ETF_rets", 
             NAV = "nav_rets", 
             IDX = "index_rets", 
             PRC = "price_rets")
    }
    retser <- get(picker(vsn)) 
    
  } else {
    
    retser <- custom_rets
    
  }
  
  rets = retser[, match(colnames(weights), colnames(retser))]
  rets = rets[, !is.na(colnames(rets))]
  rets = data.frame(date = index(rets), rets)
  
  idx_avail = apply(rets, 2, FUN <- function(x) min(which(!is.na(x))))
  idx_avail = data.frame(idx_avail)
  idx_avail$idx_avail = rets$date[idx_avail$idx_avail]
  
  wts = model
  wts = data.frame(apply(wts, 2, FUN <- function(x) ifelse(x == 0, NA, x)))
  wts = data.frame(date = index(model), wts)
  
  inv_avail = apply(wts, 2, FUN <- function(x) min(which(!is.na(x))))
  inv_avail = data.frame(inv_avail)
  inv_avail$inv_avail = wts$date[inv_avail$inv_avail]
  
  date_test = data.frame(idx = idx_avail$idx_avail, inv = inv_avail[match(rownames(idx_avail), rownames(inv_avail)), ])
  date_test$good = ifelse(date_test$idx<= date_test$inv, 1, 0)
  date_test$tix = rownames(idx_avail)
  
  date_test = date_test[complete.cases(date_test$inv), ]
  
  rets[is.na(rets)]<-0
  rets = zoo(rets[, 2:ncol(rets)], order.by = rets$date)
  rets = as.xts(rets)#, order.by = index(rets))
  if (sum(grep('cash', colnames(weights), ignore.case = TRUE) != 0)) {
    rets$CASH = 0
  }
  rets = rets/100
  
  ### start added code to match contribution code
  miss_mat <- matrix(data = 0, nrow = length(index(weights)[!index(weights) %in% index(rets)]), 
                     ncol = length(colnames(weights)))
  missing <- data.frame(miss_mat)
  colnames(missing) <- colnames(weights)
  missing.z <- xts(missing, order.by = index(weights)[!index(weights) %in% index(rets)])
  if (length(index(missing.z)) > 0){
    rets <- rbind(rets, missing.z)
  }
  ### end added code to match contribution code
  
  rets = rets[index(rets)>= start(weights)]
  weights = weights[index(weights)>= start(rets)]
  
  val = rets
  val[1:length(val[, 1]), ]<-NA
  val = val[, order(colnames(val))]
  weights = weights[, order(colnames(weights))]
  rets = rets[, order(colnames(rets))]
  val[1, ] = weights[1, match(toupper(colnames(val)), toupper(colnames(weights)), nomatch = NA)]*100
  
  for(row in 2:nrow(val)){
    if(dim(weights[index(val[row-1])])[1] == 0)
      val[row, ] = as.xts(t(as.vector(val[row-1, ])*as.vector(1+rets[row, ])), 
                          order.by = index(rets[row, ]), colnames = colnames(val))
    else
      val[row, ] = (t(as.vector(weights[index(val[row-1, ])]*sum(val[row-1, ]))*as.vector(1+rets[row, ])))
  }
  
  val$idx = period.apply(val, endpoints(val, 'days'), sum)
  val$ret = val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  
  result = zoo(val$ret)*100
  
  if (sum(date_test$good)!= length(date_test$good)){
    result = print("NOT ENOUGH INDEX HISTORY!")
    result = print(date_test)
  }
  names(result) <- namer
  result
}

#' casts a bloomberg download
#' @export
bdh_1fld <- function(df) {
  ### casts the melted BB download data
  df[, 3] = as.numeric(df[, 3])
  df2 = dcast(df, date~ticker, value.var = colnames(df)[3])
  # df2 = data.frame(df2, check.names = FALSE, stringsAsFactors = FALSE)
  df2$date = as.Date(df2$date)
  df3 = zoo(df2[, colnames(df2)!= 'date'], order.by = df2$date)
  colnames(df3) = gsub(" Index", "", colnames(df3))
  colnames(df3) = gsub(" Equity", "", colnames(df3))
  result = na.locf(df3, na.rm = FALSE)
  
}

#' APX to R conversion
#' @export
perf_shrink <- function(df){
  # df <- reader("RB_TRC.pbf")
  df$'V22' = gsub(" ", "", df$'V22')
  df = df[, do.call(cbind, lapply(df, class))!= "logical"]
  names(df)[names(df) == "V5"] = "date"
  names(df)[names(df) == "V4"] = "totport"
  names(df)[names(df) == "V22"] = "TR"
  df$TR[df$TR == ""] <- 0
  df$TR[df$TR == "?"] <- 0
  df$TR <- as.double(df$TR)
  df = df[df$totport == "totport", ]
  df$nchar = sapply(df$date, nchar)
  df$date = ifelse(df$nchar == 5, paste(0, as.character(df$date), sep=""), df$date)
  df$date = as.Date(as.character(df$date), "%m%d%y")
  dt = data.frame(TR = as.numeric(df$TR), row.names = df$date)
  result = zoo(dt, order.by = as.Date(row.names(dt)))
}

#' APX to R conversion with EOD weights for factsheets
#' @export
perf_shrink_factsheet <- function(df, fees = "n"){
  df = df[, do.call(cbind, lapply(df, class))!= "logical"]
  names(df)[names(df) == "V1"] = "trade"
  names(df)[names(df) == "V5"] = "date"
  names(df)[names(df) == "V4"] = "ticker"
  names(df)[names(df) == "V3"] = "type"
  names(df)[names(df) == "V8"] = "flow"
  names(df)[names(df) == "V16"] = "pos_eod"
  names(df)[names(df) == "V18"] = "pos_acb"
  names(df)[names(df) == "V24"] = "trade"
  names(df)[names(df) == "V63"] = "pos_gain"
  names(df)[names(df) == "V74"] = "ynfees"
  df$nchar = sapply(df$date, nchar)
  df$date = ifelse(df$nchar == 7, paste(0, as.character(df$date), sep = ""), df$date)
  df$date = as.Date(df$date, "%m%d%Y")
  df = df[df$ticker %notin% c('client', 'dvshrt', 'dvmid', 'dvlong', 'dvfive',
                              'divacc', 'dvwash', 'mfwash', 'calong', 'cashrt'), ]
  df = df[df$trade!= 'si', ]
  df = df[df$ynfees == fees, ]
  dt = data.frame(date = (df$date), ticker = as.vector(toupper(df$ticker)), type = as.vector(df$type), 
                  flow = (df$flow), pos_eod = (df$pos_eod), 
                  pos_acb = (df$pos_acb), pos_gain = (df$pos_gain), stringsAsFactors = FALSE)
  ###Fix BGU and MWJ and HSBC ticker change issue
  dt = dt[!(dt$date == "2012-06-29"&dt$ticker == "MWJ"), ]
  dt = dt[!(dt$date == "2012-06-29"&dt$ticker == "BGU"), ]
  dt = dt[!(dt$date == "2014-06-12"&dt$ticker == "CFT"), ]
  dt = dt[!(dt$date >= "2013-11-15"&dt$ticker == "HBC"), ]
  dt = dt[!(dt$date < "2013-11-15"&dt$ticker == "HSBC"), ]
  dt = dt[!(dt$date >= "2011-07-20"&dt$ticker == "PVH.OLD"), ]
  dt = dt[!(dt$date < "2011-07-20"&dt$ticker == "PVH"), ]
  dt = dt[!(dt$date >= "2011-11-30"&dt$ticker == "ETN.OLD"), ]
  dt = dt[!(dt$date >= "2015-04-07"&dt$ticker == "IGT.OLD"), ]
  dt = dt[!(dt$date < "2011-11-30"&dt$ticker == "ETN"), ]
  dt = dt[!dt$ticker == "ESAAX", ]
  dt = dt[!dt$ticker == "SHRHOLDER", ]
  dt = dt[!dt$ticker == "TAXWHLD", ]
  
  
  dt$ticker[dt$ticker == "MWJ"] = "MIDU"
  dt$ticker[dt$ticker == "BGU"] = "SPXL"
  dt$ticker[dt$ticker == "HBC"] = "HSBC"
  dt$ticker[dt$ticker == "CFT"] = "CRED"
  dt$ticker[dt$ticker == "PVH.OLD"] = "PVH"
  dt$ticker[dt$ticker == "ETN.OLD"] = "ETN"
  dt$ticker[dt$ticker == "UNTD.OLD"] = "UNTD"
  dt$ticker[dt$ticker == "SPTN.OLD"] = "SPTN"
  dt$ticker[dt$ticker == "SMH.OLD"] = "SMHYL"
  dt$ticker[dt$ticker == "SANM.OLD"] = "SANM"
  dt$ticker[dt$ticker == "IGT.OLD"] = "IGT"
  dt$ticker[dt$ticker == "RDS.A"] = "RDS/A"
  dt$ticker[dt$ticker == "GOOG.OLD"] = "GOOG"
  dt <- dt[!dt$ticker == "370ESC758",]
  dt$ticker[dt$ticker == "HYEM.OLD"] = "HYEM"
  dt$ticker[dt$ticker == "RSX.OLD"] = "RSX"
  
  dt$pos_tr=dt$pos_gain/dt$pos_acb
  dt=ddply(dt,"date",mutate,port_eod=sum(pos_eod))
  dt=ddply(dt,"date",mutate,port_acb=sum(pos_acb))
  #dt$wgt=with(dt,pos_acb/port_acb)
  dt$wgt=with(dt,pos_eod/port_eod)
  
  return(dt)
}

#' specific to the .prf and .prb files from APX; clears the header 
#' @export
reader <- function(df){
read.csv(df, header = FALSE, sep = ",", skip = 7, stringsAsFactors = FALSE, strip.white = TRUE)
}

#' specific to the .pse files from APX; clears the header 
#' @export
reader_sec <- function(df){
  read.csv(df, header = FALSE, sep = ",", skip = 2, stringsAsFactors = FALSE, strip.white = TRUE)
}

#' contribution
#' @export
contr <- function(start_date, end_date, contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")){
  #   i = 2
  #   start_date = as.Date("2011-10-16")
  #   end_date <- as.Date("2014-01-15")
  #   end_date = as.Date(xts::last(as.yearmon(index(CRP)))) - 1
  #   contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")
  memb_contr = get(contr_data)
  memb_contr$date = as.Date(memb_contr$date)
  dfer = memb_contr[(memb_contr$date > start_date & memb_contr$date <= end_date), ]
  dfer$ticker = as.vector(dfer$ticker)
  dfer$ticker[dfer$ticker == 'MNYFUND']<-'Cash'
  tickers = unique(as.vector(dfer$ticker))
  
  port = dfer[dfer$ticker == "Cash", ]
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod = (port.z$port_cumtr)
  port_prod = port_prod[length(port_prod)]-1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = dfer[dfer$ticker == ticker, c('date', 'contr', 'wgt', 'pos_tr')]
    names(memb) = c('date', 'contr', 'wgt', 'pos_tr')
    memb.z = zoo(memb[, c('contr', 'wgt', 'pos_tr')], order.by = memb$date)
    
    df2 = merge(port.z, memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    df2$pos_tr = ifelse(is.na(df2$pos_tr), 0, df2$pos_tr)
    cum_contr = cumsum(df2$beta*df2$contr)
    cum_pos_tr = cumprod(1+df2$pos_tr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    memb_wgt = ifelse(is.na(df2$wgt), 0, df2$wgt)
    wgt_tab = merge(wgt_tab, memb_wgt)
    names(wgt_tab)[names(wgt_tab) == "memb_wgt"] = ticker
  }
  
  contr_sum = t(contr_tab[length(index(contr_tab)),]*100)
  contr_sum = data.frame(t(contr_sum))
  contr_sum = contr_sum[names(contr_sum)!= "contr_tab"]
  rownames(contr_sum) = NULL
  
  avg_wgt = apply(wgt_tab, 2, FUN = function(x) mean(x))
  avg_wgt = data.frame(t(avg_wgt))
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  result_temp = t(rbind(contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(result_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total))
  result = rbind(result_temp, Total)
  result = data.frame(result)
  # orderer = c("Cash", sort(rownames(result)[2:(length(rownames(result))-1)]), "Total")
  orderer = c("Cash", sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")
  result = result[orderer, ]
}

#' contribution trade
#' @export
contr_tr <- function(start_date, end_date, contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")){
  #    i = 2
  #    start_date = as.Date("2012-12-31")
  #    end_date = as.Date("2013-11-03")
  #    contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")
  memb_contr = get(contr_data)
  memb_contr$date = as.Date(memb_contr$date)
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  df$ticker[df$ticker == 'MNYFUND']<-'Cash'
  
  ###control for fractionals and other
  df$ticker = ifelse(df$wgt<0.0025, 
                     paste('Cash', as.vector(df$ticker), sep = "_"), 
                     as.vector(df$ticker))
  df$ticker[df$ticker == 'Cash_Cash'] = 'Cash'
  
  tickers = unique(as.vector(df$ticker))
  
  port = df[df$ticker == "Cash", ]
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = (port.z$port_cumtr)
  port_prod = port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  pos_tr_tab = port.z$port_tr
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'wgt', 'pos_tr')]
    names(memb) = c('date', 'contr', 'wgt', 'pos_tr')
    memb.z = zoo(memb[, c('contr', 'wgt', 'pos_tr')], order.by = memb$date)
    
    df2 = merge(port.z, memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    df2$pos_tr = ifelse(is.na(df2$pos_tr), 0, df2$pos_tr)
    cum_contr = cumsum(df2$beta*df2$contr)
    cum_pos_tr = cumprod(1+df2$pos_tr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    pos_tr_tab = merge(pos_tr_tab, cum_pos_tr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    names(pos_tr_tab)[names(pos_tr_tab) == "cum_pos_tr"] = ticker
    
    memb_wgt = ifelse(is.na(df2$wgt), 0, df2$wgt)
    wgt_tab = merge(wgt_tab, memb_wgt)
    names(wgt_tab)[names(wgt_tab) == "memb_wgt"] = ticker
  }
  
  contr_sum = t(contr_tab[length(index(contr_tab))] * 100)
  contr_sum = data.frame(t(contr_sum))
  contr_sum = contr_sum[names(contr_sum)!= "contr_tab"]
  rownames(contr_sum) = NULL
  
  pos_tr_sum = t((pos_tr_tab[length(index(pos_tr_tab))] - 1) * 100)
  pos_tr_sum = data.frame(t(pos_tr_sum))
  pos_tr_sum = pos_tr_sum[names(pos_tr_sum)!= "pos_tr_tab"]
  rownames(pos_tr_sum) = NULL
  
  avg_wgt = apply(wgt_tab, 2, FUN = function(x) mean(x))
  avg_wgt = data.frame(t(avg_wgt))
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  result_temp = t(rbind(TR = pos_tr_sum, contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(result_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total))
  # Total[2] = Total[1]
  output3 = rbind(result_temp, Total)
  output3 = data.frame(output3)
  
  output1 = output3[rownames(output3) %in% ETF_cats$Ticker, ]
  output2 = output3[!rownames(output3) %in% ETF_cats$Ticker, ]
  other_contr = sum(output2$contr)
  
  output1$contr[row.names(output1) == 'Cash'] = output1$contr[row.names(output1) == 'Cash'] + other_contr
  
  result = output1
  orderer = c("Cash", sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")
  result['Total', 'TR'] = NA
  result = result[orderer, ]
  result
}

#' long-term contribution
#' @export
contr_lt <- function(start_date, end_date, contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")){
  #     i = 2
  #     start_date = as.Date("2009-12-31")
  #     end_date = as.Date("2013-12-31")
  #     contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")
  memb_contr = get(contr_data)
  memb_contr$date = as.Date(memb_contr$date)
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  df$ticker[df$ticker == 'MNYFUND']<-'Cash'
  tickers = unique(as.vector(df$ticker))
  
  port = df[df$ticker == "Cash", ]
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = port.z$port_cumtr
  port_prod <- port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'wgt', 'pos_tr')]
    names(memb) = c('date', 'contr', 'wgt', 'pos_tr')
    memb.z = zoo(memb[, c('contr', 'wgt', 'pos_tr')], order.by = memb$date)
    
    df2 = merge(port.z, memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    df2$pos_tr = ifelse(is.na(df2$pos_tr), 0, df2$pos_tr)
    cum_contr = cumsum(df2$beta*df2$contr)
    cum_pos_tr = cumprod(1+df2$pos_tr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
  }
  
  contr_tab <- contr_tab[,colnames(contr_tab)!= "contr_tab"]
  result = contr_tab
}

#' contribution by asset class
#' @export
contr_lt_class <- function(contr_tab, classer = "US_Broad_Asset_Class"){
  df2 <- contr_tab
  classes <- as.vector(ETF_cats[,classer][match(colnames(df2), ETF_cats$Ticker)])
  
  classes[is.na(classes)] <- "Other"
  class_list <- unique(classes)
  
  df3 <- lapply(class_list, FUN = function(cl_l) 
    apply(df2, 1, FUN = function(df2) sum(df2[classes==cl_l])))
  
  df4 <- do.call('cbind', df3)
  colnames(df4) <- class_list
  df4 <- data.frame(df4, check.names = FALSE)
  df5 <- zoo(df4, order.by = as.Date(rownames(df4)))
  result <- df5
}

#' contribution by trades
#' @export
contr_trades <- function(start_date, end_date, contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")){
  #   i = 2
  #   start_date = as.Date("2010-10-16")
  #   end_date = as.Date("2014-01-15")
  #   contr_data = paste(composites$STRAT_CODES[i], "S", sep = "_")
  memb_contr = get(contr_data)
  memb_contr$date = as.Date(memb_contr$date)
  memb_contr_trim = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  memb_contr_trim$ticker = as.vector(memb_contr_trim$ticker)
  memb_contr_trim$ticker[memb_contr_trim$ticker == 'MNYFUND']<-'Cash'
  
  ###control for fractionals and other
  memb_contr_trim$ticker = ifelse(memb_contr_trim$wgt<0.001, 
                                  paste('Cash', as.vector(memb_contr_trim$ticker), sep = "_"), 
                                  as.vector(memb_contr_trim$ticker))
  memb_contr_trim$ticker[memb_contr_trim$ticker == 'Cash_Cash'] = 'Cash'
  
  tickers = unique(as.vector(memb_contr_trim$ticker))
  dates = unique(memb_contr_trim$date)
  
  port = memb_contr_trim[memb_contr_trim$ticker == "Cash", ]
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z_main = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z_main$port_cumtr = cumprod(1 + port.z_main$port_tr)
  
  port_prod_temp = port.z_main$port_cumtr
  port_prod <- port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z_main$beta <- (log(1+port.z_main$port_tr) / port.z_main$port_tr)
  port.z_main$beta[is.na(port.z_main$beta)] <- 0
  port.z_main$beta = port.z_main$beta/linkcoef[[1]]
  
  cum_contr = NULL
  
  ##FIND TRADE DATES
  output3 <- data.frame()
  
  for (j in 1:length(tickers)){
    tix <- tickers[j]
    df = memb_contr_trim[as.character(memb_contr_trim$ticker) == tix, ]
    df$wgt_dif <- c(NA, diff(df$wgt)*100) 
    
    dateopen = ifelse(diff(c(as.Date("1950-12-31"), df$date))>1, df$date, 0)
    dateopen = as.Date(dateopen[dateopen!= 0], format = "%Y-%m-%d")
    dateclose = ifelse(diff(rev(c(df$date, as.Date("2150-12-31"))))<= -2, rev(df$date), 0)
    dateclose = as.Date(dateclose[dateclose!= 0], format = "%Y-%m-%d")
    open = dateopen
    close = rev(dateclose)
    hold_per = data.frame(open, close)
    hold_per$open = as.Date(hold_per$open, format = "%Y-%m-%d")
    hold_per$close = as.Date(hold_per$close, format = "%Y-%m-%d")
    
    if (length(dateopen) == length(df$date)) hold_per$close <- hold_per$open
    
    ### calculate number of subsequent trades
    sqnt_tr <- df[df$wgt_dif>0.5&df$flow>0, ]#&(df$flow/df$port_eod)<0.25
    sqnt_tr <- sqnt_tr[complete.cases(sqnt_tr), ]
    sqnt_trs <- sqnt_tr$date
    
    for (tr in 1:length(hold_per$open)){
      hold_per$trs[tr] <- 
        length(sqnt_trs[sqnt_trs>hold_per[tr, 'open']&sqnt_trs<hold_per[tr, 'close']])
    }
    
    ### PERFORMANCE CALCS
    for (p in 1:nrow(hold_per)){
      if (hold_per$open[p] == hold_per$close[p]) df2 = df[df$date == hold_per$open[p], ] else
        df2 = df[(df$date >= hold_per$open[p] & df$date <= hold_per$close[p]), ]
      #       if (hold_per$open[p] == hold_per$close[p]) port.z = port.z_main[index(port.z_main) == hold_per$open[p], ] else
      #         port.z = port.z_main[(index(port.z_main) >= hold_per$open[p] & index(port.z_main) <= hold_per$close[p]), ]
      port.z <- port.z_main
      
      contr_tab <- port.z$beta
      wgt_tab <- port.z$port_acb
      pos_tr_tab = port.z$port_tr
      
      cum_contr = NULL
      cum_pos_tr = NULL
      
      ###contribution
      memb = df2[, c('date', 'contr', 'wgt', 'pos_tr', 'pos_eod')]
      names(memb) = c('date', 'contr', 'wgt', 'pos_tr', 'pos_eod')
      memb.z = zoo(memb[, c('contr', 'wgt', 'pos_tr', 'pos_eod')], order.by = memb$date)
      
      df2 <- merge(port.z, memb.z)
      df2$contr[is.na(df2$contr)] <- 0
      df2$pos_tr[is.na(df2$pos_tr)] <- 0
      
      cum_contr <- cumsum(df2$beta * df2$contr)
      cum_pos_tr <- cumprod(1 + df2$pos_tr)
      
      contr_tab = merge(contr_tab, cum_contr)
      contr_tab <- contr_tab[index(contr_tab) >= hold_per$open[p] & index(contr_tab) <= hold_per$close[p], ]
      df2 <- df2[(index(df2) >= hold_per$open[p] & index(df2) <= hold_per$close[p]), ]
      
      port.z$port_cumtr <- cumprod(1 + port.z_main$port_tr)
      
      pos_tr_tab = merge(pos_tr_tab, cum_pos_tr)
      memb_wgt = df2$wgt
      memb_wgt[is.na(memb_wgt)] <- 0
      wgt_tab = merge(wgt_tab, memb_wgt)
      
      contr_tab <- contr_tab[index(contr_tab) >= hold_per$open[p] & index(contr_tab) <= hold_per$close[p], ]
      pos_tr_tab <- pos_tr_tab[index(pos_tr_tab) >= hold_per$open[p] & index(pos_tr_tab) <= hold_per$close[p], ]
      memb_wgt <- memb_wgt[index(memb_wgt) >= hold_per$open[p] & index(memb_wgt) <= hold_per$close[p], ]
      wgt_tab <- wgt_tab[index(wgt_tab) >= hold_per$open[p] & index(wgt_tab) <= hold_per$close[p], ]
      
      contr_sum = t(contr_tab[length(index(contr_tab)),] * 100)
      contr_sum = data.frame(t(contr_sum))
      contr_sum = contr_sum[names(contr_sum)!= "contr_tab"]
      rownames(contr_sum) = NULL
      
      pos_tr_sum = t((pos_tr_tab[length(index(pos_tr_tab))] - 1) * 100)
      pos_tr_sum = data.frame(t(pos_tr_sum))
      pos_tr_sum = pos_tr_sum[names(pos_tr_sum)!= "pos_tr_tab"]
      rownames(pos_tr_sum) = NULL
      
      yearfrac = (hold_per$close[p]-hold_per$open[p])[[1]]/365
      pos_tr_ann = ((1+pos_tr_sum[[1]]/100)^(1/yearfrac)-1)*100
      
      avg_wgt = apply(wgt_tab, 2, mean, na.rm = TRUE)
      avg_wgt = data.frame(t(avg_wgt))
      avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
      
      port.z <- port.z[(index(port.z) >= hold_per$open[p] & index(port.z) <= hold_per$close[p]), ]
      port_trs = (prod(1 + port.z$port_tr) - 1) * 100
      
      close_flag = ifelse(hold_per$close[p]<max(hold_per$close), ">", "")
      
      output4 <- data.frame(class = (ETF_cats$Broad_Category_Group[match(tix, ETF_cats$Ticker)]), 
                            ticker = tix, 
                            name = (ETF_cats$Name[match(tix, ETF_cats$Ticker)]), 
                            open = hold_per$open[p], 
                            close = ifelse(hold_per$close[p]<end_date, 
                                           as.character(hold_per$close[p]), 
                                           ifelse(memb.z$pos_eod[index(memb.z$pos_eod) == hold_per$close[p]] == 0, 
                                                  as.character(hold_per$close[p]), "")), 
                            contr = contr_sum[[1]], 
                            twr = pos_tr_sum[[1]], #tr_ann = pos_tr_ann, 
                            avg_wgt = avg_wgt[[1]]*100,
                            port_tr = port_trs, 
                            sbqnt_tr = hold_per$trs[p], 
                            stringsAsFactors = FALSE)
      output3 <- rbind(output3, output4) 
    }
  }
  output1 = output3[output3$ticker %in% ETF_cats$Ticker, ]
  output2 = output3[!output3$ticker %in% ETF_cats$Ticker, ]
  other_contr = sum(output2$contr)
  
  output1$contr[output1$ticker == 'Cash'] = output1$contr[output1$ticker == 'Cash'] + other_contr
  #   output1$contr[row.names(output1) == 'Cash'] = output1$contr[row.names(output1) == 'Cash']+other_contr
  
  output = output1
  ###add in portfolio and FI relative returns
  result = data.frame(output)
}

#' contribution for closed positions
#' @export
contr_trade_cl <- function(contr_trader){
  df = contr_trader[contr_trader$close!= "", ]
  df = df[order(df$class, df$open, df$ticker), ]
  rownames(df) = NULL
  result = df[colnames(df)!= 'name' & colnames(df)!= 'class']
  result
}

#' contribution for open positions
#' @export
contr_trade_op <- function(contr_trader){
  df = contr_trader[contr_trader$close == "", ]
  df = df[order(df$class, df$open, df$ticker), ]
  rownames(df) = NULL
  result = df[colnames(df)!= 'close' & colnames(df)!= 'name' & colnames(df)!= 'class']
  result
}

# print(contr_trade_op(test))
#' contribution by sector
#' @export
contr_sector <- function(contr_data){
  #contr_data<-'SELECTV_S'
  memb_contr = get(contr_data)
  memb_contr$date = as.Date(memb_contr$date)
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  df$ticker[df$ticker == 'MNYFUND']<-'Cash'
  #df$ticker[df$ticker == 'MNYFUND2']<-'Cash'
  
  ###control for fractionals and other
  df$ticker = ifelse(df$wgt<0.001, 
                     paste('Cash', as.vector(df$ticker), sep = "_"), 
                     as.vector(df$ticker))
  df$ticker[df$ticker == 'Cash_Cash'] = 'Cash'
  
  ###control for fractionals and other
  
  #df$ticker[df$ticker == 'Cash_Cash'] = 'Cash'
  
  tickers = unique(as.vector(df$ticker))
  
  port = df[df$ticker == "Cash", ]
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = port.z$port_cumtr
  port_prod <- port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  pos_tr_tab = port.z$port_tr
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'wgt', 'pos_tr')]
    names(memb) = c('date', 'contr', 'wgt', 'pos_tr')
    memb.z = zoo(memb[, c('contr', 'wgt', 'pos_tr')], order.by = memb$date)
    
    df2 = merge(port.z, memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    df2$pos_tr = ifelse(is.na(df2$pos_tr), 0, df2$pos_tr)
    cum_contr = cumsum(df2$beta*df2$contr)
    cum_pos_tr = cumprod(1+df2$pos_tr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    pos_tr_tab = merge(pos_tr_tab, cum_pos_tr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    names(pos_tr_tab)[names(pos_tr_tab) == "cum_pos_tr"] = ticker
    
    memb_wgt = ifelse(is.na(df2$wgt), 0, df2$wgt)
    wgt_tab = merge(wgt_tab, memb_wgt)
    names(wgt_tab)[names(wgt_tab) == "memb_wgt"] = ticker
  }
  
  contr_sum = t(contr_tab[length(index(contr_tab)),] * 100)
  contr_sum = data.frame(t(contr_sum))
  contr_sum = contr_sum[names(contr_sum)!= "contr_tab"]
  rownames(contr_sum) = NULL
  
  pos_tr_sum = t((pos_tr_tab[length(index(pos_tr_tab)),] - 1) * 100)
  pos_tr_sum = data.frame(t(pos_tr_sum))
  pos_tr_sum = pos_tr_sum[names(pos_tr_sum)!= "pos_tr_tab"]
  rownames(pos_tr_sum) = NULL
  
  avg_wgt = apply(wgt_tab, 2, mean)
  avg_wgt = data.frame(t(avg_wgt))
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  result_temp = t(rbind(TR = pos_tr_sum, contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(result_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total))
  
  output3 = rbind(result_temp, Total)
  output3 = data.frame(output3)
  
  output1 = output3[!substring(rownames(output3),1,5)=='Cash_',]
  output2 = output3[substring(rownames(output3),1,5)=='Cash_',]
  other_contr = sum(output2$contr)
  
  #   output1$contr[output1$ticker == 'Cash'] = output1$contr[output1$ticker == 'Cash']+other_contr
  output1$contr[row.names(output1) == 'Cash'] = output1$contr[row.names(output1) == 'Cash']+other_contr
  
  result = output1
  orderer = c("Cash", sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")
  result['Total', 'TR'] = NA
  result = result[orderer, ]
  
  result_temp<-result
  #result_temp<-contr('2012-12-31', '2013-09-30', 'SELECTV_S')
  conn = blpConnect(throw.ticker.errors = FALSE)
  rownames(result_temp) <- gsub("PGN.OLD1","PGN",rownames(result_temp))
  rownames(result_temp) <- gsub("\\.","/",rownames(result_temp))
  
  result_temp$ticker<- paste0(rownames(result_temp), " US Equity")
  ticker<-unique(result_temp$ticker)
  result_temp$Sector = bdp(conn, ticker, "GICS_SECTOR_NAME")[, 1]
  for(i in 1:nrow(result_temp)){ifelse(result_temp$ticker[i] == 'MNYFUND', result_temp$Sector[i]<-'Cash', result_temp$Sector[i])}
  result_temp$Sector[is.na(result_temp$Sector)]<-"Unclassified"
  result_temp$ticker=NULL
  result_temp = result_temp[rownames(result_temp)!= 'Total', ]
  result_temp = result_temp[result_temp[, 'avg_wgt']>.01, ]
  result_temp = result_temp[order(result_temp[, 'contr'], decreasing = TRUE), ]
  result = data.frame(Sector = result_temp$Sector, Ticker = rownames(result_temp), Contribution = result_temp$contr, 
                      'Average Weight' = result_temp$avg_wgt, 'Total Return' = result_temp$TR, check.names = FALSE)
  resulta<-cast(result, Sector~., value = 'Contribution', sum, na.rm = TRUE)
  resultb<-cast(result, Sector~., value = 'Average Weight', sum, na.rm = TRUE)
  resultc<-cast(result, Sector~., value = 'Total Return', mean, na.rm = TRUE)
  result<-data.frame(Class=resulta[,1],Contribution=resulta[,2],
                     'Average Weight'=resultb[,2],'Total Return'=resultc[,2])
  
  colnames(result)<-c("Sector", "Contribution", "Average Weight","Total Return")
  result$'Average Weight'[nrow(result)]<-(100-(sum(result$'Average Weight')-result$'Average Weight'[nrow(result)]))
  resultsum<-data.frame('Sector' = 'Total', 
                        Contribution = sum(result$Contribution), 
                        'Average Weight' = sum(result$'Average Weight'),
                        'Total Return' = mean(result$'Total Return'))
  colnames(resultsum)<-colnames(result)
  result<-rbind(result, resultsum)
  result$Contribution<-forceround2(result$Contribution)
  result$'Average Weight'<-forceround2(result$'Average Weight')
  result$'Total Return'<-forceround2(result$'Total Return')
  result$'Total Return'[nrow(result)]<-'NA'
  blpDisconnect(conn)
  return(result)
}
#' contribution by security name
#' @export


contr_name <- function(df){
  result_temp<-df
  #result_temp<-contr('2012-12-31', '2013-09-30', 'SELECTV_S')
  conn = blpConnect(throw.ticker.errors = FALSE)
  result_temp$ticker<-rownames(result_temp)
  result_temp = result_temp[rownames(result_temp)!= 'Total', ]
  result_temp$ticker<-gsub("\\.", "/", result_temp$ticker) 
  result_temp$ticker = paste(result_temp$ticker, " US Equity", sep = "")
  result_temp$Name = bdp(conn, as.vector(result_temp$ticker), "SECURITY_NAME")[, 1]
  result_temp$ticker<-NULL
  result_temp[rownames(result_temp) == 'Cash', 'Name']<-'Cash & Equivalents'
  sult_temp$Name<-nametrim(result_temp$Name)
  
  result_temp = result_temp[result_temp[, 'avg_wgt']>.01, ]
  result_temp = result_temp[order(result_temp[, 'contr'], decreasing = TRUE), ]
  result = data.frame(Name = result_temp$Name, Ticker = rownames(result_temp), Contribution = result_temp$contr, 'Average Weight' = result_temp$avg_wgt, check.names = FALSE)
  result$Contribution<-forceround2(result$Contribution)
  result$'Average Weight'<-forceround2(result$'Average Weight')
  blpDisconnect(conn)
  return(result)
}
#' contribution by asset class
#' @export


contr_class <- function(dfer, classer = "Broad_Category_Group"){
  dfer = dfer[rownames(dfer)!= 'total', ]
  dfer = dfer[rownames(dfer)!= 'Total', ]
  dfer$class = as.vector(ETF_cats[,classer][match(rownames(dfer), ETF_cats$Ticker)])
  #   dfer$class = as.vector(ETF_cats$US_Broad_Asset_Class[match(rownames(dfer), ETF_cats$Ticker)])
  dfer$class[is.na(dfer$class)]<-"Cash"
  dfer2 = ddply(dfer, "class", summarize, contr = sum(contr), avg_wgt = sum(avg_wgt))
  rownames(dfer2) = dfer2$class
  dfer2 = dfer2[, colnames(dfer2)!= 'class']
  Total = apply(dfer2, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total))
  result = rbind(dfer2, Total)
  result = data.frame(result)
  # result = result[result$avg_wgt>.009, ]
  colnames(result)<-c("Contribution", "Average Weight")
  result
}
#' contribution by benchmark for the ETF-based positions
#' @export


contr_bench_ETF <- function(start_date, end_date, eq_ret = composites$bench_rets_ETF_EQ[i], 
                            fi_ret = composites$bench_rets_ETF_FI[i], mkt_pct = composites$eq_pct[i], 
                            tickers = c(composites$bench_tickers_ETF_EQ[i], composites$bench_tickers_ETF_FI[i]),
                            classer = "Broad_Category_Group"){
  #   i = 1
  #   start_date = "2009-12-31"
  #   end_date = "2014-09-30"
  #   eq_ret = composites$bench_rets_ETF_EQ[i]
  #   fi_ret = composites$bench_rets_ETF_FI[i]
  #   mkt_pct = composites$eq_pct[i]
  tickers = c(composites$bench_tickers_ETF_EQ[i], composites$bench_tickers_ETF_FI[i])
  options(xts.compat.zoo.lag = TRUE)
  tickers[is.na(tickers)] = "XXX"
  if (is.na(eq_ret)) eq_rets<-fi_ret else eq_rets = eq_ret
  if (is.na(fi_ret)) fi_rets<-eq_ret else fi_rets = fi_ret
  rets = merge(mkt = get(eq_rets)/100, agg = get(fi_rets)/100)
  if (is.na(eq_ret)) rets$mkt<-1
  if (is.na(fi_ret)) rets$agg<-1
  rets = as.xts(rets, order.by = index(rets))
  rets[is.na(rets)] = 0
  quarters = index(rets[endpoints(index(rets), on = 'quarters')])
  quarters = c(index(rets[start(rets)]), quarters)
  if (quarters[1] == quarters[2]) quarters = quarters[2:length(quarters)]
  val = rets
  val$mkt = NA
  val$agg = NA
  val$mkt[1] = mkt_pct*100
  val$agg[1] = (1-mkt_pct)*100
  wts = xts(t(matrix(rep(c(mkt_pct, (1-mkt_pct)), length(quarters)), nrow = 2)), order.by = quarters)
  wts = wts[1:length(index(wts))-1, ]
  colnames(wts) = tickers
  
  for(row in 2:nrow(val)){
    if(dim(wts[index(val[row-1])])[1] == 0)
      val[row, ] = as.xts(t(as.vector(val[index(rets[row-1])])*
                              as.vector(1+rets[row])), order.by = index(rets[row]), colnames = c("mkt", "wgt"))
    else
      val[row, ] = (t(as.vector(wts[index(rets[row-1])]*sum(val[row-1]))*as.vector(1+rets[row])))
  }
  val$idx = period.apply(val, endpoints(val, 'days'), sum)
  val$ret = val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  colnames(val) = c(tickers, 'BENCH', 'TR')
  for (i in seq(tickers)){
    ticker = tickers[i]
    val[, ticker] = val[, ticker]/val$BENCH
  } 
  val[index(val) %in% index(wts), ][, tickers]<-wts
  
  colnames(rets) = tickers
  contrs = val[, colnames(val) %in% tickers]
  for (i in seq(tickers)){
    ticker = tickers[i]
    contrs[, ticker] = lag.xts(val[, ticker], k = -1, na.pad = TRUE)*rets[, ticker]
  }
  
  valer = val[, colnames(val)!= 'TR']
  acb = lag.xts(valer, -1, na.pad = TRUE)
  acb = (acb + valer) / 2
  
  ###create a fake .pse file
  top = data.frame(date = index(val), val$BENCH, val$TR, check.names = FALSE)
  colnames(top)[colnames(top) == 'BENCH']<-'port_eod'
  colnames(top)[colnames(top) == 'TR']<-'port_tr'
  
  acb = data.frame(date = index(acb), acb)
  port_acb = data.frame(date = acb$date, port_acb = acb$BENCH)
  acb = melt(acb[, colnames(acb)!= 'BENCH'], id.vars = 'date')
  colnames(acb)[colnames(acb) == 'value']<-'pos_acb'
  colnames(acb)[colnames(acb) == 'variable']<-'ticker'
  
  contrs = data.frame(date = index(contrs), contrs)
  contrs = melt(contrs, id.vars = 'date')
  colnames(contrs)[colnames(contrs) == 'value']<-'contr'
  colnames(contrs)[colnames(contrs) == 'variable']<-'ticker'
  
  trs = data.frame(date = index(rets), rets)
  trs = melt(trs, id.vars = 'date')
  colnames(trs)[colnames(trs) == 'value']<-'pos_tr'
  colnames(trs)[colnames(trs) == 'variable']<-'ticker'
  
  memb_contr = data.frame(date = acb$date, ticker = acb$ticker, pos_acb = acb$pos_acb, contr = contrs$contr, pos_tr = trs$pos_tr)
  memb_contr$port_tr = top$port_tr[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_acb = port_acb$port_acb[match(memb_contr$date, port_acb$date)]
  
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  tickers = unique(as.vector(df$ticker))
  
  ###strip dates
  port = df[match(unique(df$date), df$date), ]
  
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = (port.z$port_cumtr)
  port_prod = port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'pos_acb')]
    names(memb) = c('date', 'contr', 'pos_acb')
    memb.z = zoo(memb[, c('contr', 'pos_acb')], order.by = memb$date)
    
    df2 = merge(port.z, contr = memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    cum_contr = cumsum(df2$beta*df2$contr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    pos_acb = ifelse(is.na(df2$pos_acb), 0, df2$pos_acb)
    wgt_tab = merge(wgt_tab, pos_acb)
    names(wgt_tab)[names(wgt_tab) == "pos_acb"] = ticker
  }
  
  ###create table
  contr_tab = contr_tab[, colnames(contr_tab)!= "contr_tab"]
  contr_sum = t(tail(contr_tab, 1)*100)
  contr_sum = data.frame(t(contr_sum), check.names = FALSE)
  rownames(contr_sum) = NULL
  wgt_tab = wgt_tab[complete.cases(wgt_tab), ]
  avg_wgt = apply(wgt_tab, 2, mean)
  avg_wgt = data.frame(t(avg_wgt), check.names = FALSE)
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  contr_temp = t(rbind(contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(contr_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total, check.names = FALSE))
  contr_temp = rbind(contr_temp, Total)
  contr_temp = contr_temp[rownames(contr_temp)!= "XXX", ]
  colnames(contr_temp)<-c("Contribution", "Average Weight")
  
  rets = rets[(time(rets)>start_date & time(rets)<= end_date), ]##changed from >= 
  tot_ret = cumprod(1+rets[complete.cases(rets), ])-1
  tot_ret_sum = (tail(tot_ret, 1)*100)
  tot_ret_sum$Total = NA
  tot_ret_sum = t(data.frame(tot_ret_sum, check.names = FALSE))
  tot_ret_sum = tot_ret_sum[dimnames(tot_ret_sum)[[1]]!= "XXX", ]
  tot_ret_sum = data.frame(tot_ret_sum, check.names = FALSE)
  colnames(tot_ret_sum)<-"Total Return"
  result = cbind(tot_ret_sum, contr_temp)
  result = data.frame(result, check.names = FALSE)
  rownames(result)[rownames(result)!= "Total"]<-
    as.vector(ETF_cats[,classer][match(rownames(result)[rownames(result)!= "Total"], ETF_cats$Ticker)])
  result
}

#' contribution by benchmark for the Index-based positions
#' @export
contr_bench_IDX <- function(start_date, end_date, eq_ret = composites$bench_rets_IDX_EQ[i], 
                            fi_ret = composites$bench_rets_IDX_FI[i], mkt_pct = composites$eq_pct[i], 
                            tickers = c(composites$bench_tickers_IDX_EQ[i], composites$bench_tickers_IDX_FI[i]),
                            classer = "Broad_Category_Group"){
  # i = 8
  # start_date = "2009-12-31"
  # end_date = "2013-03-31"
  # eq_ret = composites$bench_rets_IDX_EQ[i]
  # fi_ret = composites$bench_rets_IDX_FI[i]
  # mkt_pct = composites$eq_pct[i]
  # tickers = c(composites$bench_tickers_IDX_EQ[i], composites$bench_tickers_IDX_FI[i])
  options(xts.compat.zoo.lag = TRUE)
  tickers[is.na(tickers)] = "XXX"
  if (is.na(eq_ret)) eq_rets<-fi_ret else eq_rets = eq_ret
  if (is.na(fi_ret)) fi_rets<-eq_ret else fi_rets = fi_ret
  rets = merge(mkt = get(eq_rets)/100, agg = get(fi_rets)/100)
  if (is.na(eq_ret)) rets$mkt<-1
  if (is.na(fi_ret)) rets$agg<-1
  rets = as.xts(rets, order.by = index(rets))
  rets[is.na(rets)] = 0
  quarters = index(rets[endpoints(index(rets), on = 'quarters')])
  quarters = c(index(rets[start(rets)]), quarters)
  if (quarters[1] == quarters[2]) quarters = quarters[2:length(quarters)]
  val = rets
  val$mkt = NA
  val$agg = NA
  val$mkt[1] = mkt_pct*100
  val$agg[1] = (1-mkt_pct)*100
  wts = xts(t(matrix(rep(c(mkt_pct, (1-mkt_pct)), length(quarters)), nrow = 2)), order.by = quarters)
  wts = wts[1:length(index(wts))-1, ]
  colnames(wts) = tickers
  
  for(row in 2:nrow(val)){
    if(dim(wts[index(val[row-1])])[1] == 0)
      val[row, ] = as.xts(t(as.vector(val[index(rets[row-1])])*
                              as.vector(1+rets[row])), order.by = index(rets[row]), colnames = c("mkt", "wgt"))
    else
      val[row, ] = (t(as.vector(wts[index(rets[row-1])]*sum(val[row-1]))*as.vector(1+rets[row])))
  }
  val$idx = period.apply(val, endpoints(val, 'days'), sum)
  val$ret = val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  colnames(val) = c(tickers, 'BENCH', 'TR')
  for (i in seq(tickers)){
    ticker = tickers[i]
    val[, ticker] = val[, ticker]/val$BENCH
  } 
  val[index(val) %in% index(wts), ][, tickers]<-wts
  
  colnames(rets) = tickers
  contrs = val[, colnames(val) %in% tickers]
  for (i in seq(tickers)){
    ticker = tickers[i]
    contrs[, ticker] = lag.xts(val[, ticker], k = -1, na.pad = TRUE)*rets[, ticker]
  }
  
  valer = val[, colnames(val)!= 'TR']
  acb = lag.xts(valer, -1, na.pad = TRUE)
  acb = (acb+valer)/2
  
  ###create a fake .pse file
  top = data.frame(date = index(val), val$BENCH, val$TR, check.names = FALSE)
  colnames(top)[colnames(top) == 'BENCH']<-'port_eod'
  colnames(top)[colnames(top) == 'TR']<-'port_tr'
  
  acb = data.frame(date = index(acb), acb)
  port_acb = data.frame(date = acb$date, port_acb = acb$BENCH)
  acb = melt(acb[, colnames(acb)!= 'BENCH'], id.vars = 'date')
  colnames(acb)[colnames(acb) == 'value']<-'pos_acb'
  colnames(acb)[colnames(acb) == 'variable']<-'ticker'
  
  contrs = data.frame(date = index(contrs), contrs)
  contrs = melt(contrs, id.vars = 'date')
  colnames(contrs)[colnames(contrs) == 'value']<-'contr'
  colnames(contrs)[colnames(contrs) == 'variable']<-'ticker'
  
  trs = data.frame(date = index(rets), rets)
  trs = melt(trs, id.vars = 'date')
  colnames(trs)[colnames(trs) == 'value']<-'pos_tr'
  colnames(trs)[colnames(trs) == 'variable']<-'ticker'
  
  memb_contr = data.frame(date = acb$date, ticker = acb$ticker, pos_acb = acb$pos_acb, contr = contrs$contr, pos_tr = trs$pos_tr)
  memb_contr$port_tr = top$port_tr[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_acb = port_acb$port_acb[match(memb_contr$date, port_acb$date)]
  
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  tickers = unique(as.vector(df$ticker))
  
  ###strip dates
  port = df[match(unique(df$date), df$date), ]
  
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = (port.z$port_cumtr)
  port_prod = port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'pos_acb')]
    names(memb) = c('date', 'contr', 'pos_acb')
    memb.z = zoo(memb[, c('contr', 'pos_acb')], order.by = memb$date)
    
    df2 = merge(port.z, contr = memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    cum_contr = cumsum(df2$beta*df2$contr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    pos_acb = ifelse(is.na(df2$pos_acb), 0, df2$pos_acb)
    wgt_tab = merge(wgt_tab, pos_acb)
    names(wgt_tab)[names(wgt_tab) == "pos_acb"] = ticker
  }
  
  ###create table
  contr_tab = contr_tab[, colnames(contr_tab)!= "contr_tab"]
  contr_sum = t(contr_tab[length(index(contr_tab)),] * 100)
  contr_sum = data.frame(t(contr_sum), check.names = FALSE)
  rownames(contr_sum) = NULL
  wgt_tab = wgt_tab[complete.cases(wgt_tab), ]
  avg_wgt = apply(wgt_tab, 2, mean)
  avg_wgt = data.frame(t(avg_wgt), check.names = FALSE)
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  contr_temp = t(rbind(contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(contr_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total, check.names = FALSE))
  contr_temp = rbind(contr_temp, Total)
  contr_temp = contr_temp[rownames(contr_temp)!= "XXX", ]
  colnames(contr_temp)<-c("Contribution", "Average Weight")
  
  rets = rets[(time(rets)>start_date & time(rets)<= end_date), ]##changed from >= 
  tot_ret = cumprod(1+rets[complete.cases(rets), ])-1
  tot_ret_sum = (tot_ret[length(index(tot_ret))])*100
  tot_ret_sum$Total = NA
  tot_ret_sum = t(data.frame(tot_ret_sum, check.names = FALSE))
  tot_ret_sum = tot_ret_sum[dimnames(tot_ret_sum)[[1]]!= "XXX", ]
  tot_ret_sum = data.frame(tot_ret_sum, check.names = FALSE)
  colnames(tot_ret_sum)<-"Total Return"
  result = cbind(tot_ret_sum, contr_temp)
  result = data.frame(result, check.names = FALSE)
  rownames(result)[rownames(result)!= "Total"]<-
    as.vector(ETF_cats[,classer][match(rownames(result)[rownames(result)!= "Total"], ETF_cats$BENCHMARK)])
  result
}

#' contribution for a model portfolio
#' @export
contr_model <-   function(vsn, start_date, end_date, modeler, custom_rets = NULL){
    #   vsn = "IDX"
    #   modeler = "FI_BENCH_wgt"
    #   start_date = as.Date("2009-12-31")
    #   end_date = as.Date("2016-03-31")
    #   end_date = as.Date(Sys.Date() - 1)
    model <- get(modeler)
    options(xts.compat.zoo.lag = TRUE)
    quarters = quarter_fun(index(model[1]), Sys.Date())
    wgt = matrix(data = NA, nrow = length(quarters), ncol = length(colnames(model)), 
                 dimnames = list(quarters, colnames(model)))
    rownames(wgt) = format(quarters, "%Y-%m-%d")
    wgts = zoo(wgt[drop = FALSE], order.by = as.Date(rownames(wgt)))
    rownames(model) = format(index(model), "%Y-%m-%d")
    wgts = wgts[is.na(match(index(wgts), index(model))), drop = FALSE]
    weights = rbind(model, wgts, deparse.level = 0)
    weights = na.locf(weights)
    if (index(weights)[1] == index(weights)[2]) weights = weights[2:length(weights), ]
    weights = weights[index(weights)<= end_date, drop = FALSE]
    
    if (is.null(custom_rets)) {
      picker <- function(vsn){
        switch(vsn, 
               ETF = "ETF_rets", 
               NAV = "nav_rets", 
               IDX = "index_rets", 
               PRC = "price_rets")
      }
      retser <- get(picker(vsn)) 
      
    } else {
      
      retser <- custom_rets
      
    }
    
    rets <- retser[, match(colnames(weights), colnames(retser)), drop = FALSE]
    rets <- rets[, !is.na(colnames(rets)), drop = FALSE]
    rets <- data.frame(date = index(rets), rets, check.names = FALSE)
    # rets[is.na(rets)] <- 0
    rets <- zoo(rets[, 2:ncol(rets), drop = FALSE], order.by = rets$date)
    # rets <- as.xts(rets)
    if (sum(grep('cash', colnames(weights), ignore.case = TRUE) != 0)) {
      
      rets$Cash <- 0
      
    }
    
    rets <- rets / 100
    
    ### fix for dates in weight file that are not trading days
    
    miss_mat <- matrix(data = 0, nrow = length(index(weights)[!index(weights) %in% index(rets)]),
                       ncol = length(colnames(weights)))
    missing <- data.frame(miss_mat, check.names = FALSE)
    colnames(missing) <- colnames(weights)
    missing.z <- zoo(missing, order.by = index(weights)[!index(weights) %in% index(rets)])
    if (length(index(missing.z)) > 0){
      rets <- rbind(rets, missing.z)
    }
    rets <- rets[index(rets) >= start(weights), drop = FALSE]
    
    val <- rets
    val[1:length(val[, 1]), ] <- NA
    val <- val[, order(colnames(val)), drop = FALSE]
    weights <- weights[, order(colnames(weights)), drop = FALSE]
    rets <- rets[, order(colnames(rets)), drop = FALSE]
    val[1, ] <- weights[1, match(toupper(colnames(val)), toupper(colnames(weights)), nomatch = NA)]*100
    
    for(row in 2:nrow(val)) {
      
      if (dim(weights[index(val[row-1, drop = FALSE]), , drop = FALSE])[1] == 0)
        val[row, ] <- 
          zoo(t(as.vector(val[index(rets[row-1, , drop = FALSE]), drop = FALSE]) *
                  as.vector(1+rets[row, drop = FALSE])), order.by = index(rets[row, drop = FALSE]))
      else
        
        val[row, ] <- 
          (t(as.vector(weights[index(rets[row-1, drop = FALSE])] * 
                         sum(val[row-1, drop = FALSE])) * as.vector(1+rets[row, drop = FALSE])))
      
    }
    
    # val$idx <- period.apply(val, endpoints(val, 'days'), sum, check.names = FALSE)
    val <- zoo(val)
    val$idx <- apply(val, 1, sum, check.names = FALSE)
    
    val$ret <- val$idx / lag.xts(val$idx, k = -1, na.pad = TRUE) - 1
    tickers <- colnames(rets)
    
    for (i in seq(tickers)) {
      
      ticker <- tickers[i]
      val[, ticker] <- val[, ticker] / val$idx
      
    }
    
    val[index(val) %in% index(weights), ][, tickers] <- weights
    
    contrs <- val[, colnames(val) %in% tickers, drop = FALSE]
    
    for (i in seq(tickers)) {
      
      ticker <- tickers[i]
      contrs[, ticker] <- lag.xts(val[, ticker], k = -1, na.pad = TRUE) * rets[, ticker]
      
    }
    
    valer <- val[, colnames(val) != 'ret']
    acb <- lag.xts(valer, -1, na.pad = TRUE)
    acb <- (acb + valer) / 2
    
    ###create a fake .pse file
    top <- data.frame(date = index(val), val$idx, val$ret, check.names = FALSE)
    colnames(top) <- c('date', 'port_eod', 'port_tr')
    
    acb <- data.frame(date = index(acb), acb, check.names = FALSE)
    port_acb <- data.frame(date = acb$date, port_acb = acb$idx, check.names = FALSE)
    acb <- melt(acb[, colnames(acb)!= 'idx'], id.vars = 'date')
    colnames(acb)[colnames(acb) == 'value'] <- 'pos_acb'
    colnames(acb)[colnames(acb) == 'variable'] <- 'ticker'
    
    contrs <- data.frame(date = index(contrs), contrs, check.names = FALSE)
    contrs <- melt(contrs, id.vars = 'date')
    colnames(contrs)[colnames(contrs) == 'value'] <- 'contr'
    colnames(contrs)[colnames(contrs) == 'variable'] <- 'ticker'
    
    trs <- data.frame(date = index(rets), rets, check.names = FALSE)
    trs <- melt(trs, id.vars = 'date')
    colnames(trs)[colnames(trs) == 'value'] <- 'pos_tr'
    colnames(trs)[colnames(trs) == 'variable'] <- 'ticker'
    
    memb_contr <- data.frame(date = acb$date, 
                             ticker = acb$ticker, 
                             pos_acb = acb$pos_acb, 
                             contr = contrs$contr, 
                             pos_tr = trs$pos_tr, 
                             check.names = FALSE)
    memb_contr$port_tr <- top$port_tr[match(memb_contr$date, top$date)]
    memb_contr$port_eod <- top$port_eod[match(memb_contr$date, top$date)]
    memb_contr$port_eod <- top$port_eod[match(memb_contr$date, top$date)]
    memb_contr$port_acb <- port_acb$port_acb[match(memb_contr$date, port_acb$date)]
    
    df <- memb_contr[(memb_contr$date > start_date & memb_contr$date <= end_date), ]
    df$ticker <- as.vector(df$ticker)
    tickers <- unique(as.vector(df$ticker))
    
    ###strip dates
    port <- df[match(unique(df$date), df$date), ]
    
    port <- data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb, check.names = FALSE)
    port.z <- zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
    port.z$port_cumtr <- cumprod(1 + port.z$port_tr)
    port_prod_temp <- (port.z$port_cumtr)
    port_prod <- port_prod_temp[length(index(port_prod_temp))] - 1
    linkcoef <- (log(1+port_prod))/port_prod
    port.z$beta <- (log(1+port.z$port_tr)/port.z$port_tr)
    port.z$beta[is.na(port.z$beta)] <- 0
    port.z$beta <- port.z$beta/linkcoef[[1]]
    
    contr_tab <- port.z$port_tr
    wgt_tab <- port.z$port_acb
    
    cum_contr <- NULL
    
    ###contribution loop
    for (q in seq(tickers)) {
      ticker <- tickers[q]
      memb <- df[df$ticker == ticker, c('date', 'contr', 'pos_acb')]
      names(memb) <- c('date', 'contr', 'pos_acb')
      memb.z <- zoo(memb[, c('contr', 'pos_acb')], order.by = memb$date)
      
      df2 <- merge(port.z, contr = memb.z)
      df2$contr <- ifelse(is.na(df2$contr), 0, df2$contr)
      cum_contr <- cumsum(df2$beta*df2$contr)
      
      contr_tab <- merge(contr_tab, cum_contr)
      names(contr_tab)[names(contr_tab) == "cum_contr"] <- ticker
      pos_acb <- ifelse(is.na(df2$pos_acb), 0, df2$pos_acb)
      wgt_tab <- merge(wgt_tab, pos_acb)
      names(wgt_tab)[names(wgt_tab) == "pos_acb"] <- ticker
      
    }
    
    ###create table
    contr_tab = contr_tab[, colnames(contr_tab) != "contr_tab", drop = FALSE]
    contr_sum = t(contr_tab[length(index(contr_tab)), drop = FALSE] * 100)
    contr_sum = data.frame(t(contr_sum), check.names = FALSE)
    rownames(contr_sum) = NULL
    wgt_tab = wgt_tab[complete.cases(wgt_tab), ]
    avg_wgt = apply(wgt_tab, 2, mean)
    avg_wgt = data.frame(t(avg_wgt), check.names = FALSE)
    avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
    contr_temp = t(rbind(contr = contr_sum, avg_wgt = avg_wgt * 100))
    
    Total = apply(contr_temp, 2, sum, na.rm = TRUE)
    Total = t(data.frame(Total, check.names = FALSE))
    contr_temp = rbind(contr_temp, Total)
    contr_temp = contr_temp[rownames(contr_temp)!= "XXX", ]
    colnames(contr_temp)<-c("contr", "avg_wgt")
    
    ###calc TRs
    wgt_on = data.frame(wgt_tab[, colnames(wgt_tab)!= 'wgt_tab'])
    temp = data.frame(t(rep(0, length(colnames(wgt_on)))))
    colnames(temp) = colnames(wgt_on)
    rownames(temp) = start_date
    wgt_on = rbind(temp, wgt_on)
    wgt_on[wgt_on > 0] <- 1
    rets = rets[(time(rets)>= start_date & time(rets)<= end_date), drop=F]
    wgt_on = zoo(wgt_on, order.by = as.Date(row.names(wgt_on)))
    
    rets = rets * wgt_on
    
    tot_ret = cumprod(1+rets[complete.cases(rets), drop=F])-1
    tot_ret_sum <- tot_ret[length(index(tot_ret)), drop=F] * 100
    tot_ret_sum$Total = NA
    tot_ret_sum = t(data.frame(tot_ret_sum, check.names = FALSE))
    tot_ret_sum = tot_ret_sum[dimnames(tot_ret_sum)[[1]]!= "XXX", ]
    tot_ret_sum = data.frame(tot_ret_sum, check.names = FALSE)
    colnames(tot_ret_sum) <- "TR"
    result = cbind(tot_ret_sum, contr_temp)
    result = result[result$avg_wgt>0, ]
    
    # if (orderit == TRUE) {
    #   
    #   sorter <- rownames(result)
    #   sorter <- sorter[rev(order(result$contr))]
    #   sorter <- sorter[(sorter != 'Cash' & sorter != 'Total')]
    #   
    # } else {
    #   
    #   sorter <- sort(rownames(result)[(rownames(result) != 'Cash'&rownames(result) != 'Total')])
    #   
    # }
    # 
    # if (grepl('cash', paste(rownames(result), collapse = ""), ignore.case = TRUE)) {
    #   
    #   orderer <- c("Cash", sorter, "Total")
    #   
    # } else {
    #   
    #   orderer <- c(sorter, "Total")    
    #   
    # }
    # 
    # result <- result[orderer, ]
    
    if (grepl('cash', paste(rownames(result), collapse = ""), ignore.case = TRUE)) {
      
      orderer = c("Cash", sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")
      
    } else {
      
      orderer = c(sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")    
      
    }
    
    result = result[orderer, ]
    
    
  }

#' contribution for a model portfolio w/ no rebalances
#' @export
contr_model_no_rebal <- function(vsn, start_date, end_date, model, custom_rets = NULL){
  #     vsn = "ETF"
  #     model = CRP_CORE_C_wgt
  #     start_date = as.Date("2009-12-31")
  #     end_date = as.Date(Sys.Date() - 1)
  options(xts.compat.zoo.lag = TRUE)
  rownames(model) = format(index(model), "%Y-%m-%d")
  weights = model
  weights = na.locf(weights)
  if (index(weights)[1] == index(weights)[2]) weights = weights[2:length(weights), ]
  weights = weights[index(weights)<= end_date]
  
  if (is.null(custom_rets)) {
    picker <- function(vsn){
      switch(vsn, 
             ETF = "ETF_rets", 
             NAV = "nav_rets", 
             IDX = "index_rets", 
             PRC = "price_rets")
    }
    retser <- get(picker(vsn)) 
    
  } else {
    
    retser <- custom_rets
    
  }
  
  rets = retser[, match(colnames(weights), colnames(retser))]
  rets = rets[, !is.na(colnames(rets))]
  rets = data.frame(date = index(rets), rets)
  rets[is.na(rets)]<-0
  rets = zoo(rets[, 2:ncol(rets)], order.by = rets$date)
  # rets = rets[, colnames(rets)!= 'date']
  rets = as.xts(rets)#, order.by = index(rets))
  if (sum(grep('cash', colnames(weights), ignore.case = TRUE) != 0)) {
    
    rets$Cash = 0
    
  }
  
  #     if (sum(grep('cash', colnames(weights), ignore.case = TRUE) == 0)) {
  #     
  #     colnames(weights)[grep('cash', colnames(weights), ignore.case = TRUE)] == 'CASH'
  #     
  #   }
  rets = rets/100
  
  ### fix for dates in weight file that are not trading days
  
  miss_mat <- matrix(data = 0, nrow = length(index(weights)[!index(weights) %in% index(rets)]), ncol = length(colnames(weights)))
  missing <- data.frame(miss_mat)
  colnames(missing) <- colnames(weights)
  missing.z <- xts(missing, order.by = index(weights)[!index(weights) %in% index(rets)])
  if (length(index(missing.z)) > 0){
    rets <- rbind(rets, missing.z)
  }
  rets = rets[index(rets) >= start(weights)]
  
  val = rets
  val[1:length(val[, 1]), ]<-NA
  val = val[, order(colnames(val))]
  weights = weights[, order(colnames(weights))]
  rets = rets[, order(colnames(rets))]
  val[1, ] = weights[1, match(toupper(colnames(val)), toupper(colnames(weights)), nomatch = NA)]*100
  
  for(row in 2:nrow(val)){
    if(dim(weights[index(val[row-1])])[1] == 0)
      val[row, ] = as.xts(t(as.vector(val[index(rets[row-1, ])])*
                              as.vector(1+rets[row, ])), order.by = index(rets[row, ]), colnames = colnames(val))
    else
      val[row, ] = (t(as.vector(weights[index(rets[row-1, ])]*sum(val[row-1, ]))*as.vector(1+rets[row, ])))
  }
  
  val$idx = period.apply(val, endpoints(val, 'days'), sum)
  val$ret = val$idx/lag.xts(val$idx, k = -1, na.pad = TRUE)-1
  tickers = colnames(rets)
  
  for (i in seq(tickers)){
    ticker = tickers[i]
    val[, ticker] = val[, ticker]/val$idx
  }
  val[index(val) %in% index(weights), ][, tickers] <- weights
  
  contrs = val[, colnames(val) %in% tickers]
  for (i in seq(tickers)){
    ticker = tickers[i]
    contrs[, ticker] = lag.xts(val[, ticker], k = -1, na.pad = TRUE)*rets[, ticker]
  }
  
  valer = val[, colnames(val)!= 'ret']
  acb = lag.xts(valer, -1, na.pad = TRUE)
  acb = (acb+valer)/2
  
  ###create a fake .pse file
  top = data.frame(date = index(val), val$idx, val$ret, check.names = FALSE)
  colnames(top)[colnames(top) == 'idx']<-'port_eod'
  colnames(top)[colnames(top) == 'ret']<-'port_tr'
  
  acb = data.frame(date = index(acb), acb)
  port_acb = data.frame(date = acb$date, port_acb = acb$idx)
  acb = melt(acb[, colnames(acb)!= 'idx'], id.vars = 'date')
  colnames(acb)[colnames(acb) == 'value']<-'pos_acb'
  colnames(acb)[colnames(acb) == 'variable']<-'ticker'
  
  contrs = data.frame(date = index(contrs), contrs)
  contrs = melt(contrs, id.vars = 'date')
  colnames(contrs)[colnames(contrs) == 'value']<-'contr'
  colnames(contrs)[colnames(contrs) == 'variable']<-'ticker'
  
  trs = data.frame(date = index(rets), rets)
  trs = melt(trs, id.vars = 'date')
  colnames(trs)[colnames(trs) == 'value']<-'pos_tr'
  colnames(trs)[colnames(trs) == 'variable']<-'ticker'
  
  memb_contr = data.frame(date = acb$date, ticker = acb$ticker, pos_acb = acb$pos_acb, contr = contrs$contr, pos_tr = trs$pos_tr)
  memb_contr$port_tr = top$port_tr[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_eod = top$port_eod[match(memb_contr$date, top$date)]
  memb_contr$port_acb = port_acb$port_acb[match(memb_contr$date, port_acb$date)]
  
  df = memb_contr[(memb_contr$date>start_date & memb_contr$date<= end_date), ]
  df$ticker = as.vector(df$ticker)
  tickers = unique(as.vector(df$ticker))
  
  ###strip dates
  port = df[match(unique(df$date), df$date), ]
  
  port = data.frame(date = port$date, port_tr = port$port_tr, port_acb = port$port_acb)
  port.z = zoo(port[, c('port_tr', 'port_acb')], order.by = port$date) 
  port.z$port_cumtr = cumprod(1+port.z$port_tr)
  port_prod_temp = (port.z$port_cumtr)
  port_prod = port_prod_temp[length(index(port_prod_temp))] - 1
  linkcoef = (log(1+port_prod))/port_prod
  port.z$beta = (log(1+port.z$port_tr)/port.z$port_tr)
  port.z$beta[is.na(port.z$beta)]<-0
  port.z$beta = port.z$beta/linkcoef[[1]]
  
  contr_tab = port.z$port_tr
  wgt_tab = port.z$port_acb
  
  cum_contr = NULL
  
  ###contribution loop
  for (q in seq(tickers)){
    ticker = tickers[q]
    memb = df[df$ticker == ticker, c('date', 'contr', 'pos_acb')]
    names(memb) = c('date', 'contr', 'pos_acb')
    memb.z = zoo(memb[, c('contr', 'pos_acb')], order.by = memb$date)
    
    df2 = merge(port.z, contr = memb.z)
    df2$contr = ifelse(is.na(df2$contr), 0, df2$contr)
    cum_contr = cumsum(df2$beta*df2$contr)
    
    contr_tab = merge(contr_tab, cum_contr)
    names(contr_tab)[names(contr_tab) == "cum_contr"] = ticker
    pos_acb = ifelse(is.na(df2$pos_acb), 0, df2$pos_acb)
    wgt_tab = merge(wgt_tab, pos_acb)
    names(wgt_tab)[names(wgt_tab) == "pos_acb"] = ticker
  }
  
  ###create table
  contr_tab = contr_tab[, colnames(contr_tab)!= "contr_tab"]
  contr_sum = t(contr_tab[length(index(contr_tab)),] * 100)
  contr_sum = data.frame(t(contr_sum), check.names = FALSE)
  rownames(contr_sum) = NULL
  wgt_tab = wgt_tab[complete.cases(wgt_tab), ]
  avg_wgt = apply(wgt_tab, 2, mean)
  avg_wgt = data.frame(t(avg_wgt), check.names = FALSE)
  avg_wgt = avg_wgt[names(avg_wgt)!= "wgt_tab"]
  contr_temp = t(rbind(contr = contr_sum, avg_wgt = avg_wgt*100))
  Total = apply(contr_temp, 2, sum, na.rm = TRUE)
  Total = t(data.frame(Total, check.names = FALSE))
  contr_temp = rbind(contr_temp, Total)
  contr_temp = contr_temp[rownames(contr_temp)!= "XXX", ]
  colnames(contr_temp)<-c("contr", "avg_wgt")
  
  ###calc TRs
  wgt_on = data.frame(wgt_tab[, colnames(wgt_tab)!= 'wgt_tab'])
  temp = data.frame(t(rep(0, length(colnames(wgt_on)))))
  colnames(temp) = colnames(wgt_on)
  rownames(temp) = start_date
  wgt_on = rbind(temp, wgt_on)
  wgt_on[wgt_on > 0] <- 1
  rets = rets[(time(rets)>= start_date & time(rets)<= end_date), ]
  wgt_on = xts(wgt_on, order.by = as.Date(row.names(wgt_on)))
  
  rets = rets*wgt_on
  
  tot_ret = cumprod(1+rets[complete.cases(rets), ])-1
  tot_ret_sum <- tot_ret[length(index(tot_ret))] * 100
  tot_ret_sum$Total = NA
  tot_ret_sum = t(data.frame(tot_ret_sum, check.names = FALSE))
  tot_ret_sum = tot_ret_sum[dimnames(tot_ret_sum)[[1]]!= "XXX", ]
  tot_ret_sum = data.frame(tot_ret_sum, check.names = FALSE)
  colnames(tot_ret_sum)<-"TR"
  result = cbind(tot_ret_sum, contr_temp)
  result = result[result$avg_wgt>0, ]
  if (grepl('cash', paste(rownames(result), collapse = ""), ignore.case = TRUE)){
    orderer = c("Cash", sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")
  } else {
    orderer = c(sort(rownames(result)[(rownames(result)!= 'Cash'&rownames(result)!= 'Total')]), "Total")    
  }
  result = result[orderer, ]
}

#' month to date return
#' @export
mtd <- function(rets){
months = index(rets[endpoints(index(rets), on = 'months')])
months = c(index(rets[start(rets)]), months)
if (months[1] == months[2]) months = months[2:length(months)]
last_date = xts::last(index(rets))
eom  = ceiling_date(last_date, 'month')-1
mo_ago  = as.Date(as.yearmon(as.Date(eom)) -(1/12), frac = 1)
if (last_date == eom){
  rets_1mo = rets[index(rets)>months[max(1,(length(months)-1))]]
} else {
  rets_1mo = rets[index(rets)>mo_ago]
}
oneMO = t((xts::last(cumprod(1+rets_1mo/100))-1)*100)
colnames(oneMO) = "MTD"
result = oneMO
if ((tail(index(rets), 1) - head(index(rets), 1)) < 28) {
  result[, 1] = NA
  result
}
result
}

#' quarter to date retrurn
#' @export
qtr <- function(rets) {
  
  quarters = index(rets[endpoints(index(rets), on = 'quarters')])
  quarters = c(index(rets[start(rets)]), quarters)
  if (quarters[1] == quarters[2]) quarters = quarters[2:length(quarters)] 
  rets_qtd = rets[index(rets)>quarters[(length(quarters)-1)]]
  QTD = t((xts::last(cumprod(1 + rets_qtd / 100)) - 1) * 100)
  
  if(difftime(quarters[(length(quarters))], quarters[(length(quarters) - 1)]) < 80) {
    
    colnames(QTD) = "QTD"
    result = QTD
    
  } else {
    
    colnames(QTD) = "QTR"
    result = QTD
    
  }
  
}

#' year to date return
#' @export
ytd <- function(rets){
  years = index(rets[endpoints(index(rets), on = 'years')])
  # length(years)
  
  starter <- 
    as.Date(as.yearmon(years %m-% months(month(years) - 1))) - 1
  starter <- starter[length(starter)]
  rets_ytd = rets[index(rets)>starter]
  YTD = t((xts::last(cumprod(1+rets_ytd/100))-1)*100)
  colnames(YTD) = "YTD"
  result = YTD
  result
}

#' variable year return for a monthly series
#' @export
xyear_m <- function(rets, yrs){ 
  xyear_per <- as.Date(as.yearmon(as.Date(last(index(rets)))) - yrs, frac = 1)
  rets <- rets[!index(rets) <= xyear_per, ]
  rets <- rets[complete.cases(rets), ]
  
  if (length(index(rets)) < (yrs * 12)) {
    
    xyear_ret <- 
      rep(NA, ncol(rets))
    names(xyear_ret) <- colnames(rets)
    
  } else {
    
    xyear_ret <- t((xts::last(cumprod(1 + rets / 100))^(1 / yrs) - 1) * 100)
    colnames(xyear_ret) <- paste0("ret", yrs, "yr")
    
  }
  
  result <- xyear_ret
  result
  
} 

#' variable year return for a daily series, end of month
#' @export
xyear <- function(rets, yrs){
months = index(rets[endpoints(index(rets), on = 'months')])
months = c(index(rets[start(rets)]), months)
if (months[1] == months[2]) months = months[2:length(months)] 
last_date = xts::last(index(rets))
eom  = ceiling_date(last_date, 'month')-1
yrXago  = last_date - months(yrs/12)
if (last_date == eom) {
  rets_Xyr = rets[index(rets)>months[max(1,(length(months)-(yrs/12)))]]
} else {
  rets_Xyr = rets[index(rets)>yrXago]
}
xYR = t((xts::last(cumprod(1+rets_Xyr/100)^(1/yrs))-1)*100)
colnames(xYR) = ifelse(yrs>1, paste0(yrs," years"), paste0(yrs," year"))
result = xYR
if ((tail(index(rets), 1) - head(index(rets), 1)) < (yrs*365.25)) {
  result[, 1] = NA
}
result
}

#' cumulative returns
#' @export
cum_returns <- function(rets){
  # rets = rets[complete.cases(rets), ]
  inc = t((xts::last(cumprod(1+rets/100))-1)*100)
  colnames(inc) = "TR"
  result = inc
  result
}

#' earliest common returns
#' @export
earliest_common <- function(rets,dt){
  #dt<-fz
  rets<-rets[!index(rets)<dt,]
  months = index(rets[endpoints(index(rets), on = 'months')])
  months = c(index(rets[start(rets)]), months)
  if (months[1] == months[2]) months = months[2:length(months)]
  
  last_date = xts::last(index(rets))
  eom  = ceiling_date(last_date, 'month')-1
  frst_date = xts::first(index(rets))
  intervaly = interval(frst_date, last_date)
  yearfrac = intervaly/dyears(1)
  if (length(months) < 12){
    ear = t((xts::last(cumprod(1+rets/100))-1)*100)
  } else {
    if (last_date == eom){
      ear = t((xts::last(cumprod(1+rets/100))^(1/((length(months)-1)/12))-1)*100)
    } else {
      ear = t((xts::last(cumprod(1+rets/100))^(1/yearfrac)-1)*100)}}
  colnames(ear) = "Earliest"
  result = ear
  result
}

#' since inception returns
#' @export
inception <- function(rets){
months = index(rets[endpoints(index(rets), on = 'months')])
months = c(index(rets[start(rets)]), months)
if (months[1] == months[2]) months = months[2:length(months)]

last_date = xts::last(index(rets))
eom  = ceiling_date(last_date, 'month')-1
frst_date = xts::first(index(rets))
intervaly = interval(frst_date, last_date)
yearfrac = intervaly/dyears(1)
if (length(months) < 12){
  inc = t((xts::last(cumprod(1+rets/100))-1)*100)
} else {
  if (last_date == eom){
    inc = t((xts::last(cumprod(1+rets/100))^(1/((length(months)-1)/12))-1)*100)
  } else {
    inc = t((xts::last(cumprod(1+rets/100))^(1/yearfrac)-1)*100)}}
colnames(inc) = "Inception"
result = inc
result
}

#' variable year standard deviation for a daily series, end of month
#' @export
xyear_sd <- function(rets, yrs, tr = trade_days_US){
  months = index(rets[endpoints(index(rets), on = 'months')])
  months = c(index(rets[start(rets)]), months)
  if (months[1] == months[2]) months = months[2:length(months)] 
  # if (length(months)<37) return ("insufficient history")
  rets = rets[!(is.na(match(index(rets), tr))), ]
  last_date = xts::last(index(rets))
  eom  = ceiling_date(last_date, 'month')-1
  xyrsago  = last_date - months(yrs * 12)
  if (last_date == eom)
    rets_xyr = rets[index(rets)>months[(length(months) - (x * 12))]]
  else
    rets_xyr = rets[index(rets)>yr10ago]
  
  tenYR = data.frame(apply(rets_10yr, 2, sd)) * sqrt(252)
  colnames(tenYR) = "10 year"
  result = tenYR
  
  if ((xts::last(index(rets))-xts::first(index(rets))) < (365 * yrs)){
    
    result[, 1] = NA
    
  }
  result
}

#' variable year standard deviation for a daily series, monthly series
#' @export
xyear_sd_m <- function(rets, yrs){
  xyear_per <- as.Date(as.yearmon(as.Date(last(index(rets)))) - (yrs), frac = 1)
  rets <- rets[!index(rets) <= xyear_per, ]
  
  if(length(index(rets)) < (yrs * 12)) {
    
    xyear_sd =  data.frame(list(port = NA, bench = NA))
    
  } else {
    
    xyear_sd <- data.frame(apply(rets, 2, sd)) * sqrt(12)
    
  }
  colnames(xyear_sd) <- "sd"
  result <- xyear_sd
  result
  } 

#' trailing rolling returns
#' @export
ret_roll <- function(rets, yrs = 1) {
    
    i <- length(rets)
    min <- as.Date(ymd(format(start(rets), "%Y/%m/%d")) %m+% months(yrs * 12))
    temp <- rets
    temp[index(temp) < min] <- NA
    n <- match(index(xts::first(temp[index(temp) >= min])), index(temp))
    
    while (i >= n) {
      
      trailWindow <- window(rets, start = as.Date(index(rets)[i] %m-% months(yrs * 12)) + 1,
                            end = as.Date(index(rets)[i]))
      cumret <- (prod(1 + trailWindow / 100) - 1) * 100
      result <- as.zoo(cumret, order.by = end(trailWindow)[1])
      temp[i] <- result
      i <- i - 1
      
    }
    
    result <- temp
    result
    
  }
  
#' forward rolling returns
#' @export
ret_roll_fwd <- function(rets, yrs = 1) {
    
    i <- 1
    max <- as.Date(ymd(format(end(rets), "%Y/%m/%d")) %m-% months(yrs * 12))
    temp <- rets
    temp[index(temp) > max] <- NA
    n <- match(index(xts::last(temp[index(temp) <= max])), index(temp))
    
    while (i <= n) {
      
      fwdWindow <- window(rets, start = as.Date(index(rets)[i] + 1),
                          end = as.Date(index(rets)[i] %m+% months(yrs * 12)))
      cumret <- (prod(1 + fwdWindow / 100) - 1) * 100
      result <- as.zoo(cumret, order.by = start(fwdWindow)[1])
      temp[i] <- result
      i <- i + 1
      
    }
    
    result = temp
    result
    
  }
  
#' trailing rolling returns
#' does NOT annualize long or short term
#' @export
ret_roll_mo <- function(rets, mos = 1){
    i = length(rets)
    min = as.Date(ymd(format(start(rets), "%Y/%m/%d")) %m+% months(mos))
    temp = rets
    temp[index(temp)<min] = NA
    n = mos#match(index(xts::first(temp[index(temp)>= min])), index(temp))
    
    if(n<12){
      while (i>= n){
        trailWindow = window(coredata(rets), start = i-n+1, end = i)
        cumret = (prod(1+trailWindow/100)-1)*100
        result = as.zoo(cumret, order.by = end(trailWindow)[1])
        temp[i] = result
        i = i-1
      }
      result = temp
    }else{
      while (i>= n){
        trailWindow = window(coredata(rets), start = i-n+1, end = i)
        cumret = (prod(1+trailWindow/100)-1)*100
        result = as.zoo(cumret, order.by = end(trailWindow)[1])
        temp[i] = result
        i = i-1
      }
      result = temp
    }
    result
  }

#' trailing rolling returns
#' annualize long and short term
#' @export
ret_roll_mo_ann_all<- function(rets, mos = 1){
    i = length(rets)
    min = as.Date(ymd(format(start(rets), "%Y/%m/%d")) %m+% months(mos))
    temp = rets
    temp[index(temp)<min] = NA
    n = mos#match(index(xts::first(temp[index(temp)>= min])), index(temp))
    
    while (i>= n){
      trailWindow = window(coredata(rets), start = i-n+1, end = i)
      #cumret = (prod(1+trailWindow/100)-1)*100
      cumret = (prod(1+trailWindow/100)^(1/(n/12))-1)*100
      result = as.zoo(cumret, order.by = end(trailWindow)[1])
      temp[i] = result
      i = i-1
    }
    result = temp
    result
  }

#' trailing rolling returns
#' annualize long term, leave short term unannualized
#' @export
ret_roll_mo_ann<- function(rets, mos = 1){
    i = length(rets)
    min = as.Date(ymd(format(start(rets), "%Y/%m/%d")) %m+% months(mos))
    temp = rets
    temp[index(temp)<min] = NA
    n = mos#match(index(xts::first(temp[index(temp)>= min])), index(temp))
    
    if(n<12){
      while (i>= n){
        trailWindow = window(coredata(rets), start = i-n+1, end = i)
        cumret = (prod(1+trailWindow/100)-1)*100
        #cumret = (prod(1+trailWindow/100)^(1/(n/12))-1)*100
        result = as.zoo(cumret, order.by = end(trailWindow)[1])
        temp[i] = result
        i = i-1
      }
      result = temp
    }else{
      while (i>= n){
        trailWindow = window(coredata(rets), start = i-n+1, end = i)
        cumret = (prod(1+trailWindow/100)^(1/(n/12))-1)*100
        result = as.zoo(cumret, order.by = end(trailWindow)[1])
        temp[i] = result
        i = i-1
      }
      result = temp
    }
    result
  }

#' trailing rolling standard deviations, for a monthly series
#' @export
sd_roll_mo <- function(rets, mos = 1){
  i = length(rets)
  min = as.Date(ymd(format(start(rets), "%Y/%m/%d")) %m+% months(mos))
  temp = rets
  temp[index(temp)<min] = NA
  n = mos#match(index(xts::first(temp[index(temp)>= min])), index(temp))
  
  while (i >= n){
    trailWindow <- window(rets, start = as.Date(index(rets)[i] %m-% months(mos)) + 1,
                          end = as.Date(index(rets)[i]))
    days = length(trailWindow)
    cumret = (sd(trailWindow) * sqrt(mos))
    result = as.zoo(cumret, order.by = end(trailWindow)[1])
    temp[i] = result
    i = i - 1
  }
  
  result = temp
  result
}

#' trailing rolling standard deviations, for a daily series
#' @export
sd_roll <- function(rets, yrs = 1, tr = trade_days_US){
  rets = rets[!(is.na(match(index(rets), tr))), ]
  i = length(rets)
  min = as.Date(ymd(format(start(rets), "%Y/%m/%d")) + months(yrs * 12))
  temp = rets
  temp[index(temp)<min] = NA
  n = match(index(head(temp[index(temp) >= min])[1,]), index(temp))
  
  while (i >= n){
    trailWindow <- window(rets, start = as.Date(index(rets)[i] %m-% months(yrs*12)) + 1,
                          end = as.Date(index(rets)[i]))
    days = length(trailWindow)
    cumret = (sd(trailWindow) * sqrt(days))
    result = as.zoo(cumret, order.by = end(trailWindow)[1])
    temp[i] = result
    i = i - 1
  }
  result = temp
  result
}

