library(yaml)
source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')
source('strategies/strategy.R')
source('a2_periods.R')
strategyFile <- 'strategies/strategy.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrdersdataList <- getData(directory="EXAMPLE")
username <- "sgrlu2"
period <- getPeriods(username)
dataList <- getData(directory="A2")
dataList_in <- lapply(dataList, function(x) x[period$startIn:period$endIn])
dataList_out <- lapply(dataList, function(x) x[period$startOut:period$endOut])
param_short <- c(5,10)
param_mid <- c(50,100)
param_long <- c(200,300)
param_series <- list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4),c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4),c(1,2,3,4))
param_total <- expand.grid(short=param_short,medium=param_mid,long=param_long,series=param_series)
profit_in <- NULL
profit_out <- NULL

for (i in seq_len(nrow(param_total))) {
    p <- param_total[i,]
  print(p)
    params <- list(lookbacks=list(short=as.integer(p$short),medium=as.integer(p$medium),long=as.integer(p$long)),series=p$series[[1]])
    results <- backtest(dataList_in,getOrders,params,sMult=0.2)
    profit_in[i] <- results$aggProfit
  print(profit_in[i])
    # param_total[i,]$profit <- results$aggProfit
}
for (i in seq_len(nrow(param_total))) {
    p <- param_total[i,]
    print(p)
    params <- list(lookbacks=list(short=as.integer(p$short),medium=as.integer(p$medium),long=as.integer(p$long)),series=p$series[[1]])
    results <- backtest(dataList_out,getOrders,params,sMult=0.2)
    profit_out[i] <- results$aggProfit
    print(profit_out[i])
}
# Strategy parameters -- this will not be an empty list when you are done
# params <- list(lookbacks=list(short=as.integer(p$short),medium=as.integer(p$medium),long=as.integer(p$long)),series=p$series[[1]])

rank_in <- rank(profit_in,ties.method="average")
rank_out <- rank(profit_out,ties.method="average")
bind <- cbind(param_total,profit_in,profit_out,rank_in,rank_out)
print("Parameters:")
print(params)
print("Profit:")
print(bind)


result_in <- bind[which(bind$profit_in==max(bind$profit_in)),]
result_out <- bind[which(bind$profit_out==max(bind$profit_out)),]
ifin <- function(x,y) ifelse(x %in% y,return(1),return(0))

para <- list(username = list(), periods = list(), ins = list(), out = list())
para$username <- username
para$periods <- lapply(getPeriods(username), as.integer)


series_in <- result_in$series[[1]]
series_out <- result_out$series[[1]]

ins <- list(short = result_in$short, medium = result_in$medium, long = result_in$long, series1 = as.integer(ifin(1,series_in)),
            series2 = as.integer(ifin(2,series_in)),series3 = as.integer(ifin(3,series_in)),
            series4 = as.integer(ifin(4,series_in)),profit = result_in$profit_in,rank_on_out= abs(result_in$rank_out-result_in$rank_in)+1)
out <- list(short = result_out$short, medium = result_out$medium, long = result_out$long, series1 = as.integer(ifin(1,series_out)),
            series2 = as.integer(ifin(2,series_out)),series3 = as.integer(ifin(3,series_out)),
            series4 = as.integer(ifin(4,series_out)),profit = result_out$profit_out,rank_on_in= abs(result_out$rank_in-result_out$rank_out)+1)

para$ins <- ins
para$out <- out
write(as.yaml(para), "sgrlu2.yaml")