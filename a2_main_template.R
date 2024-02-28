############################################################################
# This file is setup to use:
# - the data for assignment 2 (a2)
# - the a2_strategy_template
# You can use this file to test a single parameter combination
# (for which you should set params, start_period and end_period as required)
# In particular you can use this file to test your implementation of getOrders 
# (when you have done it) with the examples in a2.pdf
# For creating results.yaml, see the hints in a2.pdf
############################################################################
source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')

# Read in data -- here with the A2 direction; subset it as required
dataList <- getData(directory="EXAMPLE")
# subset data: choose the period to run on 

# Choose strategy -- this should be called strategy.R when you submit it
strategyFile <- 'strategies/strategy.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

# Strategy parameters -- this will not be an empty list when you are done
params <- list(lookbacks=list(short=as.integer(5),medium=as.integer(50),long=as.integer(100)),series=1:4)

print("Parameters:")
print(params)

# Do backtest
results <- backtest(dataList,getOrders,params,sMult=0.2)
plotResults(dataList,results)
cat("Profit:", results$aggProfit, '\n')
