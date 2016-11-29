#
#  CREATE DATASETS.R
#
#  Script to create the datasets embedded in xtsanalytics.
#
#
#
library(xtsanalytics)

#-----------------------------------
# Create Earnings dataset
#-----------------------------------
rdatafile    <- "./ignore-rawdata/Earnings Dataset.Rdata"
timeframe    <- "2011-01-01/2012-12-31"
datecol      <- "dtBuy"

sprint("Loading dataset from file:       %s", rdatafile)
load(rdatafile)  # storing data in dataframe df

Earnings     <- subset_dfdates(df, timeframe = timeframe, datecol = datecol, keep_NAs = TRUE)

#save(Earnings, file = "./Results/Earnings.Rdata")

devtools::use_data(Earnings)

stop("Done Creating Earnings Dataset")

#-----------------------------------
#  Make xts_GSPC dataset
#-----------------------------------
library(quantmod)
getSymbols('^GSPC', from="1950-01-01", to= "2014-12-31")
xts_gspc <- Ad(GSPC)
colnames(xts_gspc) <- 'GSPC'


#  Make xts_data and xts_alldata datasets
symbols <- c('SPY', 'VTI', 'BND', 'VNQ', 'QQQ', 'VIG', 'IEV', 'EWN', 'MDY',
             'XLY', 'XLP', 'XLE', 'XLF', 'XLV', 'XLI', 'XLB', 'XLK', 'XLU')
getSymbols(symbols, from="2007-01-01", to= "2014-12-31")
prices <- list()
for(i in seq_along(symbols)) { prices[[i]] <- Ad(get(symbols[i])) }

xts_alldata <- do.call(cbind, prices)
colnames(xts_alldata) <- symbols

xts_data <- xts_alldata[, 1:8]

# Use save to the data while keeping the xts structure
save(xts_data, file = './data/xts_data.RData')
save(xts_alldata, file = './data/xts_alldata.RData')
save(xts_gspc, file = './data/xts_gspc.RData')


xtsplot(xts_data, type="regular", bench='QQQ.Adjusted')
xtsplot(xts_data, type="spaghetti", bench='QQQ.Adjusted')
xtsplot(xts_data, normalize=TRUE, main="some plot")
xtsplot(xts_data, type="regular", bench='EWN.Adjusted')

xtsplot(xts_data, type="regular", bench='EWN.Adjusted', filepath="./ignore-results/")
xtsplot(xts_data, type="spaghetti", bench='EWN.Adjusted', filepath="./ignore-results/",
        main="my cool plot")




# The use_data command loses the xts meta information.
# So don't use it for xts information. Use save instead.
#devtools::use_data(sp500data)



