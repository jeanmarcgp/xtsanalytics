#
#  xtsanalytics scratchpad.R
#
#  Script to capture some package build commands, including
#  high level messing around tests that are not part of testing
#  the package.  This will not be included in the package source.
#
#

library(devtools)
# Add packages to the DESCRIPTION
devtools::use_package("ggplot2")

# Create a basic vignette
devtools::use_vignette("xtsanalytics-vignette")

# Convert roxygen comments into .Rd document  <ctrl <shift> D
devtools::document()

# Do a full build, including the vignette
devtools::build()
devtools::install(build_vignettes = TRUE)

#  Make xts_GSPC dataset
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

# Create a folder and set up package for testthat.
devtools::use_testthat()


library(xts)

x <- funapply(xts_data[,1], FUN='sd', windows=c(20, 40, 100, 200))

x <- make_bench(xts_data[, 1])



######
#  Test funapply with multiple arguments
fn <- function(data, arg1) {
  retval <- mean(data)
  if(arg1) retval <- retval + 10
  return(retval)
}


vec <- c(1,2,3,4,5,6,7,8,9)
for

data <- xts(vec, order.by=index(xts_data[1:length(vec)]))
mean(data)

funapply(data, FUN=mean, windows=4)

fn(data, arg1=TRUE)
fn(data, arg1=FALSE)

funapply(data, FUN=fn, windows=4, arg1=FALSE)
funapply(data, FUN=fn, windows=4, arg1=TRUE)


#  code to test sd_uni
data <- ROC(xts_data[1:20,1:2], type="discrete")
colnames(data) <- c('SPY_rets', 'VTI_rets')
test <- data
test$sduni <- funapply(test$SPY_rets, FUN = sd_uni, windows=5, posvals=TRUE)
test$sduni2 <- funapply(test$SPY_rets, FUN = sd_uni, windows=5, posvals=FALSE)






