#
#
# TO DO.R
# -------
#

# Adding a line for a commit to test the github sync...
# adding another line to test the github commit and sync

#-----------------------------------------
#  Earnings project to do
#-----------------------------------------
#  .Finalize Feature Set Stability Validation with both
#     RF and xgboost.  Ensure can iterate over multiple timeframes
#  .Upgrade assess_features to include xgboost
#  .Run and report stability validation.
#  .Release scripts and package to github

#
# I think this is what I used when I implemented UPI.  Note there
# is both Ulcer Index and Ulcer Perfomance Index.
#
# https://en.wikipedia.org/wiki/Ulcer_index
#
# Don

# Ulcer Performance Index which is CAGR (or sum(Rets)) / UI, annualized.
# I much prefer UPI since it is a risk adjusted gain, analogous to Sharpe, Sortino, and MAR.


# I have already modified it to pass the date range information and
# added to the return list. Since I plan to call Don’s modelEC inside
# Assess_Features, I going to pass the idcols columns to Assess_Features
# so all dates and other information will be present for that and other TBD uses.
# It may be premature to do that for the package version unless you want to
# have the package version of Assess_Features return the equity curve


#########################################################################
#  .later: function to load/verify xls ML hyper parameters with defaults
#  .document each key function in package
#  .document vignette - basic docs
#  .add stoplevel argument as high water mark
#  .
#  .read hadley's section on publishing a package
#  .learn drat package
#
#
#  .expand maxcreen to include momentum ranks

library(beepr)

for(i in 1:11) {
  print(i)
  beep(i)
  Sys.sleep(4)
}
