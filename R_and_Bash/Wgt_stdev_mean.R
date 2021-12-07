# source1: https://www.statology.org/weighted-standard-deviation-in-r/
# source2: https://rdrr.io/cran/Hmisc/man/wtd.stats.html
library(Hmisc); package_version(Hmisc)

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# Calculate Weighted Standard Deviation ----
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
x <- c(14, 19, 22, 25, 29, 31, 31, 38, 40, 41) #define data values
wt <- c(1, 1, 1.5, 2, 2, 1.5, 1, 2, 3, 2)      #define weights
weighted_var <- wtd.var(x, wt)                 #calculate weighted variance 
wtd_stdv <- sqrt(weighted_var)                 #calculate weighted standard deviation

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# Calculate Weighted Mean ----
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
x <- c(14, 19, 22, 25, 29, 31, 31, 38, 40, 41) #define data values
wt <- c(1, 1, 1.5, 2, 2, 1.5, 1, 2, 3, 2)      #define weights
wtd_avg <- wtd.mean(x, weights=wt)             #calculate weighted mean

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# Calculate Rounded z-scores in R
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# (x - avg)/ stdv
zscore <- round( (x - wtd_avg) / wtd_stdv, 0)

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# Making them into a Table
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
wtd_TblOut <- data.frame(
  Vals = x,
  Wgts = wt,
  wStd = wtd_stdv,
  wAvg = wtd_avg,
  zScr = zscore
)

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# Weighted Standard Deviation for Multiple Columns of Data Frame ----
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
#define data frame
df <- data.frame(team=c('A', 'A', 'A', 'A', 'A', 'B', 'B', 'C'),
                 wins=c(2, 9, 11, 12, 15, 17, 18, 19),
                 points=c(1, 2, 2, 2, 3, 3, 3, 3))
wt <- c(1, 1, 1.5, 2, 2, 1.5, 1, 2)           #define weights
sapply(df[c('wins', 'points')], 
       function(x) sqrt(wtd.var(x, wt)))      # weighted stdev of points & wins

#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# using the describe function ----
#+~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
# describe uses wtd.mean, wtd.quantile, wtd.table
x <- c(14, 19, 22, 25, 29, 31, 31, 38, 40, 41) #define data values
wt <- c(1, 1, 1.5, 2, 2, 1.5, 1, 2, 3, 2)      #define weights
describe(~x, weights = wt)
