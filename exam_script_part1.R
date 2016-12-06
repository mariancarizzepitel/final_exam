library(tidyverse)
library(apaTables)
library(pwr)
library(MBESS)

# INCREMENTAL PREDICTION IN MR -------------------------------------------
# when you think that 1 key predictor will account for 2% of variance, above
# and beyond other 2 predictors. Determine N to get power of .80 for detecting 
# this increment in variance. 

# Step 1: get F2 
my.f2.increment <- .10/(1-.20) # sr2/(1-R2) #f2=.125
print(my.f2.increment)

#Step 2: calculate v 
pwr.f2.test(u=1,f2=.125,power=.85) #u = 1 b/c only interested in incremental prediction of 1 variable 
# v = 72

#Step 3: calculate N 
#N = u + v + 1 = 2 + 72 +1 = 75

