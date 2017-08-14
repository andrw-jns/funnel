
# rounding functions for funl_Data

roundup_Nice <- function(x, nice = c(1, 2, 5, 10)) {

    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


rounddown_Nice <- function(x, nice = c(1, 2, 5, 10)) {

    10^floor(log10(x)) * nice[[which(x >= 10^floor(log10(x)) * nice)[[1]]]]
}

# example
# for (i in seq(0, 200, 5)[-1]) {
#      print(roundup_Nice(i))
#   }
# 
# for (i in seq(0, 200, 5)[-1]) {
#      print(rounddown_Nice(i))
#   }

