
# rounding functions for funl_data
# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x

roundup_Nice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {

    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


rounddown_Nice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {

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

