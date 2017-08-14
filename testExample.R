
# test example for funl_Data

library(testthat)  # test_that
# confirm correct library path
test_that('correctlibPath', {
  expect_equal(.libPaths()
               , 'C:/Program Files/R/R-3.4.1/library')
})

# required packages
library(tidyverse)




# 1 setup options -------------------------------------------------------------
baseDir <- c("C:/Projects")
projectDir <- paste0(baseDir, "/exFunnel")

setwd(projectDir)

# source funlData
source("./funlData.R")




# 2 read test data -----------------------------------------------------------
testData <- read.csv("testData.csv"
                     , header = TRUE
                     , stringsAsFactors = FALSE)


