
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



# does it work ----------------------------------------------------------------
funnelPlotData <- funl_Data(testData, col.unit = "practice", col.group = "grp", col.O = "O", col.n = "n", col.rt = "rt"
                            , target = NULL, smoothness = 100, fnlMinEvents = NULL, fnlMaxEvents = NULL)

# preview result
funnelPlotData

# a quick plot
plotFunnels <- funnelPlotData[[1]]
plotUnits <- funnelPlotData[[2]]
plotUnits <- plotUnits %>% 
  left_join(testData, "practice")

funlPlot <-
  ggplot(plotFunnels) +
  geom_line(aes(x = n, y = fnlLow, group = fnlLimit), linetype = "44") +
  geom_line(aes(x = n, y = fnlHigh, group = fnlLimit), linetype = "44") +
  geom_line(aes(x = n, y = target)) +
  #geom_hline(aes(yintercept = target)) +
  geom_point(aes(x = n, y = rt, color = status), data = plotUnits)
print(funlPlot)

