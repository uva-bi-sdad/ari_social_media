# This code snippet is for making a radar chart 
# chartJSRadar wants a table with a column of categorical data labels on the 
# left, and n named columns of values on the right, it can take a 3 by n 
# matrix with rgb values as vectors to color each value column on the chart
library(radarchart)
library(jsonlite)

sumSents1 <- sumSents[2:1]
sdalcolor <- grDevices::col2rgb("#98C893")
chartJSRadar(scores = sumSents1,labelSize = 60)
