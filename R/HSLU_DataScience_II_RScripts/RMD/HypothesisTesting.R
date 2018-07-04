####################################################
# RMD Test code
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
####################################################

library(earlywarnings)
library(plotly)

df.data <- read.csv('data/TweetsUkraine.csv')
df.data$time <- as.POSIXct(as.numeric(as.character(df.data$time)),origin="1970-01-01",tz="CET")

# plot the raw data
p <- df.data[1:600,] %>%
  plot_ly(x = ~time, y = ~obs, type = 'scatter', mode = "lines", line = list(shape = "spline", color = "#D95945")) %>%
  layout(xaxis = list(title = "time"), yaxis = list(title = "tweets / hour"))
p

ews <- generic_ews(df.data[1:600,1:2], winsize=10, detrending = 'gaussian', powerspectrum = FALSE)
ews$timeindex <- as.POSIXct(ews$timeindex, origin="1970-01-01", tz="CET")
df.m <- merge(ews, df.data, by.x = "timeindex", by.y = "time")

#plotly
vars <- setdiff(names(df.m), c("timeindex","kurt","cv","returnrate","densratio","acf1"))
vars <- vars[c(4,2,1,3)] # reorder the vector
plots <- lapply(vars, function(var) {
  plot_ly(df.m, x = ~timeindex, y = as.formula(paste0("~", var)), name = var) %>%
    add_lines()
})
subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

