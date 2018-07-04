#########################################
# Visualization
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
#########################################

############################
# Basic plotting
############################
mySequence <- seq(1,10,0.5)
length(mySequence)
mySequence[6]
mySequence[6] <- mySequence[6]+mySequence[7]

mySequence           # numeric visualisation
plot(mySequence)     # graphical visualization

# The iris data set
iris.data <- iris
plot(iris.data)


####################################################################################
# R Lattice graphs
# Sources:
# e.g.: http://www.statmethods.net/advgraphs/trellis.html
# e.g.: https://science.nature.nps.gov/im/datamgmt/statistics/r/graphics/lattice.cfm
####################################################################################
library(lattice)

set.seed(123)
rnd.data <- data.frame (var1 = rnorm (60, 10, 5), var2 = rnorm (60, 8,2), var3 = rnorm (60, 50, 10), var4 = rnorm (60, 15, 10), group = rep(c ("A", "B", "C"), each = 20))
parallelplot(~rnd.data[1:4], data = rnd.data, groups=group, pscales = 0)

iris.data <- iris
parallelplot(~iris.data[1:4], data = iris.data, groups=Species)


#################################################################
# ggplot2 graphs after preprocessing
# Sources:
# e.g.: http://r4ds.had.co.nz/data-visualisation.html
# e.g.: http://www.r-graph-gallery.com/portfolio/ggplot2-package/
#################################################################
library(ggplot2)
library(readr)
library(data.table)
library(xts)


# Load Twitter data (two methods to compare)
now <- Sys.time()
inputData <- read_csv('data/TweetsPegida.csv')
loadingTime <- Sys.time() - now
loadingTime

# Recommended load method for very large files
now <- Sys.time()
inputData <- fread('data/TweetsPegida.csv')
loadingTime <- Sys.time() - now
loadingTime

# Set the time zone at the system level
Sys.setenv(TZ="Europe/Zurich")

# Transform the input data to get a time series of hourly aggregated tweets
class(inputData$createdAt)
inputData$createdAt <- as.POSIXct(inputData$createdAt, '%Y-%m-%dT%H:%M:%SZ', tz='CET')

# Define a new data frame
df.myData <- data.frame(time=inputData$createdAt)
df.myData$obs <- 1

# Aggregate the observations per hour
x.xts = xts(df.myData$obs, df.myData$time)
aligned.xts <- align.time(x.xts, n=3600)
agg.xts <- period.apply(aligned.xts, endpoints(aligned.xts, 'hours', 1), sum)
empty.xts <- xts(NULL,seq(start(aligned.xts),end(aligned.xts), by=3600))
out <- merge(empty.xts, agg.xts)
out.df <- fortify(out)
colnames(out.df) <- c("time", "obs")
out.df[is.na(out.df)] <- 0

ggplot(out.df, aes(x = time, y = obs)) + 
  theme_bw() + 
  geom_line(colour = "#D95945") + 
  labs(x="Time",y="# Tweets per hour")

##################
# Plotly R Library
# Sources:
# https://plot.ly/r/
##################
library(plotly)

# Transform the previous ggplot into a plotly object
p <- ggplot(out.df, aes(x = time, y = obs)) + 
  theme_bw() + 
  geom_line(colour = "#D95945") + 
  labs(x="Time",y="# Tweets per hour")
ggplotly(p)

# Straight visualization using plotly
p <- out.df %>%
  plot_ly(x = ~time, y = ~obs, type = 'scatter', mode = "lines", line = list(shape = "spline", color = "#D95945")) %>%
  layout(xaxis = list(title = "time"), yaxis = list( title = "tweets / hour"))
p

#####################################
# Plotly choropleth precipitation map
#####################################

df <- read.csv('data/2015_06_30_precipitation.csv')
# HRAP (Hydrologic Rainfall Analysis Project) is a grid coordinate system used within the National Weather Service.

# Change default color scale title
m <- list(colorbar = list(title = "Total [mm] Rain"))

# Geo styling
g <- list(
  scope = 'north america',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(lon = -100)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(20, 60),
    dtick = 5
  )
)

p <- plot_geo(df, lat = ~Lat, lon = ~Lon, color = ~Globvalue) %>%
  add_markers(
    text = ~paste(df$Globvalue*25.4, "mm"), hoverinfo = "text"
  ) %>%
  layout(title = 'US Precipitation 06-30-2015<br>Source: NOAA', geo = g)
p


############################
# Plotly with a map subgraph
############################

df <- read.csv('data/2014_ebola.csv')

# Restrict from June to September
df <- subset(df, Month %in% 6:9)

# Ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])

# September totals
df9 <- subset(df, Month == 9)

# Common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)

df %>%
  plot_geo(
    locationmode = 'country names', sizes = c(1, 600), color = I("black")
  ) %>%
  add_markers(
    y = ~Lat, x = ~Lon, locations = ~Country,
    size = ~Value, color = ~abbrev, text = ~paste(Value, "cases")
  ) %>%
  add_text(
    x = 21.0936, y = 7.1881, text = 'Africa', showlegend = F, geo = "geo2"
  ) %>%
  add_trace(
    data = df9, z = ~Month, locations = ~Country,
    showscale = F, geo = "geo2"
  ) %>%
  layout(
    title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
    geo = g1, geo2 = g2
  )


##########################
# networkD3 graph plotting
##########################
library(igraph)
library(networkD3)

# Use igraph to make the graph and find membership
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)

# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)

# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 12, opacity = 0.9)

