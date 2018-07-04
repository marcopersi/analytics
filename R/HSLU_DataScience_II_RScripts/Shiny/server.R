####################################################
# Shiny demo - server
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
####################################################

library(shiny)
library(solr)
library(data.table)
library(bit64)
library(xts)
library(plotly)
library(ggplot2)
library(peacots)
library(igraph)
library(wordcloud2)
library(tm)
library(networkD3)

options(shiny.maxRequestSize=100*1024^2)
options(scipen = 999)

# Set the time zone at the system level
Sys.setenv(TZ="Europe/Zurich")

shinyServer(function(input, output, session) {
  
  # This function is responsible for loading manually selected files
  getCSVData <- reactive({
    infile <- input$csvfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    fread(infile$datapath, header = TRUE)
  })
  
  # Aggregation function for different time granularities
  aggTS <- function (aggLevel){
    rawdata <- getCSVData()
    rawdata$obs <- 1
    if (aggLevel==3600){
      aggLevelT = 'hours'
    }
    else if (aggLevel==86400){
      aggLevelT = 'days'
    }
    x.xts = xts(rawdata$obs, as.POSIXct(rawdata$createdAt, format('%Y-%m-%dT%H:%M:%SZ'), tz='CET'))
    aligned.xts <- align.time(x.xts, n=aggLevel)
    agg.xts <- period.apply(aligned.xts, endpoints(aligned.xts, aggLevelT, 1), sum)
    empty.xts <- xts(NULL,seq(start(aligned.xts),end(aligned.xts), by=aggLevel))
    out <- merge(empty.xts, agg.xts)
    out.df <- fortify(out)
    colnames(out.df) <- c("time", "obs")
    out.df[is.na(out.df)] <- 0
    return(out.df)
  }
  
  getCentralityGraphData <- function(dirGraph) {
    inputData <- fread (input = "../data/TweetsPegidaMentionGraph.csv" , header = TRUE)
    inputData$createdAt <- as.POSIXct(inputData$createdAt, format('%Y-%m-%dT%H:%M:%SZ'), tz='CET')
    inputData <- subset(inputData,!is.na(mentions))
    #select a temporal slice
    from <- input$dateRangeOfCentrality[1]
    to <- input$dateRangeOfCentrality[2]
    inputData <- subset(inputData, as.Date(createdAt)>=from & as.Date(createdAt)<=to)
    graphData <- NULL
    graphData <- graph.data.frame(subset(inputData, select=c('userScreenName','msn')), directed=dirGraph)
    graphData <- simplify(graphData, remove.multiple = FALSE, remove.loops = TRUE)
    graphData
  }
  
  getCommunityGraphData <- function() {
    inputData <- fread (input = "../data/TweetsPegidaMentionGraph.csv" , header = TRUE)
    inputData$createdAt <- as.POSIXct(inputData$createdAt, format('%Y-%m-%dT%H:%M:%SZ'), tz='CET')
    inputData <- subset(inputData,!is.na(mentions))
    #select a temporal slice
    from <- input$dateRangeOfCommunity[1]
    to <- input$dateRangeOfCommunity[2]
    inputData <- subset(inputData, as.Date(createdAt)>=from & as.Date(createdAt)<=to)
    inputData
  }
  
  getEdge.weights <- function(community, network, weight.within = 100, weight.between = 1) {
    bridges <- crossing(communities = community, graph = network)
    weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
    return(weights) 
  }
  
  output$csvData <- DT::renderDataTable({
    rawdata <- getCSVData()
    DT::datatable(rawdata)
  })
  
  # Plot the time series
  output$timeSeries <- renderPlotly({
    df.agg <- aggTS(as.integer(input$aggLevel))
    if (input$aggLevel==3600) {
      aggTime <- "hour"
    } 
    else if (input$aggLevel==86400) {
      aggTime <- "day"
    }
    p <- df.agg %>%
      plot_ly(x = ~time, y = ~obs, type = 'scatter', mode = "lines", line = list(shape = "spline", color = "#D95945")) %>%
      layout(xaxis = list(title = "time"), yaxis = list(title = paste("tweets / ", aggTime, sep = "")))
    p
  })
  
  # Plot a Lomb-Scargle periodogram
  output$plotLSSA <- renderPlotly({
    df.agg <- aggTS(3600)
    approx_obs <- na.approx(df.agg$obs)
    times  = seq(1,length(df.agg$obs),1);
    signal = df.agg$obs;
    report = evaluate.pm.wn(times=times, signal=signal);
    df.lssa <- data.frame(ts(1/report$frequencies),ts(report$periodogram))
    q <- ggplot(df.lssa, aes(x = df.lssa[,1], y = df.lssa[,2])) + 
      theme_bw() + 
      geom_line(colour = "#D95945") + 
      labs(x="hours",y="power") +
      scale_x_continuous(limits=c(1,30),breaks=seq(1, 30, by = 2))
    ggplotly(q)
  })
  
  # Calculate degree centrality
  getAllDegreeData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(TRUE)
    deg <- degree(graph.data, mode="all")
    df.deg.centrality <- as.data.frame(deg)
    setDT(df.deg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
    names(df.deg.centrality)[names(df.deg.centrality)=="rn"] <- "node"
    names(df.deg.centrality)[names(df.deg.centrality)=="deg"] <- "degree"
    df.deg.centrality <- df.deg.centrality[order(df.deg.centrality$degree,decreasing = TRUE),]
    DT::datatable(df.deg.centrality)
  })
  
  output$allDegreeData <- DT::renderDataTable({
    getAllDegreeData()
  })
  
  # Calculate in-degree centrality
  getInDegreeData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(TRUE)
    indeg <- degree(graph.data, mode="in")
    df.indeg.centrality <- as.data.frame(indeg)
    setDT(df.indeg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
    names(df.indeg.centrality)[names(df.indeg.centrality)=="rn"] <- "node"
    names(df.indeg.centrality)[names(df.indeg.centrality)=="indeg"] <- "indegree"
    df.indeg.centrality <- df.indeg.centrality[order(df.indeg.centrality$indegree,decreasing = TRUE),]
    DT::datatable(df.indeg.centrality)
  })
  
  output$inDegreeData <- DT::renderDataTable({
    getInDegreeData()
  })
  
  # Calculate out-degree centrality
  getOutDegreeData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(TRUE)
    outdeg <- degree(graph.data, mode="out")
    df.outdeg.centrality <- as.data.frame(outdeg)
    setDT(df.outdeg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
    names(df.outdeg.centrality)[names(df.outdeg.centrality)=="rn"] <- "node"
    names(df.outdeg.centrality)[names(df.outdeg.centrality)=="outdeg"] <- "outdegree"
    df.outdeg.centrality <- df.outdeg.centrality[order(df.outdeg.centrality$outdegree,decreasing = TRUE),]
    DT::datatable(df.outdeg.centrality)
  })
  
  output$outDegreeData <- DT::renderDataTable({
    getOutDegreeData()
  })
  
  # Calculate betweenness centrality
  getBtwCentralityData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(FALSE)
    btw <- betweenness(graph.data, v = V(graph.data), directed = TRUE, weights = NULL, nobigint = FALSE, normalized = TRUE)
    df.btw.centrality <- as.data.frame(btw)
    setDT(df.btw.centrality, keep.rownames = TRUE)[]
    names(df.btw.centrality)[names(df.btw.centrality)=="rn"] <- "node"
    names(df.btw.centrality)[names(df.btw.centrality)=="btw"] <- "vertexbetweenness"
    df.btw.centrality <- df.btw.centrality[order(df.btw.centrality$vertexbetweenness,decreasing = TRUE),]
    DT::datatable(df.btw.centrality)
  })
  
  output$btwCentralityData <- DT::renderDataTable({
    getBtwCentralityData()
  })
  
  # Calculate closeness centrality
  getCloCentralityData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(TRUE)
    clo <- closeness(graph.data, mode="all", normalized = TRUE)
    df.clo.centrality <- as.data.frame(clo)
    setDT(df.clo.centrality, keep.rownames = TRUE)[]
    names(df.clo.centrality)[names(df.clo.centrality)=="rn"] <- "node"
    names(df.clo.centrality)[names(df.clo.centrality)=="clo"] <- "closeness"
    df.clo.centrality <- df.clo.centrality[order(df.clo.centrality$closeness,decreasing = TRUE),]
    DT::datatable(df.clo.centrality)
  })
  
  output$cloCentralityData <- DT::renderDataTable({
    getCloCentralityData()
  })
  
  # Calculate Eigenvector centrality
  getEvcCentralityData <- eventReactive(input$go1Button, {
    graph.data <- getCentralityGraphData(FALSE)
    evc <- eigen_centrality(graph.data, directed = TRUE, scale = TRUE, weights = NULL)
    df.evc.centrality <- as.data.frame(evc)
    setDT(df.evc.centrality, keep.rownames = TRUE)[]
    names(df.evc.centrality)[names(df.evc.centrality)=="rn"] <- "node"
    names(df.evc.centrality)[names(df.evc.centrality)=="vector"] <- "eigenvectorcentrality"
    df.evc.centrality <- df.evc.centrality[order(df.evc.centrality$eigenvectorcentrality,decreasing = TRUE),]
    df.evc.centrality <- subset(df.evc.centrality, select=c("node","eigenvectorcentrality"))
    DT::datatable(df.evc.centrality)
  })
  
  output$evcCentralityData <- DT::renderDataTable({
    getEvcCentralityData()
  })
  
  # Calculate a community graph, here using the Louvain algorithm
  getCommunityGraph <- eventReactive(input$go2Button, {
    rawData <- getCommunityGraphData()
    
    graph.undir.data <- NULL
    graph.undir.data <- graph.data.frame(subset(rawData, select=c('userScreenName','msn')), directed=F)
    graph.undir.data <- simplify(graph.undir.data, remove.multiple = FALSE, remove.loops = TRUE)
    
    graph.undir.data <- delete.vertices(graph.undir.data,which(degree(graph.undir.data, mode="all")<2))
    
    # Apply the community detection algorithm
    cl <- cluster_louvain(graph.undir.data)
    members <- membership(cl)
    
    communitySizes <- sort(sizes(cl),decreasing = TRUE)
    communitySizes
    names(communitySizes)
    
    maxCommunitySize <- 10
    topNodes <- as.integer(names(which(communitySizes>maxCommunitySize)))
    topNodes
    
    for (i in 1:length(members)) {
      if (members[[i]] %in% topNodes) {
        members[[i]] <- which(topNodes == members[[i]])
      } else {
        members[[i]] <- length(topNodes) +1
      }
    }
    members
    sort(members)
    # Transform into a D3 object for visualization
    graphData_d3 <- igraph_to_networkD3(graph.undir.data, group = members)
    forceNetwork(Links = graphData_d3$links, Nodes = graphData_d3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', fontSize = 10, opacity = 0.9, linkDistance = 1, legend=TRUE, zoom = TRUE)
  })
  
  output$communityGraph <- renderForceNetwork({
    getCommunityGraph()
  })
  
})
