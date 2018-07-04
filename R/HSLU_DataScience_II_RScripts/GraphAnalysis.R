###########################################
# A basic graph analysis: igraph, networkD3
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
###########################################
library(igraph)
library(networkD3)
library(data.table)

# Twitter data for mention graph
data <- fread(input = 'data/TweetsPegidaMentionGraph.csv')

# Transform time stamps from strings to POSIXct
class(data$createdAt)
data$createdAt <- as.POSIXct(data$createdAt, '%Y-%m-%dT%H:%M:%SZ', tz='CET')
class(data$createdAt)

# Select one day of the Twitter sample
sample.data <- subset(data, as.Date(createdAt)==as.Date("2016-10-11"))
# Remove all rows, i.e. tweets, without mentioning
sample.data <- subset(sample.data, !is.na(mentions))

# Build a graph object
graph.dir.data <- graph.data.frame(subset(sample.data, select=c('userScreenName','msn')), directed=T)
graph.dir.data <- simplify(graph.dir.data, remove.multiple = FALSE, remove.loops = TRUE)

graph.undir.data <- graph.data.frame(subset(sample.data, select=c('userScreenName','msn')), directed=F)
graph.undir.data <- simplify(graph.undir.data, remove.multiple = FALSE, remove.loops = TRUE)

# Calculate different centrality measures: degree
deg <- degree(graph.dir.data, mode="all")
deg
df.deg.centrality <- as.data.frame(deg)
setDT(df.deg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
names(df.deg.centrality)[names(df.deg.centrality)=="rn"] <- "node"
names(df.deg.centrality)[names(df.deg.centrality)=="deg"] <- "degree"
df.deg.centrality <- df.deg.centrality[order(df.deg.centrality$degree,decreasing = TRUE),]
DT::datatable(df.deg.centrality)

# In-degree
indeg <- degree(graph.dir.data, mode="in")
df.indeg.centrality <- as.data.frame(indeg)
setDT(df.indeg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
names(df.indeg.centrality)[names(df.indeg.centrality)=="rn"] <- "node"
names(df.indeg.centrality)[names(df.indeg.centrality)=="indeg"] <- "indegree"
df.indeg.centrality <- df.indeg.centrality[order(df.indeg.centrality$indegree,decreasing = TRUE),]
DT::datatable(df.indeg.centrality)

# Out-degree
outdeg <- degree(graph.dir.data, mode="out")
df.outdeg.centrality <- as.data.frame(outdeg)
setDT(df.outdeg.centrality, keep.rownames = TRUE)[] # set the rownames (rn) into a separate column
names(df.outdeg.centrality)[names(df.outdeg.centrality)=="rn"] <- "node"
names(df.outdeg.centrality)[names(df.outdeg.centrality)=="outdeg"] <- "outdegree"
df.outdeg.centrality <- df.outdeg.centrality[order(df.outdeg.centrality$outdegree,decreasing = TRUE),]
DT::datatable(df.outdeg.centrality)

# Betweenness centrality
btw <- betweenness(graph.undir.data, v = V(graph.undir.data), directed = TRUE, weights = NULL, nobigint = FALSE, normalized = TRUE)
df.btw.centrality <- as.data.frame(btw)
setDT(df.btw.centrality, keep.rownames = TRUE)[]
names(df.btw.centrality)[names(df.btw.centrality)=="rn"] <- "node"
names(df.btw.centrality)[names(df.btw.centrality)=="btw"] <- "vertexbetweenness"
df.btw.centrality <- df.btw.centrality[order(df.btw.centrality$vertexbetweenness,decreasing = TRUE),]
DT::datatable(df.btw.centrality)

# Closeness centrality
clo <- closeness(graph.dir.data, mode="all", normalized = TRUE)
df.clo.centrality <- as.data.frame(clo)
setDT(df.clo.centrality, keep.rownames = TRUE)[]
names(df.clo.centrality)[names(df.clo.centrality)=="rn"] <- "node"
names(df.clo.centrality)[names(df.clo.centrality)=="clo"] <- "closeness"
df.clo.centrality <- df.clo.centrality[order(df.clo.centrality$closeness,decreasing = TRUE),]
DT::datatable(df.clo.centrality)

# Eigenvector centrality
evc <- eigen_centrality(graph.undir.data, directed = TRUE, scale = TRUE, weights = NULL)
df.evc.centrality <- as.data.frame(evc)
setDT(df.evc.centrality, keep.rownames = TRUE)[]
names(df.evc.centrality)[names(df.evc.centrality)=="rn"] <- "node"
names(df.evc.centrality)[names(df.evc.centrality)=="vector"] <- "eigenvectorcentrality"
df.evc.centrality <- df.evc.centrality[order(df.evc.centrality$eigenvectorcentrality,decreasing = TRUE),]
df.evc.centrality <- subset(df.evc.centrality, select=c("node","eigenvectorcentrality"))
DT::datatable(df.evc.centrality)

# Graphical representation
# Apply first a community detection algorithm and transform into a D3 object for visualization
cl <- cluster_louvain(graph.undir.data)
members <- membership(cl)
graphData_d3 <- igraph_to_networkD3(graph.undir.data, group = members)
forceNetwork(Links = graphData_d3$links, Nodes = graphData_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 12, opacity = 0.9, linkDistance = 1, legend=TRUE, zoom=TRUE)

# Improve the graph visualization
# Remove all nodes with degrees smaller than k (e.g. k=2)
graph.undir.data <- delete.vertices(graph.undir.data,which(degree(graph.undir.data, mode="all")<2))

# Apply a community detection algorithm
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


# Build wordclouds per community
library(tm)
library(wordcloud2)

comm <- as.integer(names(communitySizes)[1]) # the biggest community
uids <- as.data.frame(cl[comm])[,1]
comm.uids <- subset(sample.data, c(userScreenName) %in% uids) 
comm.uids <- comm.uids[!duplicated(comm.uids$id),] # remove duplicated entries in denorm data table

myCorpus = Corpus(VectorSource(as.vector(comm.uids$text)))

#Remove unnecessary words & convert to lowercase:
myCorpus <- tm_map(myCorpus, removePunctuation) 
myCorpus <- tm_map(myCorpus, removeNumbers)   
myCorpus <- tm_map(myCorpus, tolower)     
myCorpus <- tm_map(myCorpus, removeWords, stopwords("german"))   
myCorpus <- tm_map(myCorpus, removeWords, c("pegida"))

dtm <- DocumentTermMatrix(myCorpus)   

#organizes the terms by their frequency:
freq <- colSums(as.matrix(dtm))   

# Plot words that appear at least 200 times.
wf <- data.frame(word=names(freq), freq=freq) 

# Plot words that occur at 150 times.
wordcloud2(subset(wf,freq>1), color = "random-light", backgroundColor = "darkseagreen")

