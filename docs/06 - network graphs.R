# This exercise uses a smaller corpus (in directory docs-2)
# 
# The steps relating to the creation of the  the dtm  are almost identical to 
# those the first part of the previous class exercise. However, we recommend you
# run through the creation of the dtm again for practice
#
#Some additional tips:
# 1. Read the documentation for igraph. We are not even scratching the surface here.
#    http://igraph.org/r/ 
# 2. Code assumes you are already in the working directory set in the earlier exercise
#
rm(list=ls())
dev.off()
library(tm)
library(igraph) #you may need to install this
docs <- VCorpus(DirSource("./docs-2"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#Check number of docs loaded
print(docs)
#inspect a particular document
writeLines(as.character(docs[[20]]))
#Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)
#Remove stopwords from standard stopword list 
docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[20]]))
#Stem document
docs <- tm_map(docs,stemDocument)
#some clean up
#docs <- tm_map(docs, content_transformer(gsub),
#               pattern = "organiz", replacement = "organ")
#docs <- tm_map(docs, content_transformer(gsub),
#               pattern = "organis", replacement = "organ")
#docs <- tm_map(docs, content_transformer(gsub),
#               pattern = "andgovern", replacement = "govern")
#docs <- tm_map(docs, content_transformer(gsub),
#               pattern = "inenterpris", replacement = "enterpris")
#docs <- tm_map(docs, content_transformer(gsub),
#               pattern = "team-", replacement = "team")
#inspect
writeLines(as.character(docs[[20]]))
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
## start clustering specific code
#convert dtm to matrix (what format is the dtm stored in?)
m<-as.matrix(dtm)

#Map filenames to matrix row numbers
#these numbers will be used to reference files in the network graph
filekey <- cbind(1:length(docs),rownames(m))
write.csv(filekey,"filekey.csv",row.names = FALSE)
#have a look at file
rownames(m) <- 1:length(docs)
#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)

#adjacency matrix: set entries below a certain threshold to 0.
#We choose half the magnitude of the largest element of the matrix
#as the cutoff. This is an arbitrary choice
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)
#write to disk
write.csv(as.matrix(cs),file="AdjacencyMatrix.csv")
#open it and have a look

# build a graph from the above matrix
#mode is undirected because similarity is a bidirectional relationship
g <- graph.adjacency(as.matrix(cs), weighted=T, mode = "undirected")

#Plot a Graph
# set seed to make the layout reproducible
set.seed(42)
#one of many possible layouts, see igraph docs
layout1 <- layout.fruchterman.reingold(g)
#basic plot with no weighting - fruchtermann reingold weighting
plot(g, layout=layout1)
#another layout
plot(g, layout=layout.kamada.kawai)

#Community detection - Fast/Greedy
comm_fg <- fastgreedy.community(g)
comm_fg$membership
V(g)$color <- comm_fg$membership
plot(g, layout=layout.kamada.kawai)
community_mapping <- cbind(as.data.frame(filekey, row.names = F),comm_fg$membership)
community_mapping

#Community detection - Louvain
comm_lv <- cluster_louvain(g)
comm_lv$membership
V(g)$color <- comm_lv$membership
plot(g, layout=layout.kamada.kawai)
community_mapping <- cbind(community_mapping,comm_lv$membership)
community_mapping

#very similar clustering

#lets weight the nodes and edges
#set label (not really necessary)
#V=vertex, E=edge
V(g)$label <- V(g)$name
#Vertex size proportional to number of connections
V(g)$size <- degree(g)*.6
#Vertex label size proportional to number of connections
V(g)$label.cex <-  degree(g) / max(degree(g))+ .8
#label colour default black
V(g)$label.color <- "black"
#Vertex color organe
V(g)$color <- "orange"
#edge color grey
E(g)$color <- "grey"
#edge width proportional to similarity (weight)
E(g)$width <- E(g)$weight*7
# plot the graph in layout1 (fruchtermann reingold)
plot(g, layout=layout1)
#output is quite ugly. Explore igraph to see how you
#can fix it

#lets weight the nodes and edges
#set label (not really necessary)
#V=vertex, E=edge
V(g)$label <- V(g)$name
#Vertex size proportional to number of connections
V(g)$size <- degree(g)*.6
#Vertex label size proportional to number of connections
V(g)$label.cex <-  degree(g) / max(degree(g))+ .6
#label colour default black
V(g)$label.color <- "black"
#Vertex color organe
V(g)$color <- "orange"
#edge color grey
E(g)$color <- "grey"
#edge width proportional to similarity (weight)
E(g)$width <- E(g)$weight*5
# plot the graph in layout1 (fruchtermann reingold)
plot(g, layout=layout.auto)
#output is quite ugly. Explore igraph to see how you
#can fix it
plot(g, layout=layout1)
