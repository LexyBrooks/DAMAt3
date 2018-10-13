# The steps relating to the creation of the  the dtm  are almost identical to 
# those the first part of the previous class exercise. However, we recommend you
# run through the creation of the dtm again for practice
#
#Some additional tips:
# 1. Read the documentation for kmeans clustering algorithms:
#    https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
#  2. There are many options for algorithms, we'll use the default (Hartigan Wong) method for kmeans. See 
#     http://stackoverflow.com/questions/20446053/k-means-lloyd-forgy-macqueen-hartigan-wong
#     for details of the different methods if you're interested
# 3. Code assumes you are already in the working directory set in the earlier exercise
#
rm(list=ls())
dev.off()
library(tm)
docs <- VCorpus(DirSource("./docs-1"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#Check number of docs loaded
print(docs)
#inspect a particular document
writeLines(as.character(docs[[30]]))
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
writeLines(as.character(docs[[30]]))
#Stem document
docs <- tm_map(docs,stemDocument)
#inspect
writeLines(as.character(docs[[30]]))
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
## start clustering specific code
#convert dtm to matrix (what format is the dtm stored in?)
m<-as.matrix(dtm)
#write as csv file
write.csv(m,file="dtmAsMatrix.csv")
#shorten rownames for display purposes
#rownames(m) <- paste(substring(rownames(m),1,7),rep("..",nrow(m)),
#                   substring(rownames(m),
#                               nchar(rownames(m))-7,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)

#kmeans clustering
#kmeans - run with nstart=100 and k=2,3,5 to compare results with hclust
kfit <- kmeans(d, 2, nstart=100)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

#rerun using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

kfit <- kmeans(cd, 2, nstart=100)

clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster
#write clusters to csv file
write.csv(kfit$cluster,file="KMClustGroups2_cosine.csv")
#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss
#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 