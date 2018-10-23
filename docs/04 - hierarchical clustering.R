# The steps relating to the creation of the  the dtm  are almost identical to 
# those the first part of the previous class exercise. However, we recommend you
# run through the creation of the dtm again for practice
#
#Some additional tips:
# 1. Read the documentation for hclust and kmeans clustering algorithms:
#    https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html and
#    https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html
#  2. There are many options for both, we'll use Ward's method for hclust and the
#     default (Hartigan Wong) method for kmeans. See 
#     http://stackoverflow.com/questions/20446053/k-means-lloyd-forgy-macqueen-hartigan-wong
#     for details of the different methods if you're interested
# 3.  Code assumes you are already in the working directory set in the earlier exercise
#
rm(list=ls())
dev.off()
library(tm)
docs <- VCorpus(DirSource("./docs-1"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#Check number of docs loaded
print(docs)
#inspect a particular document (another way to do this)
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
#rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
#                     substring(rownames(m),
#                               nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)
#run hierarchical clustering using Ward's method (explore other options later)
groups <- hclust(d,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 2 subtrees. Try 3,4,5,6 cuts; comment on your results
rect.hclust(groups,2)
hclusters <- cutree(groups,2)
write.csv(hclusters,"hclusters.csv")

dev.off()
#try another distance measure
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

#run hierarchical clustering using cosine distance
groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)
#cut into 2 subtrees.
rect.hclust(groups,2)
hclusters_cosine <- cutree(groups,2)
write.csv(hclusters_cosine,"hclusters_cosine.csv")

#compare dendrograms obtained using the two distance measures.
