#uses docs-1 corpus
# Check out the lsa documentation:
#https://cran.r-project.org/web/packages/lsa/lsa.pdf 

rm(list=ls())
#set working directory
getwd() 
setwd("C:/Users/<Your Username>/Documents/textmining") #change path as needed
getwd() #check that you're where you should be
library(tm) 
library(lsa)
#Let's go. Load corpus... 
docs <- VCorpus(DirSource("./docs-1"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

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

#Create term-document matrix (lsa expects a TDM rather than a DTM)
tdm <- TermDocumentMatrix(docs)
#summary
tdm
#inspect segment of document term matrix
inspect(tdm[1000:1006,1:10])

#what kind of object is the tdm?
class(tdm)
#note: a simple triplet matrix (STM) is an efficient format to store
#sparse matrices
#need to convert STM to regular matrix
#convert to regular matrix
tdm.matrix <- as.matrix(tdm)
#check class
class(tdm.matrix)
dim(tdm.matrix)

#weight terms and docs

#Note: We weight the TDM before calculating the latent semantic space.
#This is to better reflect the relative importance of each term/doc 
#in relation to the entire corpus (much like tf-idf weighting).  
#It is convenient to express the transformation as a product of 
#two numbers - local and global weight functions, like so:
#a (i,j) = L(i,j)*G(i).
#The local weight function L(i,j) presents the weight of term i 
#in document j. The global weight function G(i) is used to express 
#the weight of the term iacross the entire document set. 

#We'll use the equivalent of tf-idf (local weight - tf, global -idf)
#check out other weighting schemes in the documentation (link above)
tdm.matrix.lsa <- lw_tf(tdm.matrix) * gw_idf(tdm.matrix)
dim(tdm.matrix.lsa)

#compute the Latent semantic space
lsaSpace <- lsa(tdm.matrix.lsa, dimcalc_share()) # create LSA space
#examine output
names(lsaSpace)

#Original Matrix is decomposed as: 
#tk(nterms,lsadim).Sk(lsadim).dk*(lsadim,ndocs)
#where 
#nterms=number of terms in TDM
#ndocs=number of docs in TDM
#lsadim=dimensionality of Latent Semantic Space (length of Sk)
LSAMat <- as.textmatrix(lsaSpace)

#1)
#Examine a term in LS space
LSAMat["social",1:10]
#compare to Term-frequency space
tdm.matrix.lsa["social",1:10]
#try other words
#What does this tell you about the term vectors in the two spaces?

#2)
#We've compared term vectors in LSA Space and TD space in (1), Now let's
#look at document vectors 
#Compare an original document vector and its counterpart in LSA Space.
#let's look at the first document in the corpus
write.csv(LSAMat[,colnames(LSAMat)[1]],paste(colnames(LSAMat)[1],".LSA.vec.csv"))
write.csv(tdm.matrix.lsa[,colnames(tdm.matrix.lsa)[1]],paste(colnames(tdm.matrix.lsa)[1],".TDM.vec.csv"))


#3) 
#Calculate similarity of documents in LSA space

cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

#Similarity matrix
cs.lsa <- as.matrix(cosineSim(t(LSAMat)))
write.csv(cs.lsa,"cs_lsa.csv")
cs.lsa[cs.lsa < 0.8] <- 0
cs.lsa <- round(cs.lsa,3)
write.csv(cs.lsa,"cs_lsa_trunc.csv")
#Have a look at the matrix. Comment.
#Optional exercises to try later:
#Build a network graph based on cosine similarity. Partition into communities 
#(clusters) using Louvain or Fast Greedy methods. Comment.
#Try clustering based on various distance measures. Comment. 
#

#4)
#nearest neighbours
library(LSAfun)
dim(lsaSpace$tk)
LSAtk <- t(lsaSpace$sk*t(lsaSpace$tk))
neighbors("slavery",n=6,tvectors=LSAtk)
neighbors("war",n=6,tvectors=LSAtk)
neighbors("god",n=6,tvectors=LSAtk)
neighbors("peace",n=6,tvectors=LSAtk)
neighbors("senate",n=6,tvectors=LSAtk)


