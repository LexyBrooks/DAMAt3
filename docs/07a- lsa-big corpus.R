#Not an in-class exercise
#uses docs-3 corpus
#https://cran.r-project.org/web/packages/lsa/lsa.pdf 

rm(list=ls())
#set working directory
getwd() 
setwd("C:/Users/<Your Username>/Documents/textmining") #change path as needed
getwd() #check that you're where you should be
library(tm) 
library(lsa)
#Let's go. Load corpus... 
docs <- VCorpus(DirSource("./docs-3"))
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
lsaSpace <- lsa(tdm.matrix.lsa) # create LSA space



library(LSAfun)
dim(lsaSpace$tk)
LSAtk <- t(lsaSpace$sk*t(lsaSpace$tk))
neighbors("slavery",n=6,tvectors=LSAtk)
neighbors("war",n=6,tvectors=LSAtk)
neighbors("god",n=6,tvectors=LSAtk)
neighbors("peace",n=6,tvectors=LSAtk)
neighbors("senate",n=6,tvectors=LSAtk)

