#Some general tips before you proceed:
# 1. Install the following packages before you proceed: tm, SnowballC, ggplot2, 
# wordcloud
# 2. Download documentation for tm from CRAN: 
# https://cran.r-project.org/web/packages/tm/tm.pdf
# 3. Run the code for  line by line and examine the results.
# 4. Understand what each step does. Check out the environment panel (on the right
#    in RStudio) to see more about the variables created in each step.
# 5. Check the tm documentation for tm specific functions: tm_map, DocumentTermMatrix, 
#    findFreqTerms and FindAssocs
# 6. Check out the following vignette on CRAN for more on tm: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# 7. ...and finally, don't forget to modify the directory path in the setwd() 
#    command before starting!

rm(list=ls())
dev.off()
#set working directory
getwd() 
setwd("C:/Users/<Your Username>/Documents/textmining") #change path as needed
getwd() #check that you're where you should be
library(tm) 
library(SnowballC)
#Let's go. Load corpus... 
docs <- VCorpus(DirSource("./docs-1"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#Check details
#of docs
print(docs)
class(docs)
#Volatile corpus consisting of 30 documents
#examine contents
docs[1]
class(docs[1])
#Volatile corpus consisting of 1 documents
#To access the actual contents we need to use the [[]] notation
class(docs[[1]])
docs[[1]]$meta
docs[[1]]$content

#Preprocessing

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
##########################
##WARNING!!! - Mac users may want to avoid running stemDocument as it can give an
##error and corrupt the corpus.
docs <- tm_map(docs,stemDocument)
#inspect
writeLines(as.character(docs[[30]]))
#end of preprocessing

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#summary
dtm
#inspect segment of document term matrix
inspect(dtm[1:10,1000:1006])
#collapse matrix by summing over columns - this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (asc)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#write to disk and inspect file
write.csv(file="freq.csv",freq[ord])
#inspect least frequently occurring terms
freq[tail(ord)]
#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtm,lowfreq=80)
#correlations
findAssocs(dtm,"state",0.95)
findAssocs(dtm,"govern",0.75)
findAssocs(dtm,"system",0.9)
#histogram
library(ggplot2)
wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, occurrences>200), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>200), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, max.words=40)
#...add color
wordcloud(names(freq),freq,max.words=40,colors=brewer.pal(6,"Dark2"))
#play with different values of max.words
#try specifying min.freq instead of max.words

##n-grams
#######################

##create bigram tokenizer
#uses ngrams function from NLP package

##ngrams
#to see what ngrams does, try running ngrams(words(docs[[1]]$content),2), which
#returns bigrams for the first document in the corpus

##unlist creates a vector from the atomic components of a list

BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#create DTM
dtmbi <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
freqbi <- colSums(as.matrix(dtmbi))
#length should be total number of terms
length(freqbi)
#create sort order (asc)
ordbi <- order(freqbi,decreasing=TRUE)
#inspect most frequently occurring terms
freqbi[head(ordbi)]
#and so on...

