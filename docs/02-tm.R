##Part 2
#Uses the same corpus as in the previous exercise
#assumes tm, ggplot and wordcloud libraries are loaded

#remove custom stopwords
#NOTE: change stopwords appopriately if you do not stem
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot","man","say","well")
docs <- tm_map(docs, removeWords, myStopwords)
#wordlengths: remove very frequent and very rare words
#bounds: include only words that occur in at least / at most n_lower / n_upper docs
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
                                             bounds = list(global = c(3,45))))
freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)

#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#write to disk and inspect file
write.csv(file="freqr.csv",freqr[ordr])
#inspect least frequently occurring terms
freqr[tail(ordr)]
#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtmr,lowfreq=60)
#correlations
findAssocs(dtmr,"state",0.9)
findAssocs(dtmr,"govern",0.7)
findAssocs(dtmr,"system",0.8)
#histogram
wf=data.frame(term=names(freqr),occurrences=freqr)
#library(ggplot2)
p <- ggplot(subset(wf, freqr>200), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#wordcloud
#library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)
#...add color
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))
