#this exercise uses the text8 file in the DAM_BS4 folder
install.packages("text2vec")
#Check out this vignette:
#https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
#and the documentation
#https://cran.r-project.org/web/packages/text2vec/text2vec.pdf

rm(list=ls())
library(text2vec)

# Load or download the wikipedia text8 dataset (first 10^9 bytes of the 
#English Wikipedia dump on Mar. 3, 2006. )
text8_file = "./text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "./text8.zip")
  unzip ("./text8.zip", files = "text8")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens <- space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)#, ngram=c(1,2))

# Take words which appear 5 or more times
vocab <- prune_vocabulary(vocab, term_count_min = 5)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)# Create Term Co-occurence matrix
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# Can modify the number of threads used in parallel - default is all available
# RcppParallel::setThreadOptions(numThreads = 4).

# Train the glove model
glove = GlobalVectors$new(word_vectors_size = 50, 
                          vocabulary = vocab, x_max = 5)
word_vectors_main = glove$fit_transform(tcm, n_iter = 5)

# Extract the word context vectors (should add these in to improve quality of embedding)
#word_vectors_context = glove$components
#word_vectors = word_vectors_main + t(word_vectors_context)
#str(word_vectors)

# Get vector for Paris - France + Germany
word <- word_vectors_main["paris", , drop = FALSE] - 
  word_vectors_main["france", , drop = FALSE] + 
  word_vectors_main["germany", , drop = FALSE]

# Find the closest word_vector to our berlin vector
cos_sim = sim2(x = word_vectors_main, y = word, method = "cosine", norm = "l2")
# Is the closest Berlin?
head(sort(cos_sim[,1], decreasing = TRUE), 5)

#another example
word1 <- word_vectors_main["london", , drop = FALSE] - 
  word_vectors_main["england", , drop = FALSE] + 
  word_vectors_main["france", , drop = FALSE]

# Find the closest word_vector to this vector
cos_sim = sim2(x = word_vectors_main, y = word1, method = "cosine", norm = "l2")
# What are the closest words?
head(sort(cos_sim[,1], decreasing = TRUE), 5)

#another example
word2 <- word_vectors_main["book", , drop = FALSE] - 
  word_vectors_main["author", , drop = FALSE] + 
  word_vectors_main["artist", , drop = FALSE]

# Find the closest word_vector to this vector
cos_sim = sim2(x = word_vectors_main, y = word2, method = "cosine", norm = "l2")
# What are the closest words?
head(sort(cos_sim[,1], decreasing = TRUE), 5)



