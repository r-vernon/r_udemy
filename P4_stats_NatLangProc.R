
# NOTES FOR NATURAL LANGUAGE PROCESSING
# -------------------------------------
#
# One approach: can represent a document as a 'bag of words'
# - Say word-list was (red,blue,house)
#  - 'Blue House' would map to (0,1,1), 'Red House' (1,0,1)
#  - Can then use similarity measures to compare two documents on freq. counts
#  - e.g. cosine similarity, sim(A,B) = cos(th) = (A.B) / (|A||B|)
# - Can improve by adjusting word counts based on overall frequency
#
# Key definitions:
# - Term Frequency TF(t,d): num. occurrences of term t in doc d
# - Inverse Document Frequency (IDF) - importance of term in corpus
#  - IDF(t) = log(N/df(t)), N = num docs, df(t) = doc freq., num docs with term
# Combined: TF-IDF = TF(t,d) x log(N/df(t))
#
# ==============================================================================
# work through example
# (note, using whatsapp data rather than twitter data)
#
# yeah... this isn't a great example, but the principles kind of apply

# load necessary modules
library(tidyverse)
library(tm) # text processing
library(SnowballC) # stem processing
library(wordcloud) # word clouds
library(RColorBrewer) # colouring wordcloud

# load in the data 
fPath <- normalizePath('./dataFiles/')
txt <- VCorpus(DirSource(fPath, encoding='UTF-8', pattern='whatsapp'))
origTxt <- txt

# ------------------------------------------------------------------------------
# clean the data

# convert to ASCII
txt <- tm_map(txt, content_transformer(function (x) 
    iconv(x,'UTF-8','ASCII',sub=' ')))

# remove timestamps
txt <- tm_map(txt, content_transformer(function(x) gsub('\\[.+?\\]','',x)))

# convert to lower case, remove punctuation, numbers 
txt <- tm_map(txt, content_transformer(tolower))
txt <- tm_map(txt, content_transformer(removePunctuation))
txt <- tm_map(txt, content_transformer(removeNumbers))

# remove common stopwords (unnecessary filler) (applying same xforms as above)
# n1 <- r~ # first name
# n2 <- d~ # second name
toRem <- tolower(removePunctuation(c(n1,n2,stopwords('english'))))
txt <- tm_map(txt, removeWords, toRem)

# stem the document (computer, computers = computer)
txt <- tm_map(txt, stemDocument)

# remove 'image omitted' lines
txt <- tm_map(txt, content_transformer(function(x) gsub('^.*image omitted.*$','',x)))

# strip whitespace
txt <- tm_map(txt, content_transformer(stripWhitespace))

# remove single characters, or whitespace at start/end of lines
txt <- tm_map(txt, content_transformer(function(x) gsub('(^| ).( |$)',' ',x)))
txt <- tm_map(txt, content_transformer(function(x) gsub('^ | $','',x)))

# remove first line
txt[[1]][["content"]] <- txt[[1]][["content"]][-1]

# remove empty lines (basically x <- x[x != ''])
txt[[1]][["content"]] <- txt[[1]][["content"]][txt[[1]][["content"]] != '']

# print first 10 lines
txt[[1]][["content"]][1:10]

# ------------------------------------------------------------------------------
# create document term matrix

docMat <- TermDocumentMatrix(txt)

# remove sparse terms
docMat <- removeSparseTerms(docMat, 0.98)

# convert to matrix
docMat <- as.matrix(docMat)

# get word counts
wordFreq <- sort(rowSums(docMat), decreasing=T)
df <- data.frame(word=names(wordFreq), freq=wordFreq)

# remove words that are either infrequent or too frequent
print(quantile(df$freq,c(0.75,0.99))) # 6, 229
df <- filter(df,freq > 6 & freq < 229)

# create a quick histogram
pl <- ggplot(df, aes(freq)) + geom_histogram(bins=50)
print(pl)

# create a wordcloud
wordcloud(df$word, df$freq, random.order=F, colors=brewer.pal(8, "Dark2")) 
