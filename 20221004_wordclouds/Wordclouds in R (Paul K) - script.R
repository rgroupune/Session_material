#.  source: https://www.r-bloggers.com/2017/10/qualitative-research-in-r/

#. set up necessary packages
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tm)
library(wordcloud)
library(RColorBrewer)

#. import data
#.. extract from survey of youth career aspirations in Mekong Delta, VN
#.. anonymised and scrambled
#.. focal question here: What are your career goals?
ys <- read.table("clipboard", header=TRUE, sep="\t", as.is=F)
names(ys) ; str(ys)

#.. check levels of factors
levels(ys$Gender)
levels(ys$Location)
levels(ys$Major)

#.. make some subsets
ys_Agr <- subset(ys, Major == "A")
ys_Eco <- subset(ys, Major == "E")
ys_Soc <- subset(ys, Major == "S")

ys_F <- subset(ys, Gender == "Female")
ys_M <- subset(ys, Gender == "Male")
ys_Oth <- subset(ys, Gender == "Other, not stated")

#.. pick which dataset to use in subsequent code
# start with all 
DF <- ys
# or use these subsets
DF <- ys_Agr
DF <- ys_Eco
DF <- ys_Soc

DF <- ys_F
DF <- ys_M
DF <- ys_Oth


#. career goals
#.. make 'corpus' and clean unwanted characters
Goals <- as.character(DF$Goals)            # Convert to 'character' format
docs <- Corpus(VectorSource(Goals))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, content_transformer(tolower)) # Convert to lower case
docs <- tm_map(docs, removeNumbers)                # Remove numbers
docs <- tm_map(docs, removePunctuation)            # Remove punctuation
docs <- tm_map(docs, stripWhitespace)              # Remove extra white spaces
docs <- tm_map(docs, removeWords, stopwords('english'))  # Remove stopwords
#docs <- tm_map(docs, stemDocument)                 # Reduce to stem words only
inspect(docs)

#.. do some processing
docs_matrix <- TermDocumentMatrix(docs)            #make word count matrix
word_matrix <- as.matrix(docs_matrix)
word_counts <- sort(rowSums(word_matrix), decreasing = TRUE)
word_df <- data.frame(word = names(word_counts), freq = word_counts)
dimnames(word_df)[[1]] <- 1:nrow(word_df)

#.. check how many words are included, depending on frequency
nrow(word_df) 
nrow(subset(word_df, freq > 1))
nrow(subset(word_df, freq > 2))
nrow(subset(word_df, freq > 5))

#.. check how many words at each frequency level
word_freq <- aggregate(word ~ freq, data = word_df, FUN = length)

#. make wordcloud
wordcloud(word_df$word, word_df$freq, min.freq = 1, #all words
  max.words = nrow(word_df), random.order = F, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"))

wordcloud(word_df$word, word_df$freq, min.freq = 6, # freq = 6+
  max.words = 200, random.order = F, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2"))

#. make barchart
word_df2 <- subset(word_df, freq > 2)     # remove infrequent words
windows(7,9, pointsize = 12, record = T)	# open window of set size
plot(1,1)                                 # make a base plot (don't know why?)
barchart(reorder(word, freq) ~ freq, data = word_df2,
  scales=list(alternating=F, cex=0.8, tck=c(1,0)),
  xlab = "Frequency", ylab = "Words",
	key = list(corner=c(0.22, 0.01), columns = 1, between = 1.5,
		text = list(c("Frequency",5:1), adj=1, cex=0.8), background="white",
		text = list(c("Words",word_freq[c(5:1),2]), cex=0.8)),
  panel = function(x, y) {
  panel.grid(v=-1, h=0)
  panel.barchart(x,y, col="cornsilk", border = "tan")
})

#. make a table
write.csv(word_df, "tab.csv", row.names = T)
