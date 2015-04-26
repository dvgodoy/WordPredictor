library(stylo)
library(stringi)
library(sqldf)
library(reshape)
library(fastmatch)

## Function to load N-grams from a specific source that were saved with SaveSource
LoadSource <- function(directory,sourcename,MinCount=2) {
  setwd(directory)
  freq = list()  
  for (i in 1:4) {
    handle = file(paste0(sourcename,i,'gram'),"rb")
    load(handle)
    freq[[i]] = unserialize(get(paste0(sourcename,i,'gram')))
  }
  rm(handle)
  if (MinCount > 1) {
    for (i in 1:4) {
      freq[[i]] = TopNGramFreq(freq[[i]],MinCount)
    }
  }
  freq
}

## Function to save N-grams of a specific source and passed as argument (Freqs)
SaveSource <- function(directory,sourcename,Freqs) {
  setwd(directory)
  for (i in 1:4) {
    handle = file(paste0(sourcename,i,'gram'),"wb")
    assign(paste0(sourcename,i,'gram'),serialize(Freqs[[i]], NULL))
    save(list=paste0(sourcename,i,'gram'),file=handle)    
  }
  close(handle)
  rm(handle)  
}

## Function to calculate and save N-Gram of a specific size
## averaged over the three sources
SaveMeanNGram <- function(directory,NGramSize,MinCount) {
  freq = LoadNGrams(directory,NGramSize,MinCount)
  blogf = data.frame(ngram=names(freq[[1]]),freq1=freq[[1]])
  newsf = data.frame(ngram=names(freq[[2]]),freq2=freq[[2]])
  twitf = data.frame(ngram=names(freq[[3]]),freq3=freq[[3]])
  meanf = merge(merge(blogf,newsf,by='ngram',all=TRUE),twitf,by='ngram',all=TRUE)
  meanf$freq1[which(is.na(meanf$freq1))]=0
  meanf$freq2[which(is.na(meanf$freq2))]=0
  meanf$freq3[which(is.na(meanf$freq3))]=0
  meanf = transform(meanf, freq=(freq1+freq2+freq3)/3)
  ngrams = meanf[,'ngram']
  meanf = meanf[,'freq']
  names(meanf) = ngrams
  
  setwd(directory)
  handle = file(paste0('mean',NGramSize,'gram'),"wb")
  assign(paste0('mean',NGramSize,'gram'),serialize(meanf, NULL))
  save(list=paste0('mean',NGramSize,'gram'),file=handle)    
  close(handle)
  rm(handle)  
}

## Function to load N-grams of a specific size from all sources
LoadNGrams <- function(directory,NGramSize,MinCount=2) {
  setwd(directory)
  freq = list()
  sources = c('blog','news','twit')
  for (i in 1:3) {
    handle = file(paste0(sources[i],NGramSize,'gram'),"rb")
    load(handle)
    freq[[i]] = unserialize(get(paste0(sources[i],NGramSize,'gram')))
  }
  rm(handle)
  if (MinCount > 1) {
    for (i in 1:3) {
      freq[[i]] = TopNGramFreq(freq[[i]],MinCount)
    }
  }
  freq
}

## Function to load a text file to be tokenized
LoadTextFile <- function(directory, filename) {
  myfile = file(paste0(directory,'/',filename), open="rb")
  textfile = readLines(myfile, encoding="UTF-8",skipNul=TRUE)
  textfile = iconv(textfile, from="UTF-8", to="latin1", sub=" ")
  close(myfile)
  rm(myfile)
  textfile
}

## Function to tokenize a text file
Tokenizer <- function(textfile) {
  my.text = textfile
  # To lower case
  my.text = tolower(my.text)
  # Removes numbers and special characters (except for the ' as we intend to keep contractions)
  my.text.eos = stri_replace_all(my.text, '', regex='[0-9]+|[+"()@#$%^&*_=|/<>]+|-')
  # Substitutes punctuation (.!?) for end of sentence (</s>) followed by begin of sentence <s>
  # and pastes a begin of sentence at the very beginning of the text.
  # This way we can keep avoid cross-sentence Ngrams.
  # Ngrams containing any of (</s>, <s>) will be removed later.
  my.text.eos = paste('<s>', stri_replace_all(my.text.eos, ' </s> <s>', regex='[.?!]'))
  # Tokenizes the text splitting by spaces, commas, colons, semicolons, tabs and newlines.
  my.text.tokenized = txt.to.words(my.text.eos, splitting.rule = "([a-z]+_)|[ ,;:\n\t]")
  # Removes temporary objects from memory
  rm(my.text)
  rm(my.text.eos)
  # Returns tokenized text
  my.text.tokenized
}

## Function to generate a table of frequencies given a tokenized file and a N-gram size
NGramFreq <- function(tokenized,NGramSize=1,MinCount=2) {
  # Generates Ngrams from the tokenized text
  if (NGramSize > 1) {
    my.NGrams = txt.to.features(tokenized, ngram.size=NGramSize)
  } else {
    my.NGrams = tokenized
  }
  
  # According to the size, make sure the sentences do not begin nor end with '.
  expr = paste0("^([a-z]+([a-z]+|('+[a-z]+)){0,1}[ ]{0,1}){1,",NGramSize,"}$")
  my.NGrams = my.NGrams[grep(expr,my.NGrams)]
  
  # Generates the complete list of unique Ngrams
  complete.Ngrams.list = names(sort(table(unlist(my.NGrams)), 
                                    decreasing = TRUE))
  # Calculates Ngrams frequencies
  Ngram.freq = make.table.of.frequencies(my.NGrams, complete.Ngrams.list)
  # Assumes minimum frequency = 1 occurrence
  # Keeps only frequency corresponding to MinCount
  top.Ngram.freq = Ngram.freq
  if (MinCount > 1) {
    top.Ngram.freq = TopNGramFreq(Ngram.freq,MinCount)
  }
  # Removes temporary objects from memory
  rm(my.NGrams)
  rm(complete.Ngrams.list)
  # Returns frequencies
  top.Ngram.freq
}

## Function to filter out rare frequencies, assuming the mininum frequency equals a count of one
TopNGramFreq <- function(NGramFreq,MinCount=2) {
  top.Ngram.freq = head(sort(NGramFreq,decreasing=TRUE),sum(NGramFreq>(MinCount-1)*min(NGramFreq))) 
  top.Ngram.freq
}

## Function to generate a prediction table from a table of frequencies and a given number of options to keep
PredictionTable <- function(NGramFreq,QtOptions=5,HashTable=FALSE) {
  # Calculates Ngram size
  NGramSize = length(unlist((stri_split_fixed(names(NGramFreq)[1]," "))))
  
  # Splits NGrams into a continuous list
  pairs = unlist(stri_split_fixed(names(NGramFreq)," "))
  
  # According to size N, generates a DF with N columns and order by descendent frequency
  # For N > 2, concatenates (N-1) first words into "first" column to be used as predictor
  if (NGramSize == 2) {
    pfreq = data.frame(first=pairs[seq(1,length(pairs)-1,2)],second=pairs[seq(2,length(pairs),2)],freq=as.vector(NGramFreq))
    pairs.ord = sqldf("select * from pfreq order by first asc, freq desc")
    tot.pairs = pairs.ord
  } else if (NGramSize == 3) {
    pfreq = data.frame(first=pairs[seq(1,length(pairs)-2,3)],second=pairs[seq(2,length(pairs)-1,3)],third=pairs[seq(3,length(pairs),3)],freq=as.vector(NGramFreq))
    pairs.ord = sqldf("select * from pfreq order by first asc, second asc, freq desc")    
    tot.pairs = data.frame(first=paste(pairs.ord$first,pairs.ord$second),second=pairs.ord$third,freq=pairs.ord$freq)  
  } else if (NGramSize == 4) {
    pfreq = data.frame(first=pairs[seq(1,length(pairs)-3,4)],second=pairs[seq(2,length(pairs)-2,4)],third=pairs[seq(3,length(pairs)-1,4)],fourth=pairs[seq(4,length(pairs),4)],freq=as.vector(NGramFreq))
    pairs.ord = sqldf("select * from pfreq order by first asc, second asc, third asc, freq desc")    
    tot.pairs = data.frame(first=paste(pairs.ord$first,pairs.ord$second,pairs.ord$third),second=pairs.ord$fourth,freq=pairs.ord$freq)  
  } else {
    return(NULL)
  }
  
  # Calculates total frequencies for each unique occurrences of (N-1) words (column "first")
  tot.pairs = transform(tot.pairs, tot.freq = ave(freq, first, FUN=sum))
  # Calculates cumulative frequency of the unique predictors and sort in descending order
  tot.pairs2 = transform(tot.pairs, first = as.character(first))
  tot.pairs2 = tot.pairs2[,c('first','tot.freq')]
  tot.pairs2 = unique(tot.pairs2)
  sum.freq = sum(tot.pairs2$tot.freq)
  tot.pairs2 = transform(tot.pairs2, tot.freq = tot.freq/sum.freq)
  tot.pairs2 = tot.pairs2[order(-tot.pairs2$tot.freq),]
  # Obtains predictor's probability at the level of 99.5% probability mass
  min.prob = tot.pairs2$tot.freq[min(which(cumsum(tot.pairs2$tot.freq)>.995))]
  # Considers only predictors with probabilities above this threshold
  tot.pairs2 = tot.pairs2[tot.pairs2$tot.freq>=min.prob,]
  # Subsets the predictors
  tot.pairs = tot.pairs[tot.pairs$first%in%tot.pairs2$first,]
  # Recalculates cumulative frequency of predicted words within a same predictor
  tot.pairs = transform(tot.pairs, cumsum = ave(freq/tot.freq, first, FUN = cumsum))
  # For predictors with a large number of predicted words,
  # keeps only those within the 95% probability mass OR
  # those with probability greater than 5%.
  tot.pairs = tot.pairs[(tot.pairs$cumsum<.95)|(tot.pairs$cumsum==1.0&(tot.pairs$freq/tot.pairs$tot.freq)>.05),]
  
  # Creates a DF with predictors and predicted words
  Ngram.pred = data.frame(first=tot.pairs$first,pred=tot.pairs$second,freq=tot.pairs$freq)
  # Adds a column full of 1's in order to compute a ranking of the predicted words
  # within a given predictor
  Ngram.pred = transform(cbind(Ngram.pred,ones=1), count = ave(ones, first, FUN=cumsum))
  # Filters out options above the specified threshold
  Ngram.pred = Ngram.pred[Ngram.pred$count<=QtOptions,]
  # Concatenates the ranking after the predictor in order to disambiguate
  Ngram.pred = data.frame(first=paste0(as.character(Ngram.pred$first),'.',Ngram.pred$count),pred=as.character(Ngram.pred$pred),freq=Ngram.pred$freq)
  
  # Removes temporary objects from memory
  rm(pairs)
  rm(pairs.ord)
  rm(pfreq)
  rm(tot.pairs)
  
  # Generates a hash table of the pairs (predictors, predicted)
  if (HashTable) {
    Ngram.hash = hash(Ngram.pred[,1],Ngram.pred[,2])
    Ngram.hash
  } else {
    Ngram.pred
  }
}

## Function to actually make a prediction
Prediction <- function(predictor) {
  require(stringi)
  require(fastmatch)
  
  # defines a subfunction to trim leading and trailing spaces
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }
  # convert predictor to lower case
  predictor = tolower(predictor)
  # if the user types punctuation, separate sentences in order to avoid cross-sentence N-grams
  last.end = stri_locate_last(predictor,regex='[.?!]')[1]
  if (is.na(last.end)) last.end = 0
  if (last.end > 0) predictor = trim(substr(predictor,start=last.end+1,stop=nchar(predictor)))
  # removes special characters and numbers
  predictor = stri_replace_all(predictor, '', regex='[0-9]+|[+"()@#$%^&*_=|/<>]+|-')
  # removes punctuation
  predictor = stri_replace_all(predictor, ' ', regex='[,;:\n\t]')
  # check if the user has typed a space...
  if (stri_sub(predictor,nchar(predictor)) == ' ' | predictor == '') {
    predictor = trim(predictor)
  } else {
    # in case it is still typing a word, checks for the most likely word being typed
    wordpart = unlist(stri_split_fixed(predictor," "))
    wordpart = wordpart[length(wordpart)]
    wordset = unigram[grep(paste0("^",wordpart,".*"),unigram$word),]
    word = as.character(wordset[which.max(wordset$freq),'word'])
    if (!length(word)) word = ""
    return(stri_sub(word,nchar(wordpart)+1))
  }
  
  # split the input into words
  words = unlist(stri_split_fixed(predictor," "))
  NGram = length(words)
  # if there are more than three words...
  if (NGram > 3) {
    # keeps only the last typed 3-gram
    predictor = paste(words[(NGram-2):NGram],collapse=" ")
    words = words[(NGram-2):NGram]
    NGram = 3
  }
  # since we are interested in the most likely word to follow, we have to check for a N-gram one size bigger
  NGram = NGram + 1
  next.word = NULL
  # simple back-off strategy: if there is no N-gram of size 4, steps down one size at a time
  # it steps down also if there are not enough suggestions
  for (i in seq(NGram,1,by=-1)) {
    if (i > 1) {
      next.word = unique(c(next.word,as.character(pred[[i]][fmatch(paste0(predictor,".",seq(1:5)),pred[[i]]$first),'pred'])))
      next.word = next.word[!is.na(next.word)]      
    } else {
      # if gets to unigrams, just add the top 5 words
      next.word = unique(c(next.word, pred[[1]][1:5]))
    }
    # filters out profanity as suggestion
    next.word = setdiff(next.word,profanityWords)
    # if there are more than 5 words suggested, keeps only the top 5
    if (length(next.word) >= 5) {
      return(next.word[1:5])
    }
    if (i > 2) {
      predictor = paste(words[(NGram-i+2):(NGram-1)],collapse=" ")
    }
  }
}

###########################################################################################
## Function calls in order to build the unigram and pred[[]] variables which are used by ##
## the Prediction function in the .RData stored in ShinyApps                             ##
###########################################################################################
source.dir = "./Data/en_US"
savedir = "./"

blog = LoadTextFile(source.dir,'en_US.blogs.txt')
tokenized = Tokenizer(blog)
freq = list()
for (i in 1:4) {
  freq[[i]] = NGramFreq(tokenized,i,1)  
}
SaveSource(savedir,'blog',freq)

news = LoadTextFile(source.dir,'en_US.news.txt')
tokenized = Tokenizer(news)
freq = list()
for (i in 1:4) {
  freq[[i]] = NGramFreq(tokenized,i,1)  
}
SaveSource(savedir,'news',freq)

twit = LoadTextFile(source.dir,'en_US.twitter.txt')
tokenized = Tokenizer(twit)
freq = list()
for (i in 1:4) {
  freq[[i]] = NGramFreq(tokenized,i,1)  
}
SaveSource(savedir,'twit',freq)

for (i in 1:4) {
  SaveMeanNGram(savedir,i,2)  
}

profanityWords = LoadTextFile(savedir,"ProfanityWords.txt")

meanGrams = LoadSource(savedir,'mean',3)
unigram = data.frame(word=names(meanGrams[[1]]),freq=meanGrams[[1]])
pred = list()
pred[[1]] = as.character(unigram$word[order(-unigram$freq)])[1:5]
for (i in 2:4) {
  pred[[i]] = PredictionTable(meanGrams[[i]],5,FALSE)
  pred[[i]]=data.frame(first=as.character(pred[[i]]$first),pred=as.character(pred[[i]]$pred))
}