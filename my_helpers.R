## 1 words as input, options for 2rd word as output sorted descending by frequency

twogramsplit <- readRDS("twogramsplit.rds")
outputBigram = function(x){
  subset(twogramsplit, prefix == x)
}


## 2 words as input, options for 3rd word as output sorted descending by frequency
threegramsplit <- readRDS("threegramsplit.rds") ##2 rest
outputTrigram = function(x){
  subset(threegramsplit, prefix == x)
}

## 3 words as input, options for 4rt word as output sorted descending by frequency
fourgramsplit <- readRDS("fourgramsplit.rds") ##2 rest
outputFourgram = function(x){
  subset(fourgramsplit, prefix == x)
}

## 4 words as input, options for 5th word as output/nextword sorted descending by frequency
fivegramsplit <- readRDS("fivegramsplit.rds") ##2 rest
outputFivegram = function(x){
  subset(fivegramsplit, prefix == x)
}


## 5 words as input, options for 4rt word as output sorted descending by frequency
sixgramsplit <- readRDS("sixgramsplit.rds") ##2 rest
outputSixegram = function(x){
  subset(sixgramsplit, prefix == x)
}


## options(warn = -1)


most_frequent_words<- readRDS("most_frequent_words.rds")
most_frequent_words$frequency<-most_frequent_words$frequency/(sum(most_frequent_words$frequency))
most_frequent_words<-head(most_frequent_words,10)
