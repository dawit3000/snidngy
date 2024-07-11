## for 1gram inputs, nextword

twogramsplit <- readRDS("twogramsplit.rds")
outputBigram = function(x){
  subset(twogramsplit, prefix == x)
}


## for 2gram inputs, nextword
threegramsplit <- readRDS("threegramsplit.rds") ##2 rest
outputTrigram = function(x){
  subset(threegramsplit, prefix == x)
}

## for 3gram inputs, nextword
fourgramsplit <- readRDS("fourgramsplit.rds") ##2 rest
outputFourgram = function(x){
  subset(fourgramsplit, prefix == x)
}

## for 4gram inputs, nextword
fivegramsplit <- readRDS("fivegramsplit.rds") ##2 rest
outputFivegram = function(x){
  subset(fivegramsplit, prefix == x)
}


## for 5gram inputs, nextword
sixgramsplit <- readRDS("sixgramsplit.rds") ##2 rest
outputSixegram = function(x){
  subset(sixgramsplit, prefix == x)
}

## options(warn = -1)


most_frequent_words<- readRDS("most_frequent_words.rds")
most_frequent_words$frequency<-most_frequent_words$frequency/(sum(most_frequent_words$frequency))
most_frequent_words<-head(most_frequent_words,10)
