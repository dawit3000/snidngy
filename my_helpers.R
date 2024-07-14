## https://shiny.posit.co/r/getstarted/shiny-basics/lesson5/
## for 1gram inputs, nextword
# library(shiny)
## library(tm)
library(lattice)
library(markdown)
## library(dplyr)

twogramsplit <- readRDS("data/twogramsplit.rds")
outputBigram = function(x){
  subset(twogramsplit, prefix == x)
}


## for 2gram inputs, nextword
threegramsplit <- readRDS("data/threegramsplit.rds") 
outputTrigram = function(x){
  subset(threegramsplit, prefix == x)
}

## for 3gram inputs, nextword
fourgramsplit <- readRDS("data/fourgramsplit.rds") 
outputFourgram = function(x){
  subset(fourgramsplit, prefix == x)
}

## for 4gram inputs, nextword
fivegramsplit <- readRDS("data/fivegramsplit.rds") 
outputFivegram = function(x){
  subset(fivegramsplit, prefix == x)
}


## for 5gram inputs, nextword
sixgramsplit <- readRDS("data/sixgramsplit.rds") 
outputSixegram = function(x){
  subset(sixgramsplit, prefix == x)
}

## options(warn = -1)


most_frequent_words<- readRDS("data/most_frequent_words.rds")
most_frequent_words$frequency<-most_frequent_words$frequency/(sum(most_frequent_words$frequency))
most_frequent_words<-head(most_frequent_words,10)
