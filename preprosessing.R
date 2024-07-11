# Load necessary libraries
library(data.table)
library(dplyr)
library(tm)

# Load custom functions for tokenization
source("my_functions.R")

# Read in text files and create a combined corpus
blogs <- readLines("en_US.blogs.txt", skipNul = TRUE)
news <- readLines("en_US.news.txt", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", skipNul = TRUE)

# Sample a portion of each text source to reduce corpus size
blogs <- sample(blogs, 0.10 * length(blogs))
news <- sample(news, 0.10 * length(news))
twitter <- sample(twitter, 0.10 * length(twitter))
mycorpus <- c(blogs, news, twitter)

# Preprocess the corpus: convert to lowercase, remove stopwords, stem words,
# remove punctuation, strip whitespace, and remove numbers
mycorpus <- tolower(mycorpus)
mycorpus <- removeWords(mycorpus, stopwords("english"))
mycorpus <- stemDocument(mycorpus)
mycorpus <- removePunctuation(mycorpus)
mycorpus <- stripWhitespace(mycorpus)
mycorpus <- removeNumbers(mycorpus)

## Tokenizer function 
## RWeka package for tokenizing did not work for me and I had to search around 
## to find out between packages and open sources. Some great functions were openly 
## available of which the following function was great handling my needs for huge megabytes. 
## It is a simplify version and hereby acknowledge. 
## Source: archived at https://github.com/zero323
Tokenizer <-  function(x) {
    stopifnot(is.character(x)) # takes character vector x, and returns character vector
    
    # Split input text into word tokens
    tokens <- unlist(split_boundaries(x, opts_brkiter = options))
    len <- length(tokens)
    
    if (all(is.na(tokens)) || len < n) {
      # "||" performs element-wise comparisons on logical vectors and returns a single logical vector 
      # Return empty vector if no words found or fewer tokens than n
      character(0)
    } else {
      # Generate n-grams from tokens
      sapply(
        1:max(1, len - n + 1),
        function(i) join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
# Function to create ngrams and save them as data tables and split tokens
create_ngrams <- function(tokenizer_func, n, corpus) {
  # Tokenize the corpus into ngrams
  ngram <- tokenizer(n)(corpus)
  # Convert to data table
  ngram_dt <- as.data.table(ngram)
  # Calculate frequency of each ngram
  ngram_dt[, frequency := .N, by = ngram_dt]    
  # Remove duplicate ngrams and order by frequency
  ngram_sorted <- unique(ngram_dt[order(-frequency)])
  return(ngram_sorted)
}

# Loop through ngram sizes from 1 to 6
for (i in 1:6) {
  # Generate ngram name dynamically
  ngram_name <- paste0("mycorpus_", i, "g")
  # Choose appropriate tokenizer function based on n
  token_func <- switch(i, 
                       Tokenizer(1), 
                       Tokenizer(2), 
                       Tokenizer(3), 
                       Tokenizer(4), 
                       Tokenizer(5), 
                       Tokenizer(6))
  # Create ngrams using custom function
  assign(ngram_name, create_ngrams(token_func, i, mycorpus))
  # Save ngrams to RDS files
  saveRDS(get(ngram_name), file = paste0(i, "gram.rds"))
  # Generate split token name dynamically
  split_name <- paste0(i, "gramsplit")
  # Split ngrams into one_less and rest tokens using appropriate function
  assign(split_name, switch(i, 
                            splitTokenBigram(get(ngram_name)), 
                            splitTokenTrigram(get(ngram_name)), 
                            splitToken4gram(get(ngram_name)), 
                            splitToken5gram(get(ngram_name)), 
                            splitToken6gram(get(ngram_name))))
  # Save split tokens to RDS files
  saveRDS(get(split_name), file = paste0(i, "gramsplit.rds"))
}

# Example of usage
# Output bi-gram for the word "time"
outputBigram("time")
# Output top 10 results for bi-gram with "time"
outputBigram("time")[1:10, c(4, 2)]
# Output tri-gram for the phrase "time take"
outputTrigram("time take")[1:10, c(4, 2)] 

## divided tokens into one_less and nextword
## SplitTokenBigram
splitTokenBigram = function(x){
  
  one_less = character(nrow(x))
  nextword = character(nrow(x))
  w <- strsplit(x$gram, " ", fixed=TRUE)
  
  for(i in 1:nrow(x)){
    one_less[i] = w[[i]][1]
    nextword[i] = w[[i]][2]
  }
  x$one_less <- one_less
  x$nextword <- nextword
  return(x)
}

## SplitTokenTrigram
splitTokenTrigram = function(x){
  
  one_less = character(nrow(x))
  nextword = character(nrow(x))
  w <- strsplit(x$gram, " ", fixed=TRUE)
  
  for(i in 1:nrow(x)){
    one_less[i] = paste(w[[i]][1],w[[i]][2])
    nextword[i] = w[[i]][3]
  }
  x$one_less <- one_less
  x$nextword <- nextword
)
  return(x)
}


## splitToken4gram

splitToken4gram = function(x){
  
  one_less = character(nrow(x))
  nextword = character(nrow(x))
  w <- strsplit(x$gram, " ", fixed=TRUE)
  
  for(i in 1:nrow(x)){
    one_less[i] = paste(w[[i]][1],w[[i]][2], w[[i]][3])
    nextword[i] = w[[i]][4]
  }
  x$one_less <- one_less
  x$nextword <- nextword
  return(x)
}


## splitToken5gram

splitToken5gram = function(x){
  
  one_less = character(nrow(x))
  nextword = character(nrow(x))
  w <- strsplit(x$gram, " ", fixed=TRUE)
  
  for(i in 1:nrow(x)){
    one_less[i] = paste( w[[i]][1], w[[i]][2], w[[i]][3], w[[i]][4])
    nextword[i] = w[[i]][5]
  }
  x$one_less <- one_less
  x$nextword <- nextword
  return(x)
}

## splitToken6gram

splitToken6gram = function(x){
  
  one_less = character(nrow(x))
  nextword = character(nrow(x))
  w <- strsplit(x$gram, " ", fixed=TRUE)
  
  for(i in 1:nrow(x)){
    one_less[i] = paste(paste(w[[i]][1],w[[i]][2]), w[[i]][3], w[[i]][4], w[[i]][5])
    nextword[i] = w[[i]][6]
  }
  x$one_less <- one_less
  x$nextword <- nextword
  return(x)
}

## 1 words as input, options for 2rd word as output sorted descending by frequency
outputBigram = function(x){
  subset(bigramfunction, one_less == x)
}


## 2 words as input, options for 3rd word as output sorted descending by frequency
outputTrigram = function(x){
  subset(trigramfunction, one_less == x)
}

## 2 words as input, options for 3rd word as output sorted descending by frequency
outputFourgram = function(x){
  subset(fourgramfunction, one_less == x)
}


