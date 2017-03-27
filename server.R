library(dplyr)
library(utils)
library(data.table)
library(devtools)
library(Rcpp)
library(cmscu, lib.loc = "C:/Users/David/Documents/R/win-library/3.3")

setwd("~/JHU_DataScience/CapstoneProject/Coursera-SwiftKey/final/en_US")
DictGrams1 <- new(FrequencyDictionary, 4, 2^22)
DictGrams2 <- new(FrequencyDictionary, 4, 2^22)
DictGrams3 <- new(FrequencyDictionary, 4, 2^22)
tripref <- new(FrequencyDictionary, 4, 2^22)
tricomp <- new(FrequencyDictionary, 4, 2^22)
trimidd <- new(FrequencyDictionary, 4, 2^21)
bipref <- new(FrequencyDictionary, 4, 2^20)
DictN21 <- new(FrequencyDictionary, 4, 2^20)
DictN22 <- new(FrequencyDictionary, 4, 2^18)
DictN23 <- new(FrequencyDictionary, 4, 2^18)
DictN31 <- new(FrequencyDictionary, 4, 2^21)
DictN32 <- new(FrequencyDictionary, 4, 2^18)
DictN33 <- new(FrequencyDictionary, 4, 2^18)

DictGrams1$read("grams1.bin")
DictGrams2$read("grams2.bin")
DictGrams3$read("grams3.bin")
tripref$read("tripref.bin")
tricomp$read("tricomp.bin")
trimidd$read("trimidd.bin")
bipref$read("bipref.bin")
DictN21$read("DictN21.bin")
DictN22$read("DictN22.bin")
DictN23$read("DictN23.bin")
DictN31$read("DictN31.bin")
DictN32$read("DictN32.bin")
DictN33$read("DictN33.bin")

#count vocabulary
V1 <- DictGrams1$unique_entries #number of unique unigrams
V2 <- DictGrams2$unique_entries #number of unique bigrams
distribution <- matrix(unlist(DictGrams2$histogram(4), use.names=FALSE),
                       ncol= 5, byrow = TRUE)
n2 <- apply(distribution, 2, median)
Y <- n2[1]/(n2[1] + 2*n2[2])
D21  <- 1 - (2*Y*n2[2]/n2[1])
D22  <- 2 - (3*Y*n2[3]/n2[2])
D23  <- 3 - (4*Y*n2[4]/n2[3])

distribution <- matrix(unlist(DictGrams3$histogram(4), use.names=FALSE),
                       ncol= 5, byrow = TRUE)
n3 <- apply(distribution, 2, median)
Y <- n3[1]/(n3[1] + 2*n3[2])
D31  <- 1 - (2*Y*n3[2]/n3[1])
D32  <- 2 - (3*Y*n3[3]/n3[2])
D33  <- 3 - (4*Y*n3[4]/n3[3])

KN_tri_all <- function(phrase){
   #check to see if the singleton words are in the training set or not
   #and if not convert them to UNK
   wv <- unlist(strsplit(phrase, " "))
   #wv <- c("to", "the", "BOS")
   for(i in seq_along(wv)){
      if (DictGrams1$query(wv[i]) == 0) wv[i] <- "UNK"
   }
   #wv
   #compute first back-off model (PKN1) (see equation 23 in Chen and Goodman 1998)
   if (wv[3] == "UNK") {
      PKN1 <- 1/(V1 - 1) #drop EOS from list of unique unigrams
   } else {
      PKN1 <- bipref$query(wv[3])/V2
   }
   
   #compute PKN2
   #compute gamma1
   #first compute N's
   N21  <- DictN21$query(wv[2])
   N22  <- DictN22$query(wv[2])
   N23  <- DictN23$query(wv[2])
   
   #compute number of trigrams that end with wv[2] and wv[3]
   endgram2 <- paste(wv[2], wv[3], sep = " ")
   numwv_23 <- tripref$query(endgram2)
   if (length(numwv_23 == 0) | numwv_23 == 0) {
      numwv_23 <- 0
      #PKN2 <- PKN1
   } 
   
   #then pick which D is used for the discount
   if (numwv_23 == 0) {
      D <- 0 
   }  else if (numwv_23 == 1) {
      D <- D21
   } else if (numwv_23 == 2) {
      D <- D22
   } else if (numwv_23 >= 3) {
      D <- D23
   } 
   
   #next compute number of trigrams with wv[2] in the middle
   numwv_2_ <- trimidd$query(wv[2])
   gamma1 <- (D21*N21 + D22*N22 + D23*N23)/numwv_2_
   if (gamma1 == 0){
      PKN2 <- PKN1
   } else {
      PKN2 <- max(numwv_23 - D, 0)/numwv_2_ + gamma1*PKN1 
   }
   
   #compute PKN3
   #compute gamma2
   #first compute N's
   beggram <- paste(wv[1], wv[2], sep = " ")
   N31  <- DictN31$query(beggram)
   N32  <- DictN32$query(beggram)
   N33  <- DictN33$query(beggram)
   
   #compute first part of numerator of PKN3
   newphrase <- paste(wv[1], wv[2], wv[3], sep =" ")
   numwv123 <- DictGrams3$query(newphrase)
   if (length(numwv123) == 0 | numwv123 == 0){
      numwv123 <- 0
      #  PKN3 <- PKN2
   } 
   
   #then pick which D is used for the discount
   if (numwv123 == 0) {
      D <- 0
   }  else if (numwv123 == 1) {
      D <- D31
   } else if (numwv123 == 2) {
      D <- D32
   } else if (numwv123 >= 3) {
      D <- D33
   } 
   
   #next compute number (not unique) of trigrams ending with wv[3]   
   numwv__3 <- tricomp$query(wv[3])
   gamma2 <- (D31*N31 + D32*N32 + D33*N33)/numwv__3
   if (gamma2 == 0){
      PKN3 <- PKN2
   } else {
      PKN3 <- max(numwv123 - D, 0)/numwv__3 + gamma2*PKN2 
   }
   
   return(PKN3)
}

#read in vector of all unigrams used in model
#calculate the proabability each one completes a three-word phrase

#create list of bigram vectors by sentence to calculate probability
#of training set to test if KN function is working correctly
#and find the wordwith the highest probability

counts1 <- readRDS("medium_rev_counts1.rds")
counts1 <- select(counts1, word1)
counts1 <- filter(counts1, word1 != "BOS" & word1 != "EOS")
lengcount <- dim(counts1)[1]

# a text cleaning function
# adapted from http://davevinson.com/cmscu-tutorial.html
cleanup <- function(line) { 
   # lower-case everything
   str <- tolower(line);
   # get rid of anything not a-z, ', or whitespace
   str <- gsub('[^a-z\'[:space:]]', '', str);
   # collapse whitespace
   str <- gsub('[[:space:]]+', ' ', str);
   # # make sure contraction's are "tight"
   str <- gsub(" ?' ?", "'", str);
   # get rid of leading and trailing whitespace
   str <- gsub("^\\s+|\\s+$", "", str)
   return(str);
}

maxprob <- function(x){
   if (x == "") {
      return("")
   } else {
   x <- cleanup(x)
   lasttwo <- tail(strsplit(x, split=" ")[[1]],2)
   lasttwo <- paste(lasttwo[1], lasttwo[2], sep = " ")
   y <- rep(lasttwo, lengcount, times = 1)
   y <- as.data.frame(y)
   z <- cbind(y, counts1)
   z$newphrase <- paste(z$y, z$word1, sep = " ")
   z$prob <- sapply(z$newphrase, KN_tri_all)
   return(z$word1[which.max(z$prob)])
   }
}
shinyServer(function(input, output) {
    
output$endword <- eventReactive(input$go, {
    maxprob(input$phrase)
})
})
