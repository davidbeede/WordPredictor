#CreateNgramsMedium_rev.R
library(dplyr)
library(quanteda)
library(data.table)

setwd("~/JHU_DataScience/CapstoneProject/Coursera-SwiftKey/final/en_US")
set.seed(as.numeric(as.Date("2017-3-21")))
#create n-grams function that tokenizes sentence data into n-grams
n_grams <- function(sent_vec, num) {
   any_vec <- tokenize(sent_vec, what = "word", ngrams = num,
                       removeNumbers = TRUE,
                       removePunct = FALSE,
                       removeSymbols = TRUE,
                       removeTwitter = TRUE,
                       removeURL = TRUE,
                       removeSeparators = FALSE,
                       concatenator = " ", simplify = TRUE)
}
#create 1 percent sample of data by taking 4 percent sample of trainingdata
sent_vec <- readRDS("train_sent.rds")
length(sent_vec)
unif_var <- runif(length(sent_vec))
sent_vec <- sent_vec[unif_var <= 0.04]
length(sent_vec)
#get rid of non-ASCII characters
sent_vec <- iconv(sent_vec, from = "UTF-8", to = "ASCII", sub = "")

#create vector of profane singleton words to be eliminated from corpora
#derived from the list found at http://bit.ly/2gEEx0r
profane <- scan(file = "profane_en_one.txt", what = "character")
profane <- as.data.frame(profane, stringsAsFactors = FALSE)
profane <- filter(profane, profane != "gay")
profane <-
   mutate(profane, subsword = as.character("UNK"), 
          elimword = as.character(profane))
profane <- select(profane, -profane)

#add beginning and/or end of sentence tags as needed
sent1 <- paste("BOS", sent_vec, sep = " ")
sent2 <- paste(sent1, "EOS", sep = " ")
sent3 <- paste("BOS", sent2, sep = " ")

#create unigrams
grams1 <- n_grams(sent2, 1)
length(grams1)

#create unigram data frame to make it easier to convert 
#profane words to UNK
gramsdf1 <- as.data.frame(grams1, stringsAsFactors = FALSE)
head(gramsdf1)
dim(gramsdf1)

#create frequency table of unigrams
gramsfrq1 <- as.data.frame(sort(table(gramsdf1$grams1), 
                               decreasing = TRUE), stringsAsFactors = FALSE)
dim(gramsfrq1)
head(gramsfrq1)
tail(gramsfrq1)

#Identify words occurring less than once in sample corpus
#create new variable subsword that initially is grams1 but gets changed for
#singleton and profane words
gramsfrq1 <- rename(gramsfrq1, elimword = Var1)
gramsfrq1 <- mutate(gramsfrq1, subsword = elimword)
gramsfrq1$subsword[gramsfrq1$Freq <= 1] <- "UNK"
head(gramsfrq1)
tail(gramsfrq1)

#replace profane and rare words with "UNK"
unk1 <- filter(gramsfrq1, subsword == "UNK")
head(unk1)
tail(unk1)
dim(unk1)
unk <- select(unk1, -Freq)
elimwrds <- rbind(unk, profane)
dim(elimwrds)
elimwrds <- unique(elimwrds)
dim(elimwrds)
gramsdf1 <- left_join(gramsdf1, elimwrds, by = c("grams1" = "elimword"))
gramsdf1 <- within(gramsdf1, grams1[subsword == "UNK"] <- "UNK")
gramsdf1 <- select(gramsdf1, grams1)
sum(gramsdf1$grams1 == "UNK")
dim(gramsdf1)

#put together final df with word counts
known1 <- filter(gramsfrq1, subsword != "UNK")
known1 <- select(known1, -elimword)
dim(known1)
unk1 <- group_by(unk1, subsword)
unk1 <- summarize(unk1, Freq = sum(Freq))
unk1 
counts1 <- rbind(known1, unk1)
counts1 <- rename(counts1, word1 = subsword)
dim(counts1)

#Save final unigrams (including UNKs)
saveRDS(gramsdf1, "medium_rev_grams1.rds")
saveRDS(counts1, "medium_rev_counts1.rds")
saveRDS(elimwrds, "medium_rev_elimwrds.rds")
rm(grams1, gramsdf1, unk1, unk, known1, counts1, gramsfrq1, profane)

#Create bigrams
grams2 <- n_grams(sent2, 2)
length(grams2)
gramsdf2 <- as.data.frame(grams2, stringsAsFactors = FALSE)
head(gramsdf2)

#create counts of bigrams to drop singletons
counts2 <- as.data.frame(sort(table(gramsdf2$grams2), 
                              decreasing = TRUE), stringsAsFactors = FALSE)
head(counts2)
tail(counts2)

#counts2 <- filter(counts2, Freq > 1)
#tail(counts2)
counts2 <- rename(counts2, grams2 = Var1)
counts2 <- mutate(counts2, 
                  word1 = sapply(strsplit(grams2, " "), "[[", 1),
                  word2 = sapply(strsplit(grams2, " "), "[[", 2))
head(counts2)
tail(counts2)
counts2 <- left_join(counts2, elimwrds, by = c("word1"="elimword"))
counts2 <- within(counts2, word1[subsword == "UNK"] <- "UNK")
counts2 <- select(counts2, -subsword)
head(counts2)
tail(counts2)
sum(counts2$word1 == "UNK")
counts2 <- left_join(counts2, elimwrds, by = c("word2" = "elimword"))
head(counts2)
tail(counts2)
counts2 <- within(counts2, word2[subsword == "UNK"] <- "UNK")
counts2 <- select(counts2, -subsword)
head(counts2)
tail(counts2)
sum(counts2$word2 == "UNK")
counts2 <- select(counts2, -grams2)
counts2 <- mutate(counts2, grams2 = paste(word1, word2, sep = " "))
head(counts2)
tail(counts2)
counts2 <- group_by(counts2, grams2)
counts2 <- summarize(counts2, Freq = sum(Freq))
counts2 <- as.data.frame(counts2)
counts2 <- mutate(counts2, 
                  word1 = sapply(strsplit(grams2, " "), "[[", 1),
                  word2 = sapply(strsplit(grams2, " "), "[[", 2))
head(counts2)
tail(counts2)
dim(counts2)
dt <- data.table(counts2)
dt <- dt[rep(seq(1, nrow(dt)), dt$Freq)]
dt <- select(dt, -Freq)
gramsdf2 <- as.data.frame(dt)
head(gramsdf2)
tail(gramsdf2)

#Save final bigrams 
saveRDS(gramsdf2, "medium_rev_grams2.rds")
saveRDS(counts2, "medium_rev_counts2.rds")
rm(grams2, gramsdf2, counts2)

#Create trigrams
grams3 <- n_grams(sent3, 3)
length(grams3)
gramsdf3 <- as.data.frame(grams3, stringsAsFactors = FALSE)
head(gramsdf3)

#create counts of bigrams to drop singletons
counts3 <- as.data.frame(sort(table(gramsdf3$grams3), 
                              decreasing = TRUE), stringsAsFactors = FALSE)
head(counts3)
tail(counts3)

#counts3 <- filter(counts3, Freq > 1)
#tail(counts3)
counts3 <- rename(counts3, grams3 = Var1)
counts3 <- mutate(counts3, 
                  word1 = sapply(strsplit(grams3, " "), "[[", 1),
                  word2 = sapply(strsplit(grams3, " "), "[[", 2),
                  word3 = sapply(strsplit(grams3, " "), "[[", 3))
head(counts3)
tail(counts3)
counts3 <- left_join(counts3, elimwrds, by = c("word1"="elimword"))
counts3 <- within(counts3, word1[subsword == "UNK"] <- "UNK")
counts3 <- select(counts3, -subsword)
head(counts3)
tail(counts3)
sum(counts3$word1 == "UNK")
counts3 <- left_join(counts3, elimwrds, by = c("word2" = "elimword"))
head(counts3)
tail(counts3)
counts3 <- within(counts3, word2[subsword == "UNK"] <- "UNK")
counts3 <- select(counts3, -subsword)
head(counts3)
tail(counts3)
sum(counts3$word2 == "UNK")
counts3 <- left_join(counts3, elimwrds, by = c("word3" = "elimword"))
head(counts3)
tail(counts3)
counts3 <- within(counts3, word3[subsword == "UNK"] <- "UNK")
counts3 <- select(counts3, -subsword)
head(counts3)
tail(counts3)
sum(counts3$word3 == "UNK")

counts3 <- select(counts3, -grams3)
counts3 <- mutate(counts3, grams3 = paste(word1, word2, word3, sep = " "))
head(counts3)
tail(counts3)
counts3 <- group_by(counts3, grams3)
counts3 <- summarize(counts3, Freq = sum(Freq))
counts3 <- as.data.frame(counts3)
counts3 <- mutate(counts3, 
                  word1 = sapply(strsplit(grams3, " "), "[[", 1),
                  word2 = sapply(strsplit(grams3, " "), "[[", 2),
                  word3 = sapply(strsplit(grams3, " "), "[[", 3))
head(counts3)
tail(counts3)
dim(counts3)
dt <- data.table(counts3)
dt <- dt[rep(seq(1, nrow(dt)), dt$Freq)]
dt <- select(dt, -Freq)
gramsdf3 <- as.data.frame(dt)
head(gramsdf3)
tail(gramsdf3)

#Save final trigrams 
saveRDS(gramsdf3, "medium_rev_grams3.rds")
saveRDS(counts3, "medium_rev_counts3.rds")
rm(grams3, gramsdf3, counts3)

