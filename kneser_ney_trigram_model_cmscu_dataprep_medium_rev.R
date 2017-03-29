#kneser_ney_trigram_model_cmscu_dataprep_medium_rev.R
library(utils)
library(qdap)
library(data.table)
library(cmscu)
library(dplyr)

setwd("~/JHU_DataScience/CapstoneProject/Coursera-SwiftKey/final/en_US")

options(max.print = 10)
x <- c(1:3)
for (i in 1:3) {
   eval(parse(text = paste0('counts', x[i], ' <- readRDS("medium_rev_counts', x[i], '.rds")')))
   eval(parse(text = paste0('grams', x[i], ' <- readRDS("medium_rev_grams', x[i], '.rds")')))
   eval(parse(text = paste0("DictGrams", x[i], 
                            " <- new(FrequencyDictionary, 4, 2^22)")))
   eval(parse(text = paste0("DictGrams", x[i], 
                            "$store(grams", x[i], "$grams", x[i], ")")))
   eval(parse(text = paste0('DictGrams', x[i], 
                            '$save("grams', x[i], '.bin")')))
}


#diagnostics
DictGrams1$uncertainty
DictGrams1$confidence
DictGrams1$density

#define test trigram
tstwv <- c("one", "of", "the")

#unique trigram prefixes completed by each bigram
newfile <- counts3
newfile <- mutate(newfile, endgram2 = paste(word2, word3, sep = " "))
newfile <- group_by(newfile, endgram2)
newfile <- summarize(newfile, npref = n())
newdt <- data.table(newfile)
repdt <- newdt[rep(seq(1, nrow(newdt)), newdt$npref)]
repdt <- select(repdt, -npref)
tripref <- new(FrequencyDictionary, 4, 2^22)
tripref$store(repdt$endgram2)
tripref$uncertainty
tripref$confidence
tripref$density
tripref$save("tripref.bin")
#test
tripref$query("of the")
dim(counts3[counts3$word2 == "of" & counts3$word3 == "the",])[1]

#total trigram prefixes completed by each unigram
tricomp <- counts3
tricomp <- group_by(tricomp, word3)
tricomp <- summarize(tricomp, newfreq = sum(Freq))
tricompdt <- data.table(tricomp)
reptricomp <- tricompdt[rep(seq(1, nrow(tricompdt)), tricompdt$newfreq)]
reptricomp <- select(reptricomp, -newfreq)
tricomp <- new(FrequencyDictionary, 4, 2^22)
tricomp$store(reptricomp$word3)
tricomp$uncertainty
tricomp$confidence
tricomp$density
tricomp$save("tricomp.bin")
#test
tricomp$query("the")
sum(counts3$Freq[counts3$word3 == "the"])

#trigram "middles" completed by each bigram
newfile <- counts3
newfile <- arrange(newfile, word2)
newfile <- group_by(newfile, word2)
newfile <- summarize(newfile, npref = n())
newdt <- data.table(newfile)
repdt <- newdt[rep(seq(1, nrow(newdt)), newdt$npref)]
repdt <- select(repdt, -npref)
trimidd <- new(FrequencyDictionary, 4, 2^21)
trimidd$store(repdt$word2)
trimidd$uncertainty
trimidd$confidence
trimidd$density
trimidd$save("trimidd.bin")
#test
trimidd$query("of")
dim(counts3[counts3$word2 == "of",])[1]

#bigram prefixes completed by each unigram
newfile <- counts2
newfile <- group_by(newfile, word2)
newfile <- summarize(newfile, nbisecnd = n())
newdt <- data.table(newfile)
repdt <- newdt[rep(seq(1, nrow(newdt)), newdt$nbisecnd)]
repdt <- select(repdt, -nbisecnd)
bipref <- new(FrequencyDictionary, 4, 2^20)
bipref$store(repdt$word2)
bipref$uncertainty
bipref$confidence
bipref$density
bipref$save("bipref.bin")
#test
bipref$query("of")
dim(counts2[counts2$word2 == "of",])[1]

DictN21 <- new(FrequencyDictionary, 4, 2^20)
DictN22 <- new(FrequencyDictionary, 4, 2^18)
DictN23 <- new(FrequencyDictionary, 4, 2^18)
DictN31 <- new(FrequencyDictionary, 4, 2^21)
DictN32 <- new(FrequencyDictionary, 4, 2^18)
DictN33 <- new(FrequencyDictionary, 4, 2^18)

#generate N2's
nfile <- counts2
nfile <- mutate(nfile, gram = word1)
nfile <- filter(nfile, Freq == 1)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN21$store(nfile$gram)
DictN21$uncertainty
DictN21$confidence
DictN21$density
DictN21$save("DictN21.bin")

nfile <- counts2
nfile <- mutate(nfile, gram = word1)
nfile <- filter(nfile, Freq == 2)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN22$store(nfile$gram)
DictN22$uncertainty
DictN22$confidence
DictN22$density
DictN22$save("DictN22.bin")

nfile <- counts2
nfile <- mutate(nfile, gram = word1)
nfile <- filter(nfile, Freq >= 3)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = sum(wrdcount))
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN23$store(nfile$gram)
DictN23$uncertainty
DictN23$confidence
DictN23$density
DictN23$save("DictN23.bin")
#check N2's
dim(counts2[counts2$word1 == tstwv[1] & counts2$Freq == 1,])[1]
DictN21$query(tstwv[1])
dim(counts2[counts2$word1 == tstwv[1] & counts2$Freq == 2,])[1]
DictN22$query(tstwv[1])
dim(counts2[counts2$word1 == tstwv[1] & counts2$Freq >= 3,])[1]
DictN23$query(tstwv[1])

#calculate N3's
nfile <- counts3
nfile <- mutate(nfile, gram = paste(word1, word2, sep = " "))
nfile <- filter(nfile, Freq == 1)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN31$store(nfile$gram)
DictN31$uncertainty
DictN31$confidence
DictN31$density
DictN31$save("DictN31.bin")

nfile <- counts3
nfile <- mutate(nfile, gram = paste(word1, word2, sep = " "))
nfile <- filter(nfile, Freq == 2)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN32$store(nfile$gram)
DictN32$uncertainty
DictN32$confidence
DictN32$density
DictN32$save("DictN32.bin")

nfile <- counts3
nfile <- mutate(nfile, gram = paste(word1, word2, sep = " "))
nfile <- filter(nfile, Freq >= 3)
nfile <- select(nfile, -Freq)
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = n())
nfile <- group_by(nfile, gram)
nfile <- summarize(nfile, wrdcount = sum(wrdcount))
nfile <- data.table(nfile)
nfile <- nfile[rep(seq(1, nrow(nfile)), nfile$wrdcount)]
nfile <- select(nfile, -wrdcount)
print(dim(nfile))
DictN33$store(nfile$gram)
DictN33$uncertainty
DictN33$confidence
DictN33$density
DictN33$save("DictN33.bin")

tstwv12 <- paste(tstwv[1], tstwv[2], sep = " ")
#check N3's
dim(counts3[counts3$word1 == tstwv[1] & counts3$word2 == tstwv[2] &
               counts3$Freq == 1,])[1]
DictN31$query(tstwv12)
dim(counts3[counts3$word1 == tstwv[1] & counts3$word2 == tstwv[2] &
               counts3$Freq == 2,])[1]
DictN32$query(tstwv12)
dim(counts3[counts3$word1 == tstwv[1] & counts3$word2 == tstwv[2] &
               counts3$Freq >= 3,])[1]
DictN33$query(tstwv12)
