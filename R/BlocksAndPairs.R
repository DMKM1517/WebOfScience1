library(stringr)
library(RMySQL)
library(dplyr)
library(tm)
library(plyr)
#install.packages('stringdist')
library(stringdist)
library(stringi)

drv<- dbDriver("MySQL")
pw<- {"dmkm1234"}
ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
                     host= "127.0.0.1", port=8889,
                     user="root", password=pw 
)
rm(pw)
########################
#Read Dataset of Signatures
signatures<- dbReadTable(ucscDb, "authors_signature")

#Step1 - Strip accents from Authors
authors_noaccent <- stri_trans_general(signatures$author,"Latin-ASCII")

#Step2 - Soundex
signatures$phonetic <- as.data.frame(phonetic(authors_noaccent))

#Step3 - Unique ID
unique_id <- rownames(signatures)
signatures$unique_id = unique_id

#Step4 - Groupping by Phonetics - Blocks
blocks<- split(signatures, signatures$phonetic)

#Step5 - Pair the signatures  This code extracts the combinations of each signature on each block, and portrays each signature side by side for comparing purposes.
blocks_paired<-lapply(blocks[[1]], function(x) data.frame(x[match(combn(unique(x$unique_id),2)[1,], x$unique_id), ], x[match(combn(unique(x$unique_id),2)[2,], x$unique_id), ]))

## Our signatures has **2,328,539 observations, where there exists 477,899 unique names, but grouped by phonetics, 
# they are reduced to 6236

#ids2 <- combn(unique(test1$id),2)
#data.frame(test1[match(ids2[1,], test1$id), ], test1[match(ids2[2,], test1$id), ])

lapply(list1, function(x) combn(unique(x$id),2))
data.frame(x[match(combn(unique(x$id),2)[1,], x$id), ], x[match(combn(unique(x$id),2)[2,], x$id), ])
lapply(blocks, function(x) data.frame(x[match(combn(unique(x$id),2)[1,], x$id), ], x[match(combn(unique(x$id),2)[2,], x$id), ]))
#For list1?
#######Disconect########
dbDisconnect(ucscDb)
