library(RMySQL)
library(dplyr)
library(tm)
library(plyr)
library(stringdist)
library(stringi)
library(stringr)

#######FUNCTIONS####  

phonetics<- function(author){
    #Step1 - Strip accents from Authors
    authors_noaccent <- stri_trans_general(author,"Latin-ASCII")
    #Step2 - Soundex
    phonetic(authors_noaccent)
}

year_distance<- function(data){
abs(as.numeric(as.character(data$year)) - as.numeric(as.character(data$year.1)))
}

jaccard_distance<- function(data,var1,var2){
  stringdist(data[,var1], data[,var2], method= "jaccard")
}

cosine_distance<- function(data,var1,var2){
  x<-stringdist(data[,var1], data[,var2], method= "cosine")
  x[which(x==Inf)] <- "" 
  x
}


#######FUNCTIONS####  

#######################################################
#############Connect and Read from DB##################
drv<- dbDriver("MySQL")
pw<- {"dmkm1234"}
ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
                     host= "127.0.0.1", port=8889,
                     user="root", password=pw 
)
rm(pw)

signature<- dbReadTable(ucscDb, "authors_signature")
#############Connect and Read from DB##################
#######################################################

#######################################################
#############         MODEL          ##################

#Get author and article
author = "Martin JM"
title = "HESS very-high-energy gamma-ray sources without identified counterparts"

author="Muller AA"


    #### Create block ####
phon = phonetics(author)
df = subset(signature, phonetic == phon)

ids <- combn(unique(df$sigID),2)
df <- data.frame(df[match(ids[1,], df$sigID), ], df[match(ids[2,], df$sigID), ])

    ##### Create Features #####
#Author Name Distance
df$dist_author = jaccard_distance(df,"author","author.1")
#Year
df$dist_year = year_distance(df)
#Coauthors Distance (jaccard)
df$dist_coauthor = jaccard_distance(df,"coauthors","coauthors.1")
#Keyword Distance   (cosine)
df$dist_keyword = cosine_distance(df,"keyword","keyword.1")

#Query for same author:   df %>% filter(sigID == 281058 & id == 25703 & id.1 == 49651)