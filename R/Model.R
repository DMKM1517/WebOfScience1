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
  firstname <- gsub("([A-Za-z]+).*", "\\1", authors_noaccent)
  #Step2 - Soundex
  phonetic(firstname)   #Deal with -Abdekabel  Ab de kabel-
}
phon_nysiis<- function(author){
  #Step1 - Strip accents from Authors
  authors_noaccent <- stri_trans_general(author,"Latin-ASCII")
  firstname <- gsub("([A-Za-z]+).*", "\\1", authors_noaccent)
  #Step2 - Nysiis
  nysiis(firstname)   
}

year_distance<- function(data){
  abs(as.numeric(as.character(data$year)) - as.numeric(as.character(data$year.1)))
}

jaccard_distance<- function(data,var1,var2){
  x<-stringdist(data[,var1], data[,var2], method= "jaccard")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
}

cosine_distance<- function(data,var1,var2){
  x<-stringdist(data[,var1], data[,var2], method= "cosine")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
}
jarowinker_distance<- function(data,var1,var2){
  x<-stringdist(data[,var1], data[,var2], method= "jw")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
}

Combinations = function(data){
  ids = combn(unique(data[,1]),2)
  df = data.frame(data[match(ids[1,], data[,1]), ], data[match(ids[2,], data[,1]), ])
  return(df)
}
#Jaccard distance from the first name initial of the authors
fname_initial_distance <- function(var1, var2){
  list1 <- strsplit(var1," ")
  list2 <- strsplit(var2, " ")
  t1 <- sapply(list1,function(x) x[2])
  t2 <- sapply(list2,function(x) x[2])
  stringdist(t1,t2, method = "jaccard")
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




    #### Create block ####
phon = phonetics(author)
data = subset(signature, phonetic == phon)

df = Combinations(data)

df = df %>% filter(author == "Muller AA" & title == "Millimeter-wave generation via frequency multiplication in graphene")

    ##### Create Features #####
#Author Name Distance
df$dist_author = jarowinker_distance(df,"author","author.1")
#Author Initial's Distance
df$dist_initials = fname_initial_distance(df$author,df$author.1)
#Title Distance
df$dist_title = jaccard_distance(df, "title","title.1")
#Year
df$dist_year = year_distance(df)
#Coauthors Distance (jaccard)
df$dist_coauthor = jaccard_distance(df,"coauthors","coauthors.1")
#Keyword Distance   (cosine)
df$dist_keyword = cosine_distance(df,"keyword","keyword.1")
#Journal Distance
df$dist_journal = cosine_distance(df,"journal","journal.1")
#Institution Distance
df$dist_institution = cosine_distance(df,"institution","institution.1")


#Prediction
#Load Models

library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
set.seed(123)

PredictForest = predict(AuthorForest , newdata = df)
PredictSVM = predict(AuthorSVM, newdata= df)
PredictGLM = predict(AuthorGLM, newdata = df, type='response')
PredictGLM = ifelse(PredictGLM > 0.5,1,0)
PredictCTree = predict(AuthorCTree, newdata = df)

#Ensemble
factorToNum = function(factor){as.numeric(as.character(factor))}
predictEnsemble = ifelse(((factorToNum(PredictForest)*2 + factorToNum(PredictSVM)*2 + 
                             PredictGLM + factorToNum(PredictCTree))*2  / 7) >= .5 , 1,0)

predictEnsemble = ifelse(((factorToNum(PredictForest) + factorToNum(PredictSVM) + factorToNum(PredictCTree))  / 3) >= .5 , 1,0)
df$label = predictEnsemble

titles = df %>% filter(label == 1) %>% select(author.1, id.1 ,title.1)
