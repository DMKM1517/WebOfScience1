library(RMySQL)
library(dplyr)
library(tm)
library(stringdist)
library(stringi)
library(stringr)
library(data.table)

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
  x<-stringdist(data[,var1], data[,var2], method= "jaccard")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
}

cosine_distance<- function(data,var1,var2){
  x<-stringdist(data[,var1], data[,var2], method= "cosine")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
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



##############################################################
################  Disambiguated Authors   ####################
  #SET WORKING DICTIONARY
setwd("/Users/saulgarcia/Desktop/DATA_DMKM/Preprocess DMKM")
disambiguated = read.csv("articles_authors_disambiguated.csv", header=TRUE)


#   #Remove empty completenames
# disambiguated = disambiguated %>% filter(completename != "")
#   #Remove accents
# disambiguated$completename = stri_trans_general(disambiguated$completename,"Latin-ASCII")
# disambiguated = disambiguated[,c(1:4)]
#   #Make a unique ID
# disambiguated$idid = paste0(disambiguated$id,",",disambiguated$d)
#   #Remove Duplicates
# disambiguated =  disambiguated %>% distinct(id,d)




#########TEST
# #Make a unique ID
# disambiguated$idid = paste0(disambiguated$id,",",disambiguated$d)
# 
# #Frequency by UniqueID
# test=disambiguated %>% group_by(id,d) %>% filter(n()>1)
# 
# test2 = disambiguated[!test,]
# disambiguated %>% filter(idid =="2689,1")
#############TEST

################  Disambiguated Authors   ####################
##############################################################

#JOIN
signature<- left_join(signature, disambiguated)

#Filter by Disambiguated  [[BLOCK TO PAIR UP]]
disam = signature %>% filter(!is.na(authorid))
head(disam)

#####Create combinations
ids <- combn(unique(disam$sigID),2)
df <- data.frame(disam[match(ids[1,], disam$sigID), ], disam[match(ids[2,], disam$sigID), ])
head(df)
#######


##### Create Features #####
#Author Name Distance
df$dist_author = jaccard_distance(df,"author","author.1")
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
#Label
df$label = as.numeric(df$authorid==df$authorid.1)


#######################################################
###################### MODELS   #######################
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
########### RANDOM FOREST
# sub0 = subset(df, label==0)[1:5131,]
# sub1 = subset(df, label==1)
# df2 = rbind(sub0,sub1)

spl = sample.split(df2$label, SplitRatio = 0.7)

train = subset(df, spl == TRUE)
test = subset(df, spl == FALSE)

AuthorForest = randomForest(as.factor(label) ~ dist_author + dist_title + dist_year + dist_coauthor +
                              dist_keyword + dist_keyword + dist_journal,
                              data=train, nodesize=25, ntree = 200)
PredictForest = predict(AuthorForest , newdata = test)
cm = table(test$label, PredictForest)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy  #[1] 0.9964035 or [1] 0.9491426 considering only real

#################### RPART With CrosValidation
library(caret)
library(e1071)
set.seed(111)

numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cpGrid = expand.grid(.cp = seq(0.01,0.5, 0.01))
train(as.factor(label) ~ dist_author + dist_title + dist_year + dist_coauthor +
        dist_keyword + dist_keyword + dist_journal,
      data=train, method="rpart" , trControl = numFolds, tuneGrid= cpGrid)

AuthorTree = rpart(label ~ dist_author + dist_title + dist_year + dist_coauthor +
                    dist_keyword + dist_keyword + dist_journal,
                    data=train,
                    method = "class",
                    minbucket = 25, cp=0.5)

PredictCART = predict(AuthorTree, newdata = test, type = "class") #This is like getting Threshold 0.5
cm = table(test2$label, PredictCART)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.9951842

###################### Random Forest    ##############
#######################################################
dbDisconnect(ucscDb)
