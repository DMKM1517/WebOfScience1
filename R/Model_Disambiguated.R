library(RMySQL)
library(dplyr)
library(tm)
library(stringdist)
library(stringi)
library(stringr)
library(data.table)
library(phonics)

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
  nysiis(firstname, maxCodeLen = 8)  
}

#Notes: For authors we can use Levenshtein distance or Jairo Wirkler

Combinations = function(data){
  ids = combn(unique(data[,1]),2)
  df = data.frame(data[match(ids[1,], data[,1]), ], data[match(ids[2,], data[,1]), ])
  return(df)
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
) #3306
rm(pw)

signature<- dbReadTable(ucscDb, "authors_signature")
#############Connect and Read from DB##################
#######################################################



##############################################################
################  Disambiguated Authors   ####################
  #SET WORKING DICTIONARY

setwd("/Users/saulgarcia/Desktop/Github/WebOfScience1/R")
disambiguated = read.csv("articles_authors_disambiguated.csv", header=TRUE)

################  Disambiguated Authors   ####################
##############################################################

#JOIN
signature<- left_join(signature, disambiguated)

#Filter by Disambiguated  [[BLOCK TO PAIR UP]]
disam = signature %>% filter(!is.na(authorid)) #Normal Join would avoid this step
head(disam)

#jose$idd = paste(jose$id,".",jose$d)
#disam$idd = paste(disam$id,".",disam$d)
#test = subset(disam, idd %in% jose$idd)
#`%ni%`<- Negate(`%in%`) 
#train = subset(disam, idd %ni% jose$idd)

#####Create combinations for training and testing set
authors = as.data.frame(unique(disam$phonetic))
names(authors)<-c("phonetic")
k=10
authors$fold = sample(rep(1:k,length=nrow(authors)))
list = 1:k
trlist=subset(authors, fold %in% list[-3] )$phonetic
telist=subset(authors, fold %in% 3 )$phonetic
train = filter(disam, phonetic %in% trlist)
test = filter(disam, phonetic %in% telist )


train = Combinations(train) #
train = train %>% filter(id!=id.1)
train = train %>% filter(phonetic == phonetic.1)
test = Combinations(test) #
test = test %>% filter(id!=id.1)
test = test %>% filter(phonetic == phonetic.1)
#######


##### Create Features for Training#####
#Author Last Name Distance
train$dist_author = jarowinker_distance(train,"author","author.1")
#Author Initial's Distance
train$dist_initials = fname_initial_distance(train$author,train$author.1)
#Title Distance
train$dist_title = cosine_distance(train, "title","title.1")
#Year
train$dist_year = year_distance(train)
#Coauthors Distance (jaccard)
train$dist_coauthor = jaccard_distance(train,"coauthors","coauthors.1")
#Keyword Distance   (cosine)
train$dist_keyword = cosine_distance(train,"keyword","keyword.1")
#Journal Distance
train$dist_journal = cosine_distance(train,"journal","journal.1")
#Institution Distance
train$dist_institution = cosine_distance(train,"institution","institution.1")
#Label
train$label = as.numeric(train$authorid==train$authorid.1)

##### Create Features for Testing#####
#Author Last Name Distance
test$dist_author = jarowinker_distance(test,"author","author.1")
#Author Initial's Distance
test$dist_initials = fname_initial_distance(test$author,test$author.1)
#Title Distance
test$dist_title = cosine_distance(test, "title","title.1")
#Year
test$dist_year = year_distance(test)
#Coauthors Distance (jaccard)
test$dist_coauthor = jaccard_distance(test,"coauthors","coauthors.1")
#Keyword Distance   (cosine)
test$dist_keyword = cosine_distance(test,"keyword","keyword.1")
#Journal Distance
test$dist_journal = cosine_distance(test,"journal","journal.1")
#Institution Distance
test$dist_institution = cosine_distance(test,"institution","institution.1")
#Label
test$label = as.numeric(test$authorid==test$authorid.1)


#######################################################
###################### MODELS   #######################
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
set.seed(123)



###################### Feature Selection ##################

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=6, repeats=2)
# train the model
model <- train(as.factor(label) ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                 dist_keyword  + dist_journal + dist_institution,
               data=train[1:10000,], method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

###################### Cross Validation ##################
results <- data.frame()
for(i in 1:k){
  trlist=subset(authors, fold %in% list[-i] )$authorid
  telist=subset(authors, fold %in% i )$authorid
  train = filter(df, authorid %in% trlist & authorid.1 %in% trlist )
  test = filter(df, authorid %in% telist | authorid.1 %in% telist)
  #Forest
  AuthorForest = randomForest(as.factor(label) ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                                dist_keyword + dist_journal + dist_institution,
                              data=train, nodesize=25, ntree = 100)
  PredictForest = predict(AuthorForest , newdata = test)
  #Save results
  temp = cbind(test$label, PredictForest)
  results <- rbind(results, temp)
}
#Build Confusion Matrix and validate Accuracy
cm = table(results[,1], results[,2])
accuracy = sum(diag(cm))/sum(cm)


# results <- rbind(results, temp)
# cm=table(results[,1], results[,2])

###############################        RANDOM FOREST       ###################
AuthorForest = randomForest(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                              dist_keyword + dist_keyword + dist_journal + dist_institution,
                            data=train, nodesize=25, ntree = 200)
PredictForest = predict(AuthorForest , newdata = test)
cm = table(test$label, PredictForest)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy  
#error = sqrt((sum(test$label - PredictForest)^2)/nrow(test))

save(AuthorForest, file = "AuthorForest.rda")
#######################################      SVM     ######################################

AuthorSVM = svm(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                              dist_keyword + dist_keyword + dist_journal + dist_institution,
                            data=train)
PredictSVM = predict(AuthorSVM, newdata= test)
cm = table(test$label, PredictSVM)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy  # [1] 0.9952765  on test  or [1] 0.9585501 considering sub1  #[1] 0.9946418

save(AuthorSVM, file = "AuthorSVM.rda")

#################### RPART With CrosValidation
library(caret)
library(e1071)
set.seed(111)

numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cpGrid = expand.grid(.cp = seq(0.01,0.5, 0.01))
train(as.factor(label) ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
        dist_keyword + dist_keyword + dist_journal,
      data=train, method="rpart" , trControl = numFolds, tuneGrid= cpGrid)

AuthorCART = rpart(label ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                    dist_keyword + dist_keyword + dist_journal,
                    data=train,
                    method = "class",
                    minbucket = 25, cp=0.5)

PredictCART = predict(AuthorCART, newdata = test, type = "class") #This is like getting Threshold 0.5
cm = table(test$label, PredictCART)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.9951842 test or [1] 0.9610056 in sub1

save(AuthorCART, file = "AuthorCART.rda")

############ CTREE
library(party)
AuthorCTree = ctree(as.factor(label) ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                                       dist_keyword + dist_keyword + dist_journal,
                                     data=train)
PredictCTree = predict(AuthorCTree, newdata = test)
cm = table(test$label, PredictCTree)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #     [1] 0.9959342 test or    [1] 0.9466238 on sub1

save(AuthorCTree, file = "AuthorCTree.rda")

############ GLM    ###  Not very good
AuthorGLM = glm(label ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                      dist_keyword + dist_keyword + dist_journal,  family = binomial(logit),
                    data=train)
PredictGLM = predict(AuthorGLM, newdata = test, type='response')
fitted.results = ifelse(PredictGLM > 0.5,1,0)
cm = table(test$label, fitted.results)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #   [1] 0.9946264 test           [1] 0.9311312 on sub1

save(AuthorGLM, file = "AuthorGLM.rda")

#error<-sqrt((sum((test$label-as.numeric(as.character(PredictForest)))^2))/nrow(test))

#### ENSEMBLE MODEL
factorToNum = function(factor){as.numeric(as.character(factor))}
predictEnsemble = ifelse(((factorToNum(PredictForest)*4 + factorToNum(PredictSVM)*2 + fitted.results +
                           factorToNum(PredictCTree))*2 + factorToNum(PredictCART)*6 / 15) >= .5 , 1,0)
predictEnsemble = ifelse(((factorToNum(PredictForest) + factorToNum(PredictSVM) + factorToNum(PredictCTree))  / 3) >= .5 , 1,0)

cm = table(test$label, predictEnsemble)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #       [1] 0.9954726


# ############ Naive Bayes
AuthorNB = naiveBayes(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                  dist_keyword + dist_keyword + dist_journal + dist_institution,
                data=train)
PredictNB = predict(AuthorNB, newdata= test)
cm = table(test$label, PredictNB)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy  # [1] 0.9952765  on test  or [1] 0.9585501 considering sub1  #[1] 0.9946418



###################### MODELS   ##############
#######################################################
dbDisconnect(ucscDb)
rm(list=setdiff(ls(), "signature"))
