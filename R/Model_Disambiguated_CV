library(RMySQL)
library(dplyr)
library(tm)
library(stringdist)
library(stringi)
library(stringr)
library(data.table)
library(phonics)

library(caTools)
library(randomForest) #RF
library(rpart)        #RPART
library(rpart.plot)
library(e1071)        #SVM #NB
library(caret)
library(party)        #CTREE
set.seed(111)
##################    FUNCTIONS    ##################  
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
  
  #Subset out combinations within the same phonetic
  df = df %>% filter(id!=id.1)
  df = df %>% filter(phonetic == phonetic.1)
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

features <- function(df){
  ##### Create Features for Training#####
  #Author Last Name Distance
  df$dist_author = jarowinker_distance(df,"author","author.1")
  #Author Initial's Distance
  df$dist_initials = fname_initial_distance(df$author,df$author.1)
  #Title Distance
  df$dist_title = cosine_distance(df, "title","title.1")
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
  
  df = df %>% select(sigID,sigID.1,dist_author,dist_initials,dist_title,dist_year,dist_coauthor,dist_keyword,dist_journal,dist_institution,label)
  return(df)
}
#Predictive Algorithms (RF, SVM, CART, CTree, GLM, NB)
models <- function(train, test, x){
  if(x == "RF"){ #Random Forest
    AuthorForest = randomForest(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                                dist_keyword  + dist_journal + dist_institution,
                                data=train, nodesize=25, ntree = 200)
    PredictForest = predict(AuthorForest , newdata = test)
    #Save results
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "RF","label" = as.factor(test$label),  "Prediction" = PredictForest)
  } else if(x == "SVM"){ 
    AuthorSVM =    svm( as.factor(label) ~dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                        dist_keyword  + dist_journal + dist_institution,
                        data=train)
    PredictSVM = predict(AuthorSVM, newdata= test)
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "SVM","label" = as.factor(test$label),  "Prediction" = PredictSVM)
  } else if(x== "CART"){
    AuthorCART = rpart(label ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                        dist_keyword  + dist_journal + dist_institution,
                        data=train, method = "class",minbucket = 25, cp=0.5)
    PredictCART = predict(AuthorCART, newdata = test, type = "class")
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "CART","label" = as.factor(test$label),  "Prediction" = PredictCART)
  } else if(x == "CTree"){
    AuthorCTree = ctree(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                          dist_keyword  + dist_journal + dist_institution,
                          data=train)
    PredictCTree = predict(AuthorCTree, newdata = test)
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "CTree","label" = as.factor(test$label),  "Prediction" = PredictCTree)
  } else if(x == "GLM"){
    AuthorGLM = glm(label ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                      dist_keyword + dist_keyword + dist_journal,  family = binomial(logit),
                    data=train)
    PredictGLM = predict(AuthorGLM, newdata = test, type='response')
    fitted.results = ifelse(PredictGLM > 0.5,1,0)
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "GLM","label" = as.factor(test$label),  "Prediction" = fitted.results)
  } else if(x =="NB"){
    AuthorNB = naiveBayes(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                            dist_keyword + dist_keyword + dist_journal + dist_institution,
                          data=train)
    PredictNB = predict(AuthorNB, newdata= test)
    temp = data.frame("iteration" = i, "sigID" = test$sigID, "sigID.1" = test$sigID.1, model = "NB","label" = as.factor(test$label),  "Prediction" = PredictNB)
    
  }
  return(temp)
}
##################    FUNCTIONS     ################## 

##################    DATA     ######################## 
#############   Connect and Read from DB  ##################
drv<- dbDriver("MySQL")
pw<- {"dmkm1234"}
ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
                     host= "127.0.0.1", port=8889,
                     user="root", password=pw 
) #3306
rm(pw)

signature<- dbReadTable(ucscDb, "authors_signature")

################  Disambiguated Authors   ####################
#SET WORKING DICTIONARY

setwd("/Users/saulgarcia/Desktop/Github/WebOfScience1/R")
disambiguated = read.csv("articles_authors_disambiguated.csv", header=TRUE)

########JOINS
signature<- left_join(signature, disambiguated)

#Filter by Disambiguated  [[BLOCK TO PAIR UP]]
disam = signature %>% filter(!is.na(authorid)) #Normal Join would avoid this step
#head(disam)


###################### MODELS   #######################

###################### Cross Validation ##################
authors = as.data.frame(unique(disam$phonetic))
names(authors)<-c("phonetic")
k=10
authors$fold = sample(rep(1:k,length=nrow(authors)))
list = 1:k
algorithms <- c("RF","SVM", "CART", "CTree", "GLM", "NB")
results <- data.frame()

for(i in 1:k){
  #Create Training and Testing Set for i fold
    #List
  trlist=subset(authors, fold %in% list[-i] )$phonetic
  telist=subset(authors, fold %in% i )$phonetic
    #Subset dataframe 
  train = filter(disam, phonetic %in% trlist)
  test = filter(disam, phonetic %in% telist )
    #Make combinations
  train = Combinations(train)
  test = Combinations(test) 
    #Create features
  train = features(train)
  test = features(test)
  
  for(algorithm in algorithms){
    predictions = models(train, test, algorithm)
    results <- rbind(results,predictions)
  }
}

#Build Confusion Matrix and validate Accuracy
results.table <- results %>% 
                    group_by(model) %>% 
                    summarise(accuracy = sum(label == Prediction) / n())
results.table
write.table(results, file = "resultsmatrix.csv")

#### ENSEMBLE MODEL
factorToNum = function(factor){as.numeric(as.character(factor))}

#(RF, SVM, CART, CTree, GLM, NB)
RF = results %>% filter(model=="RF") %>% select(Prediction)
SVM = results %>% filter(model=="SVM") %>% select(Prediction)
CART = results %>% filter(model=="CART") %>% select(Prediction)
CTree = results %>% filter(model=="CTree") %>% select(Prediction)
GLM = results %>% filter(model=="GLM") %>% select(Prediction)
NB = results %>% filter(model=="NB") %>% select(Prediction)

Label = results %>% filter(model=="RF") %>% select(label)
Label = factorToNum(Label[,1])


predictEnsemble = ifelse((factorToNum(RF[,1]) + 0*factorToNum(SVM[,1]) + 2*factorToNum(CART[,1]) +
                            0*factorToNum(CTree[,1]) + factorToNum(GLM[,1]) + 2*factorToNum(NB[,1]) )/6
                         >=.5 ,1,0)

cm = table(Label,predictEnsemble)
accuracyEnsemble = sum(diag(cm))/sum(cm)
accuracyEnsemble




###############################        GENERATE MODELS       ###################
#Prepare Full Dataset 
df = Combinations(disam)
#Create features
df = features(df)

###################### Feature Selection ##################

# Ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=6, repeats=2)
# train the model
model <- train(as.factor(label) ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                 dist_keyword  + dist_journal + dist_institution,
               data=df, method="lvq", preProcess="scale", trControl=control)

# Estimate variable importance
importance <- varImp(model, scale=FALSE)
# Summarize importance
print(importance)
# Plot importance
plot(importance)

###############################        RANDOM FOREST       ###################
AuthorForest = randomForest(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                              dist_keyword  + dist_journal + dist_institution,
                            data=df, nodesize=25, ntree = 200)
save(AuthorForest, file = "AuthorForest.rda")
#######################################      SVM     ######################################
AuthorSVM = svm(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                  dist_keyword  + dist_journal + dist_institution,
                  data=df)
save(AuthorSVM, file = "AuthorSVM.rda")

#################### RPART With CrosValidation   ##################
AuthorCART = rpart(label ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                     dist_keyword  + dist_journal + dist_institution,
                   data=df, method = "class", minbucket = 25, cp=0.5)
save(AuthorCART, file = "AuthorCART.rda")

############ CTREE
AuthorCTree = ctree(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                      dist_keyword  + dist_journal + dist_institution,
                    data=df)
save(AuthorCTree, file = "AuthorCTree.rda")

############ GLM   
AuthorGLM = glm(label ~ dist_author + dist_initials + dist_title + dist_year + dist_coauthor +
                  dist_keyword + dist_keyword + dist_journal,  family = binomial(logit),
                data=df)
save(AuthorGLM, file = "AuthorGLM.rda")

############# Naive Bayes
AuthorNB = naiveBayes(as.factor(label) ~ dist_author+ dist_initials + dist_title + dist_year + dist_coauthor +
                        dist_keyword + dist_keyword + dist_journal + dist_institution,
                      data=df)
save(AuthorNB, file = "AuthorNB.rda")


      
###################### MODELS   ##############
dbDisconnect(ucscDb)
rm(list=setdiff(ls(), "signature"))
