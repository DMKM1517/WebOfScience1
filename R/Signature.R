library(RMySQL)
library(dplyr)
library(tm)
library(plyr)
library(stringdist)
library(stringi)
library(stringr)
library(phonics)

#######################################################
#################CONNECT TO DATABASE###################
drv<- dbDriver("MySQL")
pw<- {"dmkm1234"}
ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
                     host= "127.0.0.1", port=8889,
                     user="root", password=pw 
)
rm(pw)
#################CONNECT TO DATABASE###################
#######################################################


#######################################################
#################READ DATASETS#########################

# Goal:  Create Signature with following features
# ("id","d", "author","title", "coauthors","journal","year", "keyword", "institution")

articles<-dbReadTable(ucscDb,"articles")
keywords<- dbReadTable(ucscDb,"articles_keywords_clean_sel")
institution <- dbReadTable(ucscDb, "articles_institutions")
position <- dbReadTable(ucscDb,"articles_authors")

#################READ DATASETS#########################
#######################################################

#######################################################
######### PreProcess Dataframes for Joins #############

#Preprocess Keywords: Aggregate keywords (Keywords per article id)
keywords_agg = aggregate(keyword~id, paste , collapse=",", data= keywords)

#Preprocess Institutions: 
#New column with unique ID for author-institution
institution$d3 = paste0(institution$id,",",institution$d1)
#Aggregate institutions (Institutions per Author)
institution = aggregate(institution~d3, paste , collapse=",", data= institution)
#Recover id, and distance
institution_missing = ldply(strsplit(institution$d3, split =","))
#Bind together into dataframe
institution_agg = cbind(institution_missing,institution) %>% select(V1, V2 , institution)
#Rename
names(institution_agg) = c("id", "d", "institution")
#Correct format
institution_agg$id = as.integer(institution_agg$id)
institution_agg$d = as.integer(institution_agg$d)

#Preprocess Coauthors
coauthors = aggregate(author~id, paste , collapse=", ", data= position) #
colnames(coauthors) = c("id","coauthors")


######### PreProcess Dataframes for Joins #############
#######################################################


#######################################################
###################  JOINS  ###########################
#Join to create signatures
articles<- articles %>% select(id, title,journal, year) #Put back author if it doesnt work
signature<- left_join(position, articles)
signature<- left_join(signature, keywords_agg)
signature<- left_join(signature,institution_agg)
signature<-left_join(signature, coauthors)
signature[is.na(signature)]<-''
#head(signature)

rm(coauthors, institution_agg, institution_missing, institution, keywords,keywords_agg,position)

#Convert authors into co-authors only
#Remove author from coauthors
coauthors <- str_replace_all(signature$coauthors, signature$author, '')
#Remove starting and ending comma
coauthors <- str_replace_all(coauthors,'^, |, $' , '') 
#Remove middle commas
coauthors <- str_replace_all(coauthors,', ,', ',')

signature$coauthors <- coauthors
#names(signature) <- c("id","d", "author","title", "coauthors","journal","year", "keyword", "institution")
rm(articles, coauthors)
#################### JOINS ############################
#######################################################

#######################################################
#################### PHONETIC #########################
phonetics<- function(author){
  #Step1 - Strip accents from Authors
  authors_noaccent <- stri_trans_general(author,"Latin-ASCII")
  firstname <- gsub("([A-Za-z]+).*", "\\1", authors_noaccent)
  #Step2 - Soundex
  phonetic(authors_noaccent)   #Deal with -Abdekabel  Ab de kabel-
}

phon_nysiis<- function(author){
  #Step1 - Strip accents from Authors
  authors_noaccent <- stri_trans_general(author,"Latin-ASCII")
  firstname <- gsub("([A-Za-z]+).*", "\\1", authors_noaccent)
  #Step2 - Nysiis
  nysiis(firstname, maxCodeLen = 8) 
}

#Step2 - Soundex
signature$phonetic <- phon_nysiis(signature$author)

#Step3 - Unique ID
unique_id <- rownames(signature)
signature$sigID = unique_id
#Reorder
signature = signature[, c(11,3,1,2,4,9,5:8,10)]
#################### PHONETIC #########################
#######################################################



#######################################################
###########  WRITE TO DB AND DISCONNECT  ##############

dbRemoveTable(ucscDb, "authors_signature")
dbWriteTable(ucscDb, "authors_signature", signature)

dbDisconnect(ucscDb)

###########  WRITE TO DB AND DISCONNECT  ##############
#######################################################