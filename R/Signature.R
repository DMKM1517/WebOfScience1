library(stringr)
library(RMySQL)
library(dplyr)
library(tm)
library(plyr)

drv<- dbDriver("MySQL")
pw<- {"dmkm1234"}
ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
                     host= "127.0.0.1", port=8889,
                     user="root", password=pw 
)
rm(pw)

# Goal: "id","d", "author","title", "coauthors","journal","year", "keyword", "institution"

articles_data<- dbReadTable(ucscDb,"articles_1")
articles_only<-dbReadTable(ucscDb,"articles")
keywords<- dbReadTable(ucscDb,"articles_keywords_clean_sel")
institution <- dbReadTable(ucscDb, "articles_institutions")
position <- dbReadTable(ucscDb,"articles_authors")



#########Joining the position to the articles_data table
articles<- articles_only %>% select(id, title, authors, journal, year)
articles_data<- left_join(position, articles)

#Extracting features for the signature

#features = articles_only %>% select(id, title, authors, journal, year)

keywords_agg = aggregate(keyword~id, paste , collapse=",", data= keywords)

institution$d3 = paste0(institution$id,",",institution$d1)
institution = aggregate(institution~d3, paste , collapse=",", data= institution) 
institution_missing = ldply(strsplit(institution$d3, split =","))
institution_agg = cbind(institution_missing,institution) %>% select(V1, V2 , institution)
names(institution_agg) = c("id", "d", "institution")
institution_agg$id = as.integer(institution_agg$id)
institution_agg$d = as.integer(institution_agg$d)


#Join to create signatures
signature<- left_join(articles_data, keywords_agg)
signature<- left_join(signature,institution_agg)
signature[is.na(signature)]<-''
#head(signature)

#Convert authors into co-authors only
coauthors <-str_replace_all(signature$authors, ',', '')
coauthors <- str_replace_all(coauthors, ';', ',')

coauthors <- str_replace_all(coauthors, signature$author, '')
coauthors <- str_replace_all(coauthors,'^, ' , '') 
coauthors <- str_replace_all(coauthors,', ,', ',') #and ending comma
coauthors <- str_replace_all(coauthors,', $', '')

signature$authors <- coauthors
names(signature) <- c("id","d", "author","title", "coauthors","journal","year", "keyword", "institution")

dbRemoveTable(ucscDb, "authors_signature")
dbWriteTable(ucscDb, "authors_signature", signature)

dbDisconnect(ucscDb)
