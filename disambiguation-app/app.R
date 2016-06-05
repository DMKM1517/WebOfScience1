library(RMySQL)
library(dplyr)
library(tm)
library(plyr)
library(stringdist)
library(stringi)
library(stringr)
library(phonics)
library(shiny)

library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)

##################    FUNCTIONS    ##################  

factorToNum<-  function(factor){as.numeric(as.character(factor))}

phon_nysiis<- function(author){
  #Step1 - Strip accents from Authors
  authors_noaccent <- stri_trans_general(author,"Latin-ASCII")
  firstname <- gsub("([A-Za-z]+).*", "\\1", authors_noaccent)
  #Step2 - Nysiis
  nysiis(firstname, maxCodeLen = 8)   
}

Combinations = function(data){
  ids = combn(unique(data[,1]),2)
  df = data.frame(data[match(ids[1,], data[,1]), ], data[match(ids[2,], data[,1]), ])
  
  #Subset out combinations within the same phonetic
  df = df %>% filter(id!=id.1)
  #df = df %>% filter(phonetic == phonetic.1)
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
  #df$label = as.numeric(df$authorid==df$authorid.1)
  
  df = df %>% select(sigID, author, title, sigID.1, author.1, title.1,
                     dist_author,dist_initials,dist_title,dist_year,dist_coauthor,dist_keyword,dist_journal,dist_institution)
  return(df)
}

#setwd("/Users/saulgarcia/Desktop/Github/WebOfScience1/WebOfScienceShiny/disambiguation-app")
#LOAD Prediction Models
load(file = "data/AuthorForest.rda")
load(file = "data/AuthorSVM.rda")
load(file = "data/AuthorGLM.rda")
load(file = "data/AuthorCTree.rda")
load(file = "data/AuthorCART.rda")
load(file = "data/AuthorNB.rda")

#############   Connect and Read from DB  ##################
# drv<- dbDriver("MySQL")
# pw<- {"dmkm1234"}
# ucscDb <- dbConnect( MySQL(), dbname="dmkm_articles",
#                      host= "127.0.0.1", port=8889,
#                      user="root", password=pw 
# ) #3306
# rm(pw)
# 
# signature<- dbReadTable(ucscDb, "authors_signature")
# signature <- signature[1:100000,]

#Dataset for Shiny's limited space & preprocess to have as from SQL
signature <- read.csv("data/signature.csv",stringsAsFactors = FALSE)
signature <- signature[1:20000,2:12]
signature$sigID <- as.character(signature$sigID) 
signature$d <- as.numeric(signature$d)

###################  USER  INTERFACE   ######################
ui <- fluidPage(
  
  # Application title
  titlePanel("Disambiguate Your Author"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("author",
                  "Author:",
                  c("All",
                    sort(unique(as.character(signature$author))))),
      p("Ex: A'Hearn MF"),
      
      selectInput("title",
                  "Title:",
                  c("All",
                    sort(unique(as.character(signature$title))))),
      p("Ex: Ultraviolet and visible photometry of asteroid (21) Lutetia using the Hubble Space Telescope"),
      
      helpText("Note: The first table portrays the other articles written by this specific author. The second table displays all authors which potentially could be the same author.", 
               "Ignore the initial error. If both tables are empty, then the author did not write the chosen article."),
     
      submitButton("Update View")
    ),
    
    # Show a summary of the dataset and an HTML table with the
    mainPanel(
      h4("Phonetic"),
      verbatimTextOutput("phonetic"),
      
      h4("Other publications by the author"),
      tableOutput("predictions"),
      
      h4("Observations"),
      p("The following observations are all the Authors which have the same phonetic as the input"),
      tableOutput("view")
    )
  )
)

# author = "Ben Ouezdou F"
# title = "From Force Control and Sensory-Motor Informations to Mass Discrimination"
###################  SERVER   ######################
server <- function(input, output) {

  #Get the phonetics
  authorPhon<- reactive({
    phon_nysiis(input$author)
  })

   # Filters the Dataset
  datasetInput <- reactive({
    data = subset(signature, phonetic == authorPhon())
    data = Combinations(data)
    data = data %>% filter(author == input$author & title == input$title)
    data = features(data)
    
    
  })
  
  #Predict other titles published by the same author
  PredictionInput <- reactive({
    prediction = cbind(datasetInput(), 
                       "label" = as.numeric(as.character(predict(AuthorCART, newdata = datasetInput(), type="class")))
                       )
    prediction = prediction %>% filter(label == 1) %>% select("Author"=author.1, "Title" =title.1)

  }) 

  # Print the phonetic of the input
  output$phonetic <- renderPrint({
    author <- authorPhon()
    print(author)
  })
  
  #Display the other titles published by the same author
  output$predictions <- renderTable({
    PredictionInput()
  })
  
  #Show the authors filtered table
  output$view <- renderTable({
     datasetInput() %>% select("Author" = author.1, "Title" = title.1, "Authors Distance"=dist_author,
                               "Initial's Distance" = dist_initials,
                               "Title Distance" = dist_title,
                               "Coauthors Distance" = dist_coauthor,
                               "Keywords Distance" = dist_keyword,
                               "Institution Distance" = dist_institution)
  })

}


shinyApp(ui = ui, server = server)