library(RMySQL)
library(dplyr)
library(tm)
library(plyr)
library(stringdist)
library(stringi)
library(stringr)
library(shiny)


###################  USER  INTERFACE   ######################
ui <- fluidPage(
  selectInput(inputId = "author", 
              label = "Choose an author", 
              choices = c("Author1","Author2")),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Millimeter-wave generation via frequency multiplication in graphene"),
  tableOutput("disambiguated")
)


###################  SERVER   ######################
server <- function(input, output) {
#   output$hist <- renderPlot({
#     hist(rnorm(input$num), main = input$title)
#   })
}

shinyApp(ui = ui, server = server)