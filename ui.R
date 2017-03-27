#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(basicPage(
  
  # Application title
  titlePanel("WordPredictor!"),
  
  # Box for user text phrase input
  sidebarLayout(
     sidebarPanel(
        textInput(inputId = "phrase", value = '"I saw a"', 
     label = "Enter a phrase in quotes to get a prediction of the next word. It may take up to 30-40 seconds, thank you for your patience!"), 
  actionButton("go", "Click to submit phrase")
     ),
    
    # Show predicted phrase ending
    mainPanel(
              
              textOutput(outputId = "endword", inline = TRUE)
  )
       
    )
  )
)
