
library(shiny)
# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("OMNews"),
  
  sidebarPanel(
    
    selectInput("slt", label = h4("Information section"), 
                choices = list("Make your choice" = "","Politics" = "politique", "Economy" = "economie",
                               "Society" = "Societe","Job Opportunity" ="emploie", "Sport" ="Sport"), selected = 1),
    dateInput("date", 
              label = h4("Period"), 
              value = "2016-01-01"),
    submitButton("Search")
    
  ),
  mainPanel( 
    h4("For more information please click on the link below"),
    verbatimTextOutput("sx"),
    imageOutput("tx")
    
    
  )
  
  
)
)