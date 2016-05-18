
library(shiny)
library(stringr)
library(dplyr)



getValue <- function (string, date){
  
  if (string == ""){
    
       D <- "Please select information section and date"    
    
  }else{
    
    
    
    
    
    
    
  }
  
  return (D)
}


shinyServer(function(input, output) {
  output$sx <- renderPrint({getValue(input$slt,input$date)})
  
})
