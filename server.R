
library(shiny)

pp <- function (st, vt){
  
  if (st == ""){
    
    p <- "Select from information section and date"
    
  }else{
    
    p <- paste(st,vt,sep="--")
  }
  
  return(p)
} 


shinyServer(function(input, output) {
  output$sx <- renderPrint({pp(input$slt,input$date)})
  
})
