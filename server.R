library(shinydashboard)
library(shiny)
library(stringr)
library(knitr)
library(dplyr)
library(wordcloud)
library(tm)
require(RColorBrewer)


getmdXRubrique <- function (mdXRubrique){
  if (tolower(mdXRubrique)== "politics" || tolower(mdXRubrique)== "politique"  )
    {mdXRubrique <- "articles-actualite-1-page-"}
  else if(tolower(mdXRubrique) == "economy" || tolower(mdXRubrique)== "economie" )
    {mdXRubrique <- "articles-actualite-3-page-"}
  else if(tolower(mdXRubrique) == "sport")
    {mdXRubrique <- "articles-actualite-8-page-"}
  else if(tolower(mdXRubrique) == "society" || tolower(mdXRubrique)== "societe" )
    {mdXRubrique <- "articles-actualite-9-page-"}
  else{ mdXRubrique <- "articles-actualite-4.html" }
  return(mdXRubrique)
}#End function


getSentimentAnalysis <- function (getWebSiteLink){
  
  keyLink <- readLines(getWebSiteLink)
  keyNbFollower <- keyLink[grep("followers",keyLink)]
  keyNbComment  <- keyLink[grep("followers",keyLink) + 1]
  for (i in 1 : 10){
    regX <- paste('([[:digit:]]{',i,'})',sep="")
    if (str_detect (keyNbFollower, regX) == TRUE){
      
      k <- str_extract (keyNbFollower,regX)
      cvtListToChar <- as.character(unlist(k)) #Convert list to caracter							
    }#End if 
  }#End for	
  keyNbFollower <- as.numeric(cvtListToChar)#Last value keyNbFollower
  
  for (j in 1 : 10){	   
    regX <- paste('([[:digit:]]{',j,'})',sep="")
    if (str_detect (keyNbComment, regX) == TRUE){
      
      k <- str_extract (keyNbComment,regX)			
      cvtListToChar <- as.character(unlist(k)) #Convert list to caracter						   
    }#End if 
  }#End for
  keyNbComment <- as.numeric(cvtListToChar)#Last value keyNbComment
  
  dDXReturn <- data.frame (keyNbFollower = keyNbFollower, keyNbComment = keyNbComment)
  dDXReturn <- as.data.frame(dDXReturn)
  
  return (dDXReturn)	
}#End function


getMediaCongo <- function (mdXRubrique, NumDay){
  
  keySource <- "http://mediacongo.net/"    
  mdXRubrique <- getmdXRubrique(mdXRubrique)
  NumDay <- as.integer(NumDay)
  dDTxtAnDFrame <- data.frame()
  for (i in 1 : NumDay){
    
    dPData <- paste(keySource,mdXRubrique,sep="")
    dPData <- paste(dPData,i,sep="")				
    dPData <- paste(dPData, ".html",sep="")				
    dPData <- readLines(dPData)#Web Scrapping				
    dPData <-  iconv(dPData,"UTF-8","latin1")
    keyWrd <- "article_other_item" #Reference expression in the data
    pDataGrep <- grep (keyWrd,dPData)#Grepping the referenced data								
    keyLen <- length(pDataGrep)#Length of referenced data
    #start
    for (i in 1:keyLen){
      
      #First line depending to the grep result
      keyPub <- dPData[pDataGrep[i]+1]
      keyPub <- str_trim(keyPub)#Removing left and right hidden characters
      
      keyLink <- substr(keyPub,10,str_length(keyPub)-2)
      keyLink <- paste(keySource,keyLink,sep="")
      Link <- keyLink
      ID <- substr(keyLink,str_length(keyLink)-9,str_length(keyLink)-5)
      
      #Reference character
      keyPub <- dPData[pDataGrep[i]+2]
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub)) + 1
      
      keyPubContent <- substr(keyPub,SpeCharGetValue[2],SpeCharGetValue[3]-7)
      
      keyPub <- dPData[pDataGrep[i]+5]
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub)) + 1 
      
      keyDatePub <- substr(keyPub,SpeCharGetValue[4],SpeCharGetValue[5]-5)
      keyDatePub <- substr(str_trim(keyDatePub),1,10)
      keyDatePub <- str_replace_all (keyDatePub, '[.]', "-")
      
      keySentAnalysis  <- getSentimentAnalysis(Link) #Function to retrieve some sentiment Analysis values
      Follower      <- keySentAnalysis$keyNbFollower #Number of followers
      CountComment  <- keySentAnalysis$keyNbComment #Number of comment
      
      #dDFConst <- data.frame (keyPubContent,keyDatePub,keyLink,keyFollower,keyCountComment) #Building data frame
      dDFConst <- data.frame (keyPubContent,keyDatePub,ID,Link,Follower,CountComment) #Building data frame
      dDTxtAnDFrame <- rbind(dDTxtAnDFrame, dDFConst)
    }#Loop to extract data
    
  }
  #Return data frame
  dDTxtAnDFrame <- as.data.frame(dDTxtAnDFrame)
  #write.csv(dDTxtAnDFrame$keyPubContent, "tmpdData.csv",row.names=F)
  return (dDTxtAnDFrame)
  
}#End function

shinyServer(function(input, output) {
  
  output$rate <- renderValueBox({
    valueBox( 
      value = summarise(getMediaCongo(input$searchText,1), total=n()), 
      subtitle = "Total of Articles", 
      icon = icon("area-chart")
    ) 
  }) 
 
  output$count <- renderValueBox({ 
    valueBox( 
      value = summarise(getMediaCongo(input$searchText,1), sum(Follower)), 
      subtitle = "Total of Followers", 
      icon = icon("download") 
    ) 
  }) 
  
  output$users <- renderValueBox({ 
    valueBox( 
      summarise(getMediaCongo(input$searchText,1), sum(CountComment)), 
      "Total of Comments", 
      icon = icon("users") 
    ) 
  }) 
  
  output$rawtable <- renderPlot({
    
    #Importing the temporary file
    #dDTxtAnDFrame <- read.csv("tmpdData.csv")
    dDTxtAnDFrame <- getMediaCongo(input$searchText,1)[, 1]
    write.csv(dDTxtAnDFrame, "tmpdData.csv",row.names=F)
    dDTxtAnDFrame <- read.csv("tmpdData.csv")
    dDTxtAnDFrame <- Corpus(VectorSource(dDTxtAnDFrame)) #Corpus
    dDTxtAnDFrame <- tm_map(dDTxtAnDFrame, removeNumbers) #Removing numbers
    dDTxtAnDFrame <- tm_map(dDTxtAnDFrame, removePunctuation) #Removing punctuation
    #Removing french articles 
    dDTxtAnDFrame <- tm_map(dDTxtAnDFrame, stripWhitespace)
    dDataMatrix <- TermDocumentMatrix(dDTxtAnDFrame)
    dMFrame <- as.matrix(dDataMatrix)
    dMSorted <- sort(rowSums(dMFrame),decreasing=TRUE)
    d <- data.frame(word = names(dMSorted),freq=dMSorted)
    wordcloud(
      words = d$word, freq = d$freq, scale = c(4, 0.2),
      min.freq = 1,max.words = Inf,random.order = FALSE,
      rot.per = 0.35,colors = brewer.pal(8, "Dark2")
    )
    
    
  })
  
  output$packageView <- renderDataTable({
    
    getMediaCongo(input$searchText,1)[, c(1,2,3,5,6)]
  
    })
  
  output$packageTable <- renderTable({ 
    
        print(select(getMediaCongo(input$searchText,1),"ID" = ID, Link))
    
  })
  
})
