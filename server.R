
library(shiny)
library(stringr)
library(dplyr)
library(knitr)

getmdXRubrique <- function (mdXRubrique){
  if (tolower(mdXRubrique)== "politics" || tolower(mdXRubrique)== "politique"  ){mdXRubrique <- "articles-actualite-1-page-"}
  else if(tolower(mdXRubrique) == "economy" || tolower(mdXRubrique)== "economie" ){mdXRubrique <- "articles-actualite-3-page-"}
  else if(tolower(mdXRubrique) == "sport"){mdXRubrique <- "articles-actualite-8-page-"}
  else if(tolower(mdXRubrique) == "society" || tolower(mdXRubrique)== "societe" ){mdXRubrique <- "articles-actualite-9-page-"}
  else{ mdXRubrique <- "eX" }   
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
      
      #Reference character
      keyPub <- dPData[pDataGrep[i]+2]
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub)) + 1
      
      keyPubContent <- substr(keyPub,SpeCharGetValue[2],SpeCharGetValue[3]-7)
      
      keyPub <- dPData[pDataGrep[i]+5]
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub)) + 1 
      
      keyDatePub <- substr(keyPub,SpeCharGetValue[4],SpeCharGetValue[5]-5)
      keyDatePub <- substr(str_trim(keyDatePub),1,10)
      keyDatePub <- str_replace_all (keyDatePub, '[.]', "-")
      
      #keySentAnalysis <- getSentimentAnalysis(keyLink) #Function to retrieve some sentiment Analysis values
      #keyFollower <- keySentAnalysis$keyNbFollower #Number of followers
      #keyCountComment <- keySentAnalysis$keyNbComment #Number of comment
      
      #dDFConst <- data.frame (keyPubContent,keyDatePub,keyLink,keyFollower,keyCountComment) #Building data frame
      dDFConst <- data.frame (keyPubContent,keyDatePub,keyLink) #Building data frame
      dDTxtAnDFrame <- rbind(dDTxtAnDFrame, dDFConst)
    }#Loop to extract data
    
  }
  #Return data frame 
  return (as.data.frame(dDTxtAnDFrame))
  
}#End function

getRadiOkapi <- function (rdRubrique, NumDay){
  library(stringr)
  keySource <- "http://www.radiookapi.net/"
  NumDay <- as.integer(NumDay)
  
  pLink <- paste(keySource,rdRubrique,sep="")
  dDTxtAnDFrame <- data.frame() 
  for (i in 0 : NumDay){
    #Link parameters
    pLink <- paste(pLink,"?page=",sep="")
    pLink <- paste(pLink,i,sep="")
    
    dPData <- readLines(pLink)
    dPData <-  iconv(dPData,"UTF-8","latin1")
    keyWrd <- "views-field views-field-title" #Reference expression in the data
    pDataGrep <- grep (keyWrd,dPData)#Grepping the referenced data
    
    keyLen <- length(pDataGrep)#Length of referenced data
    
    for (i in 1: keyLen){    
      
      #First line depending to the grep result
      keyPub <- dPData[pDataGrep[i]]
      keyPub <- str_trim(keyPub)#Removing left and right hidden characters   
      
      #Reference character
      SpeCharGetValue <- unlist(gregexpr(pattern = keyWrd, keyPub)) + str_length(keyWrd) + 2
      keyPub <- substr(keyPub,SpeCharGetValue,str_length(keyPub))
      keyPub <- str_trim(keyPub)
      
      #Reference character
      keyWrdHTML <- "a href"
      SpeCharGetValue <- unlist(gregexpr(pattern = keyWrdHTML, keyPub)) + str_length(keyWrdHTML) + 2
      keyPub <- substr(keyPub,SpeCharGetValue,str_length(keyPub))
      
      #Reference character
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub)) 
      keyLink <- paste(keySource,str_trim(substr(keyPub,1,SpeCharGetValue[1]-2)), sep="")#Getting the Link
      
      keyPubContent <- str_trim(substr(keyPub,SpeCharGetValue[1]+1,SpeCharGetValue[2]-4))#Substring to get the keyPubContent
      #Following line in the data
      keyPub <- dPData[pDataGrep[i]+1]
      
      #Reference character
      SpeCharGetValue <- unlist(gregexpr(pattern = '>', keyPub))
      keyDatePub <- str_trim(substr(keyPub,SpeCharGetValue[2]+1,SpeCharGetValue[2]+18))#Substring to get the date of Pub.
      
      dDFConst <- data.frame (keyPubContent,keyDatePub,keyLink)
      dDTxtAnDFrame <- rbind(dDTxtAnDFrame, dDFConst)
    }#Loop to extract data
  }
  #Return the data frame	
  return (as.data.frame(dDTxtAnDFrame))
}#End function




getValue <- function (string, date){
  
  if (string == ""){
    
       D <- "Please select information section and date"    
    
  }else{
    #Test function
    
    D <- getRadiOkapi(string,2)
  }
  
  return (D)
}


shinyServer(function(input, output) {
  output$sx <- renderPrint({getValue(input$slt,input$date)})
  
})
