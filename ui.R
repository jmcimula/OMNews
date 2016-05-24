library(shinydashboard)
library(shiny)
library(knitr)


dashboardPage(
  dashboardHeader(title = "OMNews Cimula Live"),
  dashboardSidebar(
    
    helpText("OMNews : Webscrapping from Mediacongo.net and the number of followers per articles posted."),
    helpText("You can search here news about : Politics, Economy, Society and Sport"),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search...",icon = shiny::icon("search")),
    sidebarMenu( 
      menuItem("Dashboard", tabName = "dashboard",icon = icon("dashboard")), 
      menuItem("WordCloud", icon = icon("th"), tabName = "rawdata",badgeLabel = "new", badgeColor = "green")
    )
    
  ),
  dashboardBody(
    tabItems( 
      tabItem("dashboard", 
              fluidRow( 
                valueBoxOutput("rate"), 
                valueBoxOutput("count"), 
                valueBoxOutput("users") 
              ), 
              fluidRow( 
                box( 
                  width = 8, status = "info", solidHeader = TRUE, 
                  title = "Latest news in Mediacongo...",
                  dataTableOutput("packageView")
                  #verbatimTextOutput("packageView")
                ), 
                box( 
                  width = 4, status = "info", 
                  title = "Description of the article", 
                  tableOutput("packageTable") 
                ) 
              ) 
      ), 
      tabItem("rawdata", 
              plotOutput("rawtable")
      )
    )  
  )
)

