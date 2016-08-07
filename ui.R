# Just About Right Scores for Consumer Research


# Libraries

        library(shiny)
        library(shinydashboard)
        library(shinyjs)
        library(dplyr)
        library(tidyr)
        library(ggplot2)
        library(lazyeval)
        library(DT)

# Constants        


# UI Function

myHeader <- dashboardHeader(disable=TRUE)
mySidebar <- dashboardSidebar(disable=TRUE)

myBody <- dashboardBody(
        
        useShinyjs(),
        
        fluidRow(
                
                box(width=12, background = "blue",
                    h4(textOutput("titleText"))
                ),
                  
                column(width=9,
                       
                       box(width=12,
                           
                           plotOutput("myPlot"),
                           
                           a(id="linkRecommend", "Recommendations"), tags$br(),
                           
                           shinyjs::hidden(
                                   div(id="divRecommend",
                                       tags$br(),
                                       tags$i("Note: Recommendations Based on Weighted Penalties.  You Decide..."),
                                       tags$br(),
                                       DT::dataTableOutput("dtRecommend"),
                                       tags$br()
                                   )
                           ),
                           
                           a(id="linkShowMyData", "View Data"), tags$br(),
                           
                           shinyjs::hidden(
                                   div(id="divShowMyData",
                                       tags$br(),
                                       DT::dataTableOutput("dataTable")
                                   )   
                           ),
                           downloadLink('downloadPlot', 'Download Plot'), tags$br(),
                           downloadLink('downloadDataTable', 'Download Data')
                       )
                ),
                
                
                column(width=3,
                       
                       # checkboxInput(inputId = "myData", label = "Use My Data", value = FALSE),
                       a(id="linkUseMyData","Use My Data"), tags$br(),
                       
                       shinyjs::hidden(
                               
                               div(id="divUseMyData",
                                   
                                   # Start with Random Data, Update in Server if Data File Selected
                                   tags$br(),
                                   fileInput(inputId = "myFile", label = NULL, accept = c(".csv",".xlsx"))
                               )
                       ),
                       
                       tags$br(),
                       a(id="linkInputs", "Column Inputs"), tags$br(),
                       
                       shinyjs::hidden(
                               
                               div(id="divInputs",
                                   
                                   tags$br(),
                                   selectInput(inputId = "mySample", label = "Sample Column", choices = c("Sample"), selected = "Sample"),
                                   selectInput(inputId = "mySampleSelect", label = "Sample Select", choices = c("A","B","C","D"), selected = "A"),
                                   selectInput(inputId = "myHedonic", label = "Liking Column", choices = c("Overall.Liking","Flavor.Liking","Texture.Liking"), selected = "Overall.Liking"),
                                   selectInput(inputId = "myJARs", label = "JAR Columns", choices = c("Sweet.JAR","Salt.JAR","Bitter.JAR","Sour.JAR"), selected = c("Sweet.JAR","Salt.JAR","Bitter.JAR","Sour.JAR"), multiple = TRUE)
                               )
                       ),
                       
                       tags$br(),
                       a(id="linkValueLabels", "Value Labels"), tags$br(),
                       
                       shinyjs::hidden(
                               
                               div(id="divValueLabels",
                                   
                                   tags$br(),
                                   textInput(inputId = "my2Little", label = "Too Little", value = "1 2"),
                                   textInput(inputId = "myJAR", label = "Just About Right (JAR)", value = "3"),
                                   textInput(inputId = "my2Much", label = "Too Much", value = "4 5")
                               )
                       ),
                       
                       
                       tags$br(),
                       a(id="linkGraphOptions","Graph Options"), tags$br(),
                       
                       shinyjs::hidden(
                               
                               div(id="divGraphOptions",
                                   
                                   tags$br(),
                                   checkboxInput(inputId = "myLabel", label = "Label", value = TRUE),
                                   checkboxInput(inputId = "myCI", label = "Confidence Intervals", value = FALSE),
                                   checkboxInput(inputId = "myGrid", label = "Grid", value=FALSE),
                                   sliderInput(inputId = "mySpace", label = "Space Between Plots", min = 10, max =100, value = 50),
                                   sliderInput(inputId = "myJARRef", label = "JAR Benchmark", min=0, max = 100, value = 75),
                                   selectInput(inputId = "myPenalty", label = "Penalty Analysis", choices = c("None","Penalty","Weighted Penalty"), selected = "None")
                               )
                       )
                )
        )
        
)


dashboardPage(myHeader, mySidebar, myBody)


