# Just About Right Scores for Consumer Research


# Libraries

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lazyeval)




# Constants        





# UI Function

myHeader <- dashboardHeader(disable=TRUE)

mySidebar <- dashboardSidebar(disable=TRUE)

myBody <- dashboardBody(
        
        useShinyjs(),
        
        column(width=9,
               
               # dataTableOutput("test"),
               plotOutput("myPlot")
               
               ),
        
        
        
        
        column(width=3,
               
               # checkboxInput(inputId = "myData", label = "Use My Data", value = FALSE),
               a(id="linkMyData","Use My Data"), tags$br(),

               shinyjs::hidden(
                       
                       div(id="divRequiredInputs",
                           
                           # Start with Random Data, Update in Server if Data File Selected
                           
                           tags$br(),
                           fileInput(inputId = "myFile", label = NULL, accept = c(".csv",".xlsx")),
                              
                           selectInput(inputId = "mySample", label = "Sample Column", choices = c("Sample"), selected = "Sample"),
                           selectInput(inputId = "mySampleSelect", label = "Sample Select", choices = c("A","B","C","D"), selected = "A"),
                           selectInput(inputId = "myHedonic", label = "Liking Column", choices = c("Overall.Liking","Flavor.Liking","Texture.Liking"), selected = "Overall.Liking"),
                           selectInput(inputId = "myJARs", label = "JAR Columns", choices = c("Sweet.JAR","Salt.JAR","Bitter.JAR","Sour.JAR"), selected = c("Sweet.JAR","Salt.JAR","Bitter.JAR","Sour.JAR"), multiple = TRUE)
                           
                       )
               ),
               
               tags$br(),
               a(id="linkGraphOptions","Options"), tags$br(),
               
               shinyjs::hidden(
               
               div(id="divGraphOptions",
                   
                   checkboxInput(inputId = "myLabel", label = "Label", value = TRUE),
                   checkboxInput(inputId = "myCI", label = "Confidence Intervals", value = FALSE),
                   checkboxInput(inputId = "myGrid", label = "Grid", value=FALSE),
                   sliderInput(inputId = "mySpace", label = "Space Between Plots", min = 10, max =100, value = 40),
                   sliderInput(inputId = "myJARRef", label = "JAR Benchmark", min=0, max = 100, value = 75),
                   selectInput(inputId = "myPenalty", label = "Penalty Analysis", choices = c("None","Penalty","Weighted Penalty"), selected = "None")
     
                   )
               )
        )
               

        
        
        
)


dashboardPage(myHeader, mySidebar, myBody)


