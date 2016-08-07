# Just About Right Scores for Consumer Research


# Libraries

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lazyeval)
library(readxl)
library(DT)


# Constants        
      
myPal <- c("#fdcdac","#cbd5e8","#b3e2cd","white")  # From Color Brewer Diverging #6 (Too Little, Too Much, JAR, Hide)  


# Functions

fGetValues <- function(x){
        
        out <- gsub(pattern = "[:;,]", replacement = " ", x)
        out <- as.numeric(unlist(strsplit(out, "\\s+")))
        out
        
}


fRecommend <- function(Too.Little, Too.Much, myCutOff){
        
        myRatio <- abs(0.0000001 + Too.Little / Too.Much)
        
        if(Too.Little < myCutOff & myRatio > 1.5){
                myRecommend <- "Increase"
        } else if (Too.Much < myCutOff & myRatio < 0.67){
                myRecommend <- "Decrease"
        } else if (Too.Little < myCutOff & Too.Much < myCutOff) {
                myRecommend <- "Segment or Fix Tonality"
        } else if (Too.Little >= myCutOff & Too.Much >= myCutOff){
                myRecommend <- "No Action"
        } else {
                myRecommend <- "IDK"
        }
        
        return(myRecommend)
}


# Server Function

server <- function(input, output, session) { 
        
# Control Statements
      
        shinyjs::onclick("linkUseMyData",
                shinyjs::toggle(id="divUseMyData", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkRecommend",
                         shinyjs::toggle(id="divRecommend", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkShowMyData",
                         shinyjs::toggle(id="divShowMyData", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkInputs",
                         shinyjs::toggle(id="divInputs", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkValueLabels",
                         shinyjs::toggle(id="divValueLabels", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkGraphOptions",
                         shinyjs::toggle(id="divGraphOptions", anim = TRUE, animType = "slide")
        )
        
        observeEvent(input$myFile, {

                req(df1())
                df1 <- df1()
                
                updateSelectInput(session, inputId = "mySample", choices = names(df1), selected = "Sample")
                
                myNewJARs <- names(df1)[grep("_JAR|\\.JAR", names(df1))]
                
                updateSelectInput(session, inputId = "myJARs", choices = myNewJARs, selected = myNewJARs)
                updateSelectInput(session, inputId = "myHedonic", choices = names(df1), selected = "Overall.Liking")
                updateSelectInput(session, inputId = "mySampleSelect", choices = unique(df1[[input$mySample]]))
                
        })
        
        observeEvent(input$mySample, {

                df1 <- df1()
                updateSelectInput(session, inputId = "mySampleSelect", choices = unique(df1[[input$mySample]]))

        })
        

        
# Data
        
        output$titleText <- renderText({paste("JAR Diagnostics For Sample", input$mySampleSelect)}) 
        
        df1 <- reactive({
                
                if(is.null(input$myFile)){
                        
                        set.seed(19370427)
                        
                        df1 <- tbl_df(data.frame(
                                Sample = rep(LETTERS[1:4],25),
                                Overall.Liking = sample(x = c(1,2,3,4,5,5,6,6,6,7,7,7,7,8,8,8,9), size = 1000, replace = TRUE),
                                Flavor.Liking = sample(x = c(1,2,3,4,5,5,6,6,6,7,7,7,7,8,8,8,9), size = 1000, replace = TRUE),
                                Texture.Liking = sample(x = c(1,2,3,4,5,5,6,6,6,7,7,7,7,8,8,8,9), size = 1000, replace = TRUE),
                                Sweet.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Salt.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Bitter.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Sour.JAR = sample(x = c(1,2,2,2,2,3,3,3,4,5), size = 1000, replace = TRUE)
                        ))
                        
                } else {
                        
                        infile <- input$myFile
                        
                        if(length(grep(".csv", infile$name)) == 1){
                                
                                df1 <- read.csv(infile$datapath, stringsAsFactors = FALSE)
                                
                        } else if (length(grep(".xlsx", infile$name)) == 1) {
                                
                                myNewName <- paste(infile$datapath, ".xlsx", sep="")
                                file.rename(infile$datapath, myNewName)
                                df1 <- read_excel(myNewName, sheet=1)
                                
                        } else {
                                return(NULL)
                        }
                        
                }
               
        })
        
        df2 <- reactive({
               
                df1 <- df1()
                myJARs <- input$myJARs
                 
                req(input$mySample)
                req(input$mySampleSelect)
                req(input$myHedonic)
                req(input$myJARs)

                if(input$mySample != "Sample"){
                        names(df1)[which(names(df1) == input$mySample)] <- "Sample"  # Force Name of Samples Column        
                }
                
                df2 <- gather_(data = df1, key_col = "Attribute", value_col = "Response", gather_cols = myJARs) %>%
                        mutate(Attribute = gsub("_JAR|\\.JAR","", Attribute))
                      
                # Convert and Condense to Standard JAR Values
                
                my2Little <- fGetValues(input$my2Little)
                myJAR <- fGetValues(input$myJAR)
                my2Much <- fGetValues(input$my2Much)
             
                df2$Response <- factor(df2$Response, levels = c(my2Little, myJAR, my2Much))
                myLevels <- c(rep(1,length(my2Little)), rep(3,length(myJAR)), rep(5,length(my2Much)))
                df2$Response <- factor(myLevels[df2$Response])
                
                df2 <- df2 %>%
                        filter(Sample %in% input$mySampleSelect)
                
                df2
                
        })
        
        dfPlot <- reactive({
                
                df2 <- df2()
                
                dfPlot <- df2 %>% group_by(Attribute, Response) %>% 
                        mutate_(Liking = paste0("round(mean(",input$myHedonic,"),3)")) %>%
                        group_by(Attribute, Response, Liking) %>% 
                        summarise(myN = n()) %>% 
                        group_by(Attribute) %>% 
                        mutate(myTotal = sum(myN),
                               myProportion = myN/myTotal,
                               mySE = sqrt(myProportion*(1-myProportion)/myN),
                               myLCL = pmax(0, round(100*(myProportion - 2*mySE),0)),
                               myUCL = pmin(100, round(100*(myProportion + 2*mySE), 0)),
                               myPercent = round(100*myProportion, 0)) %>% 
                        select(-mySE, -myProportion) 
                
                dfPlot2 <- dfPlot %>% 
                        filter(Response == 3) %>% 
                        select(Attribute, JAR.Liking = Liking)
                
                dfPlot <- left_join(dfPlot, dfPlot2) %>% 
                        mutate(Penalty = round(Liking - JAR.Liking,3),
                               Weighted.Penalty = round(myPercent * Penalty / 100, 3))
                
                dfPlot
                
        }) 
        
        
        # Calculated Constants (Depend on Data)
        
        myMinTooLittle <- reactive({as.numeric(ungroup(dfPlot()) %>% filter(Response == 1) %>% summarise(-max(myPercent)))})
        myMaxJAR <- reactive({as.numeric(ungroup(dfPlot()) %>% filter(Response == 3) %>% summarise(max(myPercent)))})
        myMaxTooMuch <- reactive({as.numeric(ungroup(dfPlot()) %>% filter(Response == 5) %>% summarise(max(myPercent)))})
        myZeroJAR <- reactive({ myMinTooLittle() - input$mySpace - myMaxJAR() })
        
        
        # Update dfPlot with Scale Values
        
        dfPlot2 <- reactive({
        
                dfPlot <- dfPlot()
                
                dfPlot2 <- ungroup(dfPlot) %>% 
                        filter(Response == 3) %>% 
                        mutate(myLow = myZeroJAR(),
                               myHigh = myLow + myPercent,
                               myLCL2 = myLow + myLCL,
                               myUCL2 = myLow + myUCL) %>% 
                        arrange(myPercent) %>% 
                        mutate(Attribute = factor(Attribute, Attribute))
                
                dfPlot <- left_join(dfPlot, dfPlot2)
                
                dfPlot
                
        })
        
        
# Plot
        
        myPlot <- reactive({
                
                validate(
                        need(!is.null(input$mySample), "Select Samples Column..."),
                        need(!is.null(input$mySampleSelect), "Select Sample to Plot..."),
                        need(!is.null(input$myHedonic), "Select Liking Column..."),
                        need(!is.null(input$myJARs), "Select JAR Columns...")
                )
       
                req(!is.infinite(myMaxTooMuch()))
                req(!is.infinite(myZeroJAR()))
                
                dfPlot <- dfPlot2()
                
                myMaxJAR <- myMaxJAR()
                myMaxTooMuch <- myMaxTooMuch()
                myMinTooLittle <- myMinTooLittle()
                myZeroJAR <- myZeroJAR()
                
                myJARRef <- input$myJARRef
                mySpace <- input$mySpace
                myGrid <- input$myGrid
                myCI <- input$myCI
                myLabel <- input$myLabel
                myPenalty <- input$myPenalty
                
                
                p1 <- ggplot(data=dfPlot, aes(x=Attribute, y=myLow, label=round(myPercent,0)))
                
                # JAR Bars
                
                p1 <- p1 + geom_bar(data=dfPlot, aes(fill=myPal[3]), stat="identity")
                p1 <- p1 + geom_bar(data=dfPlot, aes(y=myHigh, fill=myPal[4]),stat="identity")
                
                if(myJARRef > 0){
                        p1 <- p1 + geom_hline(yintercept = myZeroJAR + myJARRef, col = "darkgreen", lty=2)
                }
                
                # Too Little Bars
                
                p1 <- p1 + geom_bar(data=dfPlot %>% filter(Response == 1), aes(x=Attribute, y=-myPercent, fill=myPal[1]), stat="identity")
                
                
                # Too Much Bars
                
                p1 <- p1 + geom_bar(data=dfPlot %>% filter(Response == 5), aes(x=Attribute, y=myPercent, fill=myPal[2]), stat="identity")
                
                
                # Scale
                       
                myBreaks <- seq(from=round(myZeroJAR, -1), to = round(myMaxTooMuch + mySpace/2, -1), by = 10)
                
                myLabels1 <- seq(from = round(myZeroJAR, -1), to = round(myZeroJAR + myMaxJAR + mySpace/2, -1), by = 10) - round(myZeroJAR, -1)
                myLabels2 <- seq(from = round(myMinTooLittle - mySpace/2, -1), to = round(myMaxTooMuch + mySpace/2, -1), by = 10)
                myLabels <- c(myLabels1[-length(myLabels1)], myLabels2)
                
                p1 <- p1 + scale_y_continuous(breaks = myBreaks, labels = abs(myLabels))
                        
                # Grid
                
                p1 <- p1 + theme_minimal()
                p1 <- p1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
                
                if(myGrid == TRUE){
                        p1 <- p1 + geom_hline(yintercept = myBreaks, col=myPal[4], size=0.5)
                }
                
                
                # Confidence Intervals
                
                if(myCI == TRUE){
                        
                        p1 <- p1 + geom_segment(data=dfPlot %>% filter(Response == 1), aes(x=Attribute, xend=Attribute, y=-myLCL, yend=-myUCL))
                        p1 <- p1 + geom_segment(data=dfPlot %>% filter(Response == 5), aes(x=Attribute, xend=Attribute, y=myLCL, yend=myUCL))
                        p1 <- p1 + geom_segment(data=dfPlot %>% filter(Response == 3), aes(x=Attribute, xend=Attribute, y=myLCL2, yend=myUCL2))
                        
                }
                
                # Labels
                
                if(myLabel == TRUE){
                        p1 <- p1 + geom_label(data=dfPlot %>% filter(Response == 3), aes(x=Attribute, y=myHigh))
                        p1 <- p1 + geom_label(data=dfPlot %>% filter(Response == 1), aes(x=Attribute, y=-myPercent, label=round(myPercent,0)))
                        p1 <- p1 + geom_label(data=dfPlot %>% filter(Response == 5), aes(x=Attribute, y=myPercent, label=round(myPercent,0)))
                }
                
                
                if(myPenalty == "Penalty"){
                        p1 <- p1 + geom_text(data=dfPlot %>% filter(Response == 1), aes(x=Attribute, y=-myUCL -5, label=round(Penalty, 1), col=Penalty))
                        p1 <- p1 + geom_text(data=dfPlot %>% filter(Response == 5), aes(x=Attribute, y=myUCL +5, label=round(Penalty, 1), col=Penalty))
                }
                
                if(myPenalty == "Weighted Penalty"){
                        p1 <- p1 + geom_text(data=dfPlot %>% filter(Response == 1), aes(x=Attribute, y=-myUCL -5, label=round(Weighted.Penalty, 1), col=Weighted.Penalty))
                        p1 <- p1 + geom_text(data=dfPlot %>% filter(Response == 5), aes(x=Attribute, y=myUCL +5, label=round(Weighted.Penalty, 1), col=Weighted.Penalty))
                }

                # Misc
                
                p1 <- p1 + coord_flip()
                p1 <- p1 + xlab("") + ylab("") 
                p1 <- p1 + scale_fill_identity(name="", guide = 'legend', labels = c("JAR","Too Much","Too Little",""))
                p1 <- p1 + scale_color_continuous(low = "red", high = "black", guide = FALSE)
                p1 <- p1 + guides(fill = guide_legend())
        
                p1 <- p1 + geom_hline(yintercept = round(myMinTooLittle - mySpace/2, -1) - 5, col="grey95", size=3)  # Line to Split Graphs
                p1 <- p1 + theme(text=element_text(size=17))
                # p1 <- p1 + ggtitle(paste("Sample =", input$mySampleSelect))
                p1 <- p1 + ggtitle(" ")
                
                p1
                
        })
        
        
        output$myPlot <- renderPlot({
                myPlot()
        })
                
      
# Download Plot
        
        output$downloadPlot = downloadHandler(
                filename = 'Plot.PNG',
                content = function(file) {
                        device <- function(..., width, height) {
                                grDevices::png(..., width = width, height = height,
                                               res = 300, units = "in")
                        }
                        ggsave(file, plot = myPlot(), device = device)
                })
        
        
        
        

# Share Data with Users
      
        dfData <- reactive({
                
                dfData <- dfPlot()
                dfData$Response <- factor(dfData$Response, levels=c(1,3,5), labels=c("Too Little","JAR","Too Much"))
                dfData <- dfData[, c(1,2,4,5,8,6,7,3,9,10,11)]
                names(dfData) <- c("Attribute","Response","N","Total","Percent","LCL","UCL","Liking","JAR.Liking","Penalty","Weighted.Penalty")
                dfData
                
        })
        
        output$dataTable <- DT::renderDataTable({dfData()}, filter="bottom", options = list(scrollX = TRUE))
        
        output$downloadDataTable <- downloadHandler(
          filename = function() {"Data.csv"},
          content = function(file) {write.csv(dfData(), file)}
        )
        
        
        
# Recommendations
        
        output$dtRecommend <- DT::renderDataTable({
                
                dfPlot <- dfPlot()
                df1 <- df1()
                
                myCutOff <- sd(df1[[input$myHedonic]]) * 4 * 0.03  # Assumes SD ~ 25% of Scale and 3% of Scale is Relevant
                
                dfAttributes <- dfPlot %>% 
                        filter(Response == 3 & myPercent < input$myJARRef) %>% 
                        select(Attribute)
                
                dfRecommend <- dfPlot %>% 
                        select(Attribute, Response, Weighted.Penalty) %>% 
                        filter(Response != 3) %>% 
                        inner_join(dfAttributes) %>% 
                        mutate(Response = factor(Response, levels=c(1,5), labels=c("Too.Little","Too.Much"))) %>% 
                        spread(key = Response, value = Weighted.Penalty) %>% 
                        mutate(Recommendation = mapply(fRecommend, Too.Little, Too.Much, -myCutOff)) %>% 
                        select(-Too.Little, -Too.Much)
                
                dfRecommend
                
        }, options = list(dom = "t"))
        
        
        
        
        
}


        
        