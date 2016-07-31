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


# Constants        
      
myPal <- c("#fdcdac","#cbd5e8","#b3e2cd","white")  # From Color Brewer Diverging #6 (Too Little, Too Much, JAR, Hide)  



# Server Function

server <- function(input, output, session) { 
        
        
# Control Statements
      
        shinyjs::onclick("linkMyData",
                shinyjs::toggle(id="divRequiredInputs", anim = TRUE, animType = "slide")
        )
        
        shinyjs::onclick("linkGraphOptions",
                         shinyjs::toggle(id="divGraphOptions", anim = TRUE, animType = "slide")
        )
        
        observeEvent(input$myFile, {

                req(df1())
                df1 <- df1()
                
                updateSelectInput(session, inputId = "mySample", choices = names(df1), selected = "Sample")
                
        })
        
        observeEvent(input$mySample, {
              
                df1 <- df1()
                
                updateSelectInput(session, inputId = "mySampleSelect", choices = unique(df1[[input$mySample]]))
                updateSelectInput(session, inputId = "myHedonic", choices = names(df1), selected = "Overall.Liking")
                
                myNewJARs <- names(df1)[grep("_JAR|\\.JAR", names(df1))]
                
                updateSelectInput(session, inputId = "myJARs", choices = myNewJARs, selected = myNewJARs)
        })
        

        
# Data
        
        df1 <- reactive({
                
                if(is.null(input$myFile)){
                        
                        set.seed(19370427)
                        
                        df1 <- tbl_df(data.frame(
                                Sample.ID = rep(LETTERS[1:4],25),
                                Overall.Liking = sample(x = c(1,2,3,4,5,5,6,6,6,7,7,7,7,8,8,8,9), size = 1000, replace = TRUE),
                                Sweet.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Salty.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Bitter.JAR = sample(x = c(1,2,2,3,3,3,3,3,3,4,4,5), size = 1000, replace = TRUE),
                                Sour_JAR = sample(x = c(1,2,2,2,2,3,3,3,4,5), size = 1000, replace = TRUE)
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
        
        myJARs <- reactive({
                
                df1 <- df1()
                myJARs <- names(df1)[grep("_JAR|\\.JAR", names(df1))]
               
        })
        
        df2 <- reactive({
                
                df1 <- df1()
                myJARs <- myJARs()
                
                
                names(df1)[which(names(df1) == "Sample.ID")] <- "Sample"  # Force Name of Samples Column
                
                df2 <- gather_(data = df1, key_col = "Attribute", value_col = "Response", gather_cols = myJARs) %>% 
                        mutate(Attribute = gsub("_JAR|\\.JAR","",Attribute))
              
                df2 <- df2 %>% filter(Sample %in% input$mySampleSelect)
                # df2 <- df2[df2[[input$mySample]] %in% input$mySampleSelect, ]
                
                df2
                
        })
        
        df3 <- reactive({
                
                df2 <- df2()
                
                df3 <- df2 %>% 
                        group_by(Sample, Attribute, Response) %>% 
                        summarise(myN = n()) %>%         
                        group_by(Sample, Attribute) %>% 
                        mutate(myTotal = sum(myN),
                               myProportion = myN/myTotal,
                               mySE = sqrt(myProportion*(1-myProportion)/myN),
                               myLCL = pmax(0, round(100*(myProportion - 2*mySE),0)),
                               myUCL = pmin(100, round(100*(myProportion + 2*mySE), 0)),
                               myPercent = round(100*myProportion, 0)
                        )
                
                df3
        })
        
        dfTotal <- reactive({
                
                df2 <- df2()
                
                dfTotal <- df2 %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise(myTotal = n())
                
                dfTotal
                
        })
        
        
        
        # Too Little
        
        dfPlot1 <- reactive({
                
                df2 <- df2()
                dfTotal <- dfTotal()
                
                dfPlot1 <- df2 %>% 
                        filter(Response %in% c(1,2)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise(myN = n()) %>% 
                        left_join(y = dfTotal) %>% 
                        mutate(myProportion = myN/myTotal,
                               mySE = sqrt(myProportion*(1-myProportion)/myN),
                               myLCL = pmax(0, round(100*(myProportion - 2*mySE),0)),
                               myUCL = pmin(100, round(100*(myProportion + 2*mySE), 0)),
                               myPercent = round(100*myProportion, 0)
                        )
                
                dfPlot1
                
        })
                
        # JAR
        
        dfPlot2 <- reactive({
                
                df2 <- df2()
                dfTotal <- dfTotal()
                
                dfPlot2 <- df2 %>% 
                        filter(Response %in% c(3)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise(myN = n()) %>% 
                        left_join(y = dfTotal) %>% 
                        mutate(myProportion = myN/myTotal,
                               mySE = sqrt(myProportion*(1-myProportion)/myN),
                               myLCL = pmax(0, round(100*(myProportion - 2*mySE),0)),
                               myUCL = pmin(100, round(100*(myProportion + 2*mySE), 0)),
                               myPercent = round(100*myProportion, 0)
                        )
                
                dfPlot2
                
        })
        
       
        
        # Too Much
        
        dfPlot3 <- reactive({
                
                df2 <- df2()
                dfTotal <- dfTotal()
                
                dfPlot3 <- df2 %>% 
                        filter(Response %in% c(4,5)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise(myN = n()) %>% 
                        left_join(y = dfTotal) %>% 
                        mutate(myProportion = myN/myTotal,
                               mySE = sqrt(myProportion*(1-myProportion)/myN),
                               myLCL = pmax(0, round(100*(myProportion - 2*mySE),0)),
                               myUCL = pmin(100, round(100*(myProportion + 2*mySE), 0)),
                               myPercent = round(100*myProportion, 0)
                        )
                
        })
        
       
        
        # Calculated Constants (Depend on Data)
        
        myMinTooLittle <- reactive({ -max(dfPlot1()$myPercent) })
        myMaxTooMuch <- reactive({ max(dfPlot3()$myPercent) })
        myMaxJAR <- reactive({ max(dfPlot2()$myPercent) })
        myZeroJAR <- reactive({ myMinTooLittle() - input$mySpace - myMaxJAR() })
        
        # Update dfPlot2 as dfPlotJAR
        
        dfPlotJAR <- reactive({
                
                dfPlot2 <- dfPlot2()
                myZeroJAR <- myZeroJAR()
                
                
                dfPlotJAR <- dfPlot2 %>% 
                        mutate(myLow = myZeroJAR,
                               myHigh = myLow + myPercent,
                               myLCL2 = myLow + myLCL,
                               myUCL2 = myLow + myUCL) %>% 
                        arrange(myPercent) %>% 
                        mutate(Attribute = factor(Attribute, Attribute),
                               Penalty = 0,
                               Weighted.Penalty = 0)
                
        })
        
        dfLike <- reactive({
                
                df2 <- df2()
                
                
                myHedonic <- input$myHedonic
                
                dfLike1 <- df2 %>% 
                        filter(Response %in% c(1,2)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise_(LikingTooLittle=interp(~mean(v), v=as.name(myHedonic)))
                
                
                dfLike2 <- df2 %>% 
                        filter(Response %in% c(3)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise_(LikingJAR=interp(~mean(v), v=as.name(myHedonic)))
                
                
                dfLike3 <- df2 %>% 
                        filter(Response %in% c(4,5)) %>% 
                        group_by(Sample, Attribute) %>% 
                        summarise_(LikingTooMuch=interp(~mean(v), v=as.name(myHedonic)))
                
                dfLike <- dfLike1 %>% left_join(dfLike2) %>% left_join(dfLike3) %>% 
                        # bind_cols(dfLike1, dfLike2, dfLike3) %>% 
                        select(Sample, Attribute, LikingTooLittle, LikingJAR, LikingTooMuch) %>% 
                        mutate(PenaltyTooLittle = LikingTooLittle - LikingJAR,
                               PenaltyTooMuch = LikingTooMuch - LikingJAR ) %>% 
                        select(Sample, Attribute, PenaltyTooLittle, PenaltyTooMuch)
                
                
        })
        
        
        dfPlot2Little <- reactive({
                
                dfPlot1 <- dfPlot1()
                dfLike <- dfLike()
                
                dfPlot2Little <- dfPlot1 %>% 
                        left_join(dfLike %>% select(-PenaltyTooMuch)) %>% 
                        rename(Penalty = PenaltyTooLittle) %>% 
                        mutate(Weighted.Penalty = myPercent*Penalty/100)
                
                dfPlot2Little
                
        })
        
        dfPlot2Much <- reactive({
                
                dfPlot3 <- dfPlot3()
                dfLike <- dfLike()
                
                dfPlot2Much <- dfPlot3 %>% 
                        left_join(dfLike %>% select(-PenaltyTooLittle)) %>% 
                        rename(Penalty = PenaltyTooMuch) %>% 
                        mutate(Weighted.Penalty = myPercent*Penalty/100)
                
                dfPlot2Much
                
        })
        
        
# Plot
        
        output$myPlot <- renderPlot({
                
                validate(
                        need(!is.null(input$mySample), "Select Samples Column..."),
                        need(!is.null(input$mySampleSelect), "Select Sample to Plot..."),
                        need(!is.null(input$myHedonic), "Select Liking Column..."),
                        need(!is.null(input$myJARs), "Select JAR Columns...")
                        
                )
                
                req(input$mySample)
                req(input$mySampleSelect)
                req(input$myHedonic)
                req(input$myJARs)
                
                dfPlot1 <- dfPlot2Little()
                dfPlot2 <- dfPlotJAR()
                dfPlot3 <- dfPlot2Much()
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
                
                
                p1 <- ggplot(data=dfPlot2, aes(x=Attribute, y=myLow, label=round(myPercent,0)))
                
                # JAR Bars
                
                p1 <- p1 + geom_bar(data=dfPlot2, aes(fill=myPal[3]), stat="identity")
                p1 <- p1 + geom_bar(data=dfPlot2, aes(y=myHigh, fill=myPal[4]),stat="identity")
                
                if(myJARRef > 0){
                        p1 <- p1 + geom_hline(yintercept = myZeroJAR + myJARRef, col = "darkgreen", lty=2)
                }
                
                # Too Little Bars
                
                p1 <- p1 + geom_bar(data=dfPlot1, aes(x=Attribute, y=-myPercent, fill=myPal[1]), stat="identity")
                
                
                # Too Much Bars
                
                p1 <- p1 + geom_bar(data=dfPlot3, aes(x=Attribute, y=myPercent, fill=myPal[2]), stat="identity")
                
                
                # Scale
                
                myBreaks <- seq(from=round(myZeroJAR, -1), to = round(myMaxTooMuch + mySpace/2, -1), by = 10)
                
                myLabels1 <- seq(from = round(myZeroJAR, -1), to = round(myZeroJAR + myMaxJAR + mySpace/2, -1), by = 10) - round(myZeroJAR, -1)
                myLabels2 <- seq(from = round(myMinTooLittle - mySpace/2, -1), to = round(myMaxTooMuch + mySpace/2, -1), by = 10)
                myLabels <- c(myLabels1[-length(myLabels1)], myLabels2)
                
                p1 <- p1 + scale_y_continuous(breaks = myBreaks, labels = abs(myLabels))
                
                
                # Grid
                
                p1 <- p1 + theme_minimal()
                p1 <- p1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=17))
                
                if(myGrid == TRUE){
                        p1 <- p1 + geom_hline(yintercept = myBreaks, col=myPal[4], size=0.5)
                }
                
                
                # Confidence Intervals
                
                if(myCI == TRUE){
                        
                        p1 <- p1 + geom_segment(data=dfPlot1, aes(x=Attribute, xend=Attribute, y=-myLCL, yend=-myUCL))
                        p1 <- p1 + geom_segment(data=dfPlot3, aes(x=Attribute, xend=Attribute, y=myLCL, yend=myUCL))
                        p1 <- p1 + geom_segment(data=dfPlot2, aes(x=Attribute, xend=Attribute, y=myLCL2, yend=myUCL2))
                        
                }
                
                # Labels
                
                if(myLabel == TRUE){
                        p1 <- p1 + geom_label(data=dfPlot2, aes(x=Attribute, y=myHigh))
                        p1 <- p1 + geom_label(data=dfPlot1, aes(x=Attribute, y=-myPercent, label=round(myPercent,0)))
                        p1 <- p1 + geom_label(data=dfPlot3, aes(x=Attribute, y=myPercent, label=round(myPercent,0)))
                }
                
                
                if(myPenalty == "Penalty"){
                        p1 <- p1 + geom_text(data=dfPlot1, aes(x=Attribute, y=-myUCL -5, label=round(Penalty, 1), col=Penalty))
                        p1 <- p1 + geom_text(data=dfPlot3, aes(x=Attribute, y=myUCL +5, label=round(Penalty, 1), col=Penalty))
                }
                
                if(myPenalty == "Weighted Penalty"){
                        p1 <- p1 + geom_text(data=dfPlot1, aes(x=Attribute, y=-myUCL -5, label=round(Weighted.Penalty, 1), col=Weighted.Penalty))
                        p1 <- p1 + geom_text(data=dfPlot3, aes(x=Attribute, y=myUCL +5, label=round(Weighted.Penalty, 1), col=Weighted.Penalty))
                }
                
                # Misc
                
                p1 <- p1 + coord_flip()
                p1 <- p1 + xlab("") + ylab("Percent (%)") 
                p1 <- p1 + scale_fill_identity(name="", guide = 'legend', labels = c("JAR","Too Much","Too Little",""))
                p1 <- p1 + scale_color_continuous(low = "red", high = "black", guide = FALSE)
                p1 <- p1 + guides(fill = guide_legend())
                
                p1 <- p1 + geom_hline(yintercept = round(myMinTooLittle - mySpace/2, -1) - 5, col="grey95", size=5)  # Line to Split Graphs
                # p1 <- p1 + geom_hline(yintercept = myLabels1[length(myLabels1)-1] + 5, col="grey50")  # Line to Split Graphs
                
                p1
                
                
                
                
                
                
        })
        
        
        output$test <- renderDataTable({df3()})
        
        
        
        
}
        
        