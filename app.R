#install.packages("forecast")
#install.packages("pals")
#install.packages("shinyWidgets")
#install.packages("shinyjs)
#library(shinyjs)
library(pals)
library(forecast)
library(dygraphs)
library(shiny)
library(shinyWidgets)
library(network)

#library(rsconnect)


source("data.R")

ui <- fluidPage(
  titlePanel("Timeseries of Covid-19 outbreak in several variables"),

  fluidRow(
    column(width=3,
      pickerInput("countries", "Select countries to display", complete,multiple=TRUE,selected=complete,
       options = list(
         `actions-box` = TRUE,
         `deselect-all-text` = "None...",
         `select-all-text` = "Yeah, all !",
         `none-selected-text` = "none"
       )
      )
    ),
    column(width=1,
           checkboxInput("scale", "log-scale", TRUE),
    ),
    column(width=1,
           checkboxInput("compare", "compare-mode", FALSE),      
    ),
    column(width=3,
           sliderInput(inputId = "slider",
                       label = "Days to forecast",
                       min = 0,
                       max = 365,
                       value =  0,
                       step = 1)
    ),
  
  ),
  fluidRow(
    column(width=3,
           selectInput("col", "Select a variable", originalvars,originalvars[2]),
    ),
    column(width=1,
           checkboxInput("smooth", "smoothed", TRUE),      
    ),
    column(width=3,
           conditionalPanel(
             condition = "input.compare == true",
             selectInput("col1", "Select a variable for plot 2", originalvars,originalvars[4]),
           ),    
    ),
    column(width=1,
           conditionalPanel(
            condition= "input.compare == true",
            checkboxInput("smooth1", "smoothed", TRUE),      
           )
    ),
    column(width=3,
           selectInput("choosecluster", "Select variable to cluster by", names(clusters)),
    ),
    column(width=1,
           checkboxInput("applycluster", "Apply clustering", FALSE),      
    )
  ),
  dygraphOutput("dygraph"),
  conditionalPanel(
    condition = "input.compare == true",
    dygraphOutput("dygraph1")
  ),
  plotOutput("plot")
)

server <- function(input, output, session) {
  observeEvent(input$smooth,{
    if(input$smooth){
      updateSelectInput(
        session, "col", choices=smoothvars
      )
    }
    else{
      updateSelectInput(
        session, "col", choices=originalvars
      )
    }
  })
  
  observeEvent(input$smooth1,{
    if(input$smooth1){
      updateSelectInput(
        session, "col1", choices=smoothvars
      )
    }
    else{
      updateSelectInput(
        session, "col1", choices=originalvars
      )
    }
  })
  
  observe({
    if(!input$applycluster){
      plotcolors=as.vector(polychrome(length(complete)))
    }
    else{
      plotcolors=as.color(unname(clusters[[input$choosecluster]]),0.9)
    }
    colmapping = cbind(1:length(complete),complete)
    selected = subset(colmapping,complete %in% input$countries)
    plotcolors=plotcolors[as.numeric(selected[,1])]
    output$plot <- renderPlot({
      if(length(input$countries)!=0){
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("topleft", legend = input$countries, pch=16, pt.cex=3, cex=1.5, bty='n',
               col = plotcolors,ncol=6)
        mtext("Countries", cex=2)
      }
    })

    if( length(selected[,1])!=length(complete) ) {
      csStyle = "
            .dygraph-legend > span { display: all !important; }
           .dygraph-legend > span.highlight { display: inline !important; }
            }
            "
      alpha=1
    }
    else{
      csStyle = "
           .dygraph-legend > span { display: none !important; }
           .dygraph-legend > span.highlight { display: inline !important; }
            }
            "
      alpha=0.2
      
    }
    predicted <- function(){
        vari = input$col
        timeMulti= TimeSeries[[vari]][,input$countries]
    
        if(input$slider>0 & vari!="R_Value"&vari!="doubling_Times"){
          fc <- lapply(timeMulti, function(timeMulti,arg1){forecast(timeMulti,h=input$slider,level=0.05)})
          
          df <- lapply(fc,summary)
    
          n = c(max(data$RDate)+1,max(data$RDate)+input$slider)
    
          #n = rownames(df[[ min(as.numeric(selected[,1]) )]])
          dfTs = lapply(df, function(df) ts(df[,1],start=min(n),end=max(n)))
          
          dfTsMulti = do.call(cbind,dfTs)
          timeMulti=cbind(timeMulti,dfTsMulti)
        }
        return(timeMulti)
    }
  
    output$dygraph <- renderDygraph({
      if(length(input$countries)!=0){
        values = predicted()
        #maxi=(max(unlist(unname(lapply(values,max))),na.rm=TRUE))
        dygraph(values, main = input$col,group = input$col) %>%
          dyRangeSelector(height = 40,
                          dateWindow = c(min(data$RDate), max(data$RDate)+input$slider+1)) %>%
          #dyAxis("y", label = input$col, valueRange = c(NULL,maxi))  %>%
          dyHighlight(highlightCircleSize = 3,
                      highlightSeriesBackgroundAlpha = alpha,
                      hideOnMouseOut = FALSE)%>%
          dyOptions(logscale=input$scale,maxNumberWidth = 10,colors =plotcolors,strokeWidth=2) %>%
          dyCSS(textConnection(csStyle)) %>%
          dyLegend(show = "onmouseover", hideOnMouseOut = TRUE)
      }
    })

  
      if(input$compare==TRUE){
        predicted1 <- function(){
          vari = input$col1
          timeMulti= TimeSeries[[vari]][,input$countries]
          
          if(input$slider>0 & vari!="R_Value"&vari!="doubling_Times"){
            fc <- lapply(timeMulti, function(timeMulti,arg1){forecast(timeMulti,h=input$slider,level=0.05)})
            
            df <- lapply(fc,summary)
            
            n = c(max(data$RDate)+1,max(data$RDate)+input$slider)
            
            #n = rownames(df[[ min(as.numeric(selected[,1]) )]])
            dfTs = lapply(df, function(df) ts(df[,1],start=min(n),end=max(n)))
            
            dfTsMulti = do.call(cbind,dfTs)
            timeMulti=cbind(timeMulti,dfTsMulti)
          }
          return(timeMulti)
        }

        output$dygraph1 <- renderDygraph({
          if(length(input$countries)!=0){
            values = predicted1()
            #maxi=(max(unlist(unname(lapply(values,max))),na.rm=TRUE))
            dygraph(values, main = input$col1,group = input$col) %>%
              dyRangeSelector(height = 40,
                              dateWindow = c(min(data$RDate), max(data$RDate)+input$slider+1)) %>%
              #dyAxis("y", label = input$col, valueRange = c(NULL,maxi))  %>%
              dyHighlight(highlightCircleSize = 3,
                          highlightSeriesBackgroundAlpha = alpha,
                          hideOnMouseOut = FALSE)%>%
              dyOptions(logscale=input$scale,maxNumberWidth = 10,colors =plotcolors,strokeWidth=2,gridLineColor="lightgrey") %>%
              #dyCSS(textConnection("
              # .dygraph-legend > span { display: none; }
              # .dygraph-legend > span.highlight { display: inline; }
              #  }
          #")) %>%
              dyLegend(show = "onmouseover", hideOnMouseOut = TRUE)
          }
        })
        
      }
  })
}

shinyApp(ui, server)


