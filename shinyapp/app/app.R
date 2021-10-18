
library(shiny)

# Load all R necessary libraries
library(plotly)
library(ggplot2)
library(DT)
library(tidyverse)
library(tidyr)
library(dplyr)
library(crosstalk)
library(dygraphs)
library(forecast)
library(fpp3)
library(fable)
library(fable.prophet)
library(tsibble)
library(tsibbledata)
library(ggfortify)
library(magrittr)
library(lubridate)
library(tsibble)
library(data.table)
library(readxl)
library(sarbcurrent)
library(strucchange)
library(changepoint)
library(lubridate)
library(tseries)
library(dygraphs)
library(quantmod)
library(corrplot)
library(sarbcurrent)
library(strucchange)
library(changepoint)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(patchwork)

# =====Loading datasets =======================================================

# Extract and load data from Yahoo!Finances

CORN <- getSymbols("CORN", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

#CN21.CBT <- getSymbols("CN21.CBT", auto.assign = FALSE,
#                    from = "1994-01-01", end = Sys.Date())

#CU21.CBT <- getSymbols("CU21.CBT", auto.assign = FALSE,
#                    from = "1994-01-01", end = Sys.Date())

BZ_F <- getSymbols("BZ=F", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

CL_F <- getSymbols("CL=F", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

XC_F <- getSymbols("XC=F", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

ZW_F <- getSymbols("ZW=F", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

ZS_F <- getSymbols("ZS=F", auto.assign = FALSE,
                   from = "2010-06-09", end = Sys.Date())

USDBRL_X <- getSymbols("USDBRL=X", auto.assign = FALSE,
                       from = "2010-06-09", end = Sys.Date()) # Real effective exchange rate Brazil/ Dollar


dataset <- cbind(
  
  CORN,
  BZ_F,
  CL_F,
  XC_F,
  ZW_F,
  ZS_F,
  USDBRL_X
  
) %>% as.data.table() %>% as_tsibble()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Time series forecasting exercise"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          varSelectInput(
            
            inputId = "selectedvar",
            label = "Choose a column from the data:",
            data = dataset, 
            selected = "CORN.Close"
            
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot <- renderPlot({
        # generate bins based on input$bins from ui.R
       dataset %>%
        ggplot(aes(x = index,
                   y = !!input$selectedvar))+
        geom_line()
    })
    
    output$Plot <- renderPlot({
      
      dataset <- dataset %>%
        fill_gaps() %>%
        fill(!!input$selectedvar, .direction = "down")
      
      model_oos <- dataset %>%
        model(
          NAIVE(!!input$selectedvar)
        )
      
      sim <- model_oos %>%
        generate(h = 30, 
                 times = 5,
                 bootstrap = TRUE)
      # Here we have generated five possible sample paths for the next 30 trading days. The .rep variable provides a new key for the tsibble. The plot below shows the five sample paths along with the historical data.
   
          b <-
               dataset %>%
              filter_index("2021-01-01"~ .) %>%
              ggplot(aes(x = index)) +
              geom_line(aes(y = !!input$selectedvar)) +
              geom_line(aes(y = .sim, colour = as.factor(.rep)),
                        data = sim) +
                ggtitle("Simulated model scenarios for selected variable") +
                guides(colour = "none") 
              
   
          b
        
 })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
