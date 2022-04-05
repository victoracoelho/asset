# Load packages ----
library(shiny)
library(quantmod)
library(PerformanceAnalytics)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Performance Relativa"),

  sidebarLayout(
    sidebarPanel(
      helpText("Digite um ativo e o benchmark:

        "),
      textInput("symb", "Ativo", "PETR4.SA"),
      textInput("bench", "Benchmark", "^BVSP"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2019-01-01",
                     end = as.character(Sys.Date())),

      br(),
      br(),

      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),

      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),

    mainPanel(plotOutput("plot"))
  )
)

# Server logic
server <- function(input, output, ret1, bench, benchret) {

  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
    

  })
  
  ret1 <- reactive({
    na.omit(ROC(dataInput()))
            
  })
  


  bench <- reactive({
    getSymbols(input$bench, src= "yahoo",
               from=input$dates[1], 
               to=input$dates[2], 
               auto.assign = F)[,4]
    })
  
  
  benchret <-  reactive({
    na.omit(ROC(bench()))
    
    })
  
  

  output$plot <- renderPlot({

    chart.RelativePerformance(ret1(), benchret())
  })

}

# Run the app
shinyApp(ui, server)

