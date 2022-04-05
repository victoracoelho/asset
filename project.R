# Load packages ----
library(shiny)
library(quantmod)
library(PerformanceAnalytics)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("stockVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Digite um ativo:

        Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "SPY"),

      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
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
server <- function(input, output, ret1, bench) {

  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
    

  })
  
  ret1 <- reactive({
    na.omit(ROC(dataInput()))
            
  })
  


  bench <- reactive({getSymbols("^BVSP", from=input$dates[1], to=input$dates[2], periodicity = 'daily', auto.assign = F)[,4]})
  bench <-  reactive({na.omit(ROC(bench))})
  
  

  output$plot <- renderPlot({

    charts.PerformanceSummary(ret1())
  })

}

# Run the app
shinyApp(ui, server)

