library(shiny)
library(quantmod)
library(PerformanceAnalytics)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("CORRELAÇÃO ENTRE ATIVOS"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Digite os ativos:

        "),
      textInput("symb1", "Ativo 1", "PETR4.SA"),
      textInput("symb2", "Ativo 2", "VALE3.SA"),
      textInput("symb3", "Ativo 3", "WEGE3.SA"),
      textInput("symb4", "Ativo 4", "BBDC4.SA"),
      textInput("symb5", "Ativo 5", "USIM5.SA"),
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2019-01-01",
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      #checkboxInput("log", "Plot y axis on log scale",
      #value = FALSE),
      
      #checkboxInput("adjust",
      #"Adjust prices for inflation", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server logic
server <- function(input, output, ret1, ret2, ret3, ret4, ret5, rets) {
  
  dataInput1 <- reactive({
    getSymbols(input$symb1, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })
  
  dataInput2 <- reactive({
    getSymbols(input$symb2, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })
  
  dataInput3 <- reactive({
    getSymbols(input$symb3, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })
  
  dataInput4 <- reactive({
    getSymbols(input$symb4, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })
  
  dataInput5 <- reactive({
    getSymbols(input$symb5, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)[,4]
  })
  
  
  
  
  ret1 <- reactive({
    na.omit(ROC(dataInput1()))
    
  })
  
  ret2 <- reactive({
    na.omit(ROC(dataInput2()))
    
  })
  
  ret3 <- reactive({
    na.omit(ROC(dataInput3()))
    
  })
  
  ret4 <- reactive({
    na.omit(ROC(dataInput4()))
    
  })
  
  ret5 <- reactive({
    na.omit(ROC(dataInput5()))
    
  })
  
  rets <- reactive({
    merge(ret1(), ret2(), ret3(), ret4(), ret5())
  })
  
  
  output$plot <- renderPlot({
    
    chart.Correlation(rets())
  })
  
}

# Run the app
shinyApp(ui, server)

