library(shiny)
library(shinymanager)
library(data.table)
library(readxl)




ui <- fluidPage(
  titlePanel("POSICAO DE COTISTAS"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Digite o codigo de cotista abaixo:
        "),
      textInput("cod", "Codigo:", ""),
      
      actionButton("sho", 
                   label = "OK",
                   icon("sync")),
      
      
      
      br(),
      br(),
      
      #checkboxInput("log", "Plot y axis on log scale",
      #value = FALSE),
      
      #checkboxInput("adjust",
      #"Adjust prices for inflation", value = FALSE)
    ),
    
    mainPanel(dataTableOutput("plot"))
  ),
)





server <- function(input, output) {
  
  dataInput1 <- reactive({
    df <- read_xlsx("vgrcot.xlsx")
    new_df <- subset(df, df$`Conta do cotista`==input$cod)
  })
   
  observeEvent(input$sho, {
    output$plot <- renderDataTable({
      
      dataInput1()
    })
  })
  
}


shinyApp(ui, server)


