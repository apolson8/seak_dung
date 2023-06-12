#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Southeast Dungeness Crab Harvest "),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("harv", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      dataTableOutput("contents")
    )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  output$contents <-renderDataTable({
    file <- input$harv
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
