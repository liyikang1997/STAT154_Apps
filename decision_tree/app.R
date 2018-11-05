#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)

col <- colnames(solder)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for decision trees: regression tree/classification tree"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput('response', 'Response variable', colnames(solder), "Opening"),
      radioButtons("type", "Tree type:", c("Classification" = "class", "Regression" = "anova"), selected = character(0)),
      numericInput("minsplit", "Minimum number of splits", min = 1, value = 20),
      numericInput("minbucket", "Minimum number of individuals in any terminal node", min = 1, value = round(20/3)),
      sliderInput("cp", "Complexity parameter", min = 0, max = 1, value = 0.01),
      numericInput("xval", "Number of cross-validations", min = 0, value = 10),
      numericInput("maxdepth", "Maximum depth", min = 0, value = 30),
      hr("Plot:"),
      sliderInput("margin", "Margin of tree plot", min = 0, max = 1, step = 0.01, value = 0.01),
      checkboxInput("all", "Label all nodes")),
    
    mainPanel(
      plotOutput("treeplot")
    )
  )
)


server <- function(input, output){
  
  tree<- reactive({
    f <- as.formula(paste(paste(input$response, "~"),paste(col[!col %in% input$response], collapse = " + ")))
    
    if (length(input$type) == 0){
      rpart(f, data = solder, minsplit = input$minsplit, 
            minbucket = input$minbucket, cp = input$cp, xval = input$xval, maxdepth = input$maxdepth)
    }
    else{
      rpart(f, data = solder, method = input$type, minsplit = input$minsplit, 
            minbucket = input$minbucket, cp = input$cp, xval = input$xval, maxdepth = input$maxdepth)
    }
    })
  
  
  plottree<-  reactive({
    plot(tree(), margin= input$margin)
    text(tree(), all = input$all)
  })
  
  
  output$treeplot <- renderPlot({
    plottree()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



