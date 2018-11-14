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
  fluidRow(
    column(6,
           selectInput('response', 'Response variable', colnames(solder), "Opening"),
           radioButtons("type", "Tree type:", c("Classification" = "class", "Regression" = "anova"), selected = character(0)),
           numericInput("minsplit", "Minimum number of obs in a node for a split", min = 1, value = 20),
           numericInput("minbucket", "Minimum number of obs in any terminal node", min = 1, value = round(20/3))
    ),
    column(6,
           sliderInput("cp", "Complexity parameter", min = 0, max = 1, value = 0.01),
           numericInput("xval", "Number of cross-validations", min = 0, value = 10),
           numericInput("maxdepth", "Maximum depth", min = 0, value = 30)
    )),
  
  hr(),

  fluidRow(
    column(width = 12,
           plotOutput("treeplot")
    )),
  
  fluidRow(
    column(6,
         sliderInput("margin", "Margin of tree plot", min = 0, max = 1, step = 0.01, value = 0.05),
         sliderInput("branch", "Branch of tree plot", min = 0, max = 1, step = 0.1, value = 1),
         checkboxInput("use.n", "Displays the number of observations of each class"),
         checkboxInput("all", "Label all nodes")
         ),
    
    column(6,
         checkboxInput("fancy", "Shows internal nodes as ellipses, terminal nodes as rectangles"),
         sliderInput("fwidth", "Width of the ellipses and rectangles", min = 0, max = 10, step = 1, value = 5),
         sliderInput("fheight", "Height of the ellipses and rectangles", min = 0, max = 10, step = 1, value = 1),
         sliderInput("minbranch", "The minimum height between levels", min = 0, max = 50, step = 1, value = 10))
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
    plot(tree(), margin= input$margin, branch = input$branch, minbranch = input$minbranch)
    text(tree(), fancy = input$fancy, use.n = input$use.n, all = input$all,
         fwidth = input$fwidth, fheight = input$fheight)
  })
  
  
  output$treeplot <- renderPlot({
    plottree()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



