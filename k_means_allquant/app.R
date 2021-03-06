library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for K-means Clustering with all quantitative variables"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris), selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1)),
    mainPanel(
      plotOutput('plot'),
      h5("Centers"),
      tableOutput('centertable')
    )
  )
)

server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  
  selectedData <- reactive({
    nums <- unlist(lapply(iris, is.numeric))  
    iris[ , nums]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  
  output$plot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData()[,c(input$xcol,input$ycol)],
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers[, input$xcol],
           clusters()$centers[, input$ycol],
           pch = 4, cex = 4, lwd = 4)
  })
  
  output$centertable <- renderTable({
    center<- clusters()$centers
    center <- cbind.data.frame(Cluster = rownames(center), center)
    center
  })
  
}
shinyApp(ui = ui, server = server)





