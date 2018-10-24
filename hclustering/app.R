
# http://gastonsanchez.com/visually-enforced/how-to/2012/10/03/Dendrograms

source('code.R')

library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for Hierarchical Clustering"),
  
  # Sidebar with different widgets
  sidebarLayout(
    sidebarPanel(
      selectInput('distance', 'Distance measure', c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), "euclidean"),
      selectInput('agglomerative', 'Agglomerative method', c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), "complete"),
      numericInput('clusters', 'Cluster count', 3, min = 1)),
    mainPanel(
      plotOutput('plot')
    )
  )
)

server <- function(input, output) {
  
  hc <- reactive({
    dist = dist(mtcars, method = input$distance)
    hclust(dist, method = input$agglomerative)
  })

  output$plot <- renderPlot({
    A2Rplot(hc(), k = input$clusters, boxes = FALSE, col.up = 'gray50')
  })
  
}
shinyApp(ui = ui, server = server)





