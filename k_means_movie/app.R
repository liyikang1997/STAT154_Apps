library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for visualizing K-means clustering process"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris), selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1),
      numericInput('seed', 'Random Seed', 42, min = 1),
      numericInput('step', 'Step', 1 , min= 1, max = 20)),
    mainPanel(
      plotOutput('plot'),
      h5("Centers"),
      tableOutput('centertable')
    )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    nums <- unlist(lapply(iris, is.numeric))  
    iris[ , nums]
  })
  
  my_kmeans <- reactive({
    n <- nrow(selectedData())
    set.seed(input$seed)
    init <- sample(1:n, input$clusters)
    centroids <- selectedData()[init, ]
    assignment <- apply(selectedData(), 1, function(obs) {
      distances <- apply(centroids, 1, function(centroid) {
        sum((obs - centroid)^2)
      })
      which.min(distances)
    })
    
    cluster_steps <- list(assignment)
    step_num <- 2
    repeat {
      prev <- assignment
      mat_list <- split(selectedData(), assignment)
      centroids <- t(sapply(mat_list, colMeans))
      assignment <- apply(selectedData(), 1, function(obs) {
        distances <- apply(centroids, 1, function(centroid) {
          sum((obs - centroid)^2)
        })
        which.min(distances)
      })
      if (all(assignment == prev)) {
        break
      }
      cluster_steps[[step_num]] <- assignment
      step_num <- step_num + 1
    }
    return(list(cluster_sizes=as.vector(table(assignment)),
                cluster_means=centroids, 
                clustering_vector=assignment,
                cluster_steps = cluster_steps))
  })
  
  
  output$plot<- renderPlot({
  plot(selectedData()[,input$xcol], selectedData()[,input$ycol], col = my_kmeans()$cluster_steps[[input$step]], pch= 19)
})
  
  output$centertable <- renderTable({
    center<- my_kmeans()$cluster_means
    center <- cbind.data.frame(Cluster = rownames(center), center)
    center
  })
  
}
shinyApp(ui = ui, server = server)





