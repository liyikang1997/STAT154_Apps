#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(FactoMineR)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Shiny App for visualizing PCA with clusters"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("nPC", "Number of PCs", min = 2, value =  5),
        checkboxInput('standardize', label = 'standardize', value = TRUE),
        numericInput("xpc", "PC on X-axis", min = 1, value = 1),
        numericInput("ypc", "PC on Y-axis", min = 1, value = 2),
        numericInput('clusters', 'Cluster count', min = 1, value = 3),
        selectInput('distance', 'Distance measure', c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), "euclidean"),
        selectInput('agglomerative', 'Agglomerative method', c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), "complete")),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("IndPlot")
      )
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  hc <- reactive({
    dist = dist(mtcars, method = input$distance)
    h<- hclust(dist, method = input$agglomerative)
    h
  })
  
  data_with_cluster<- reactive({
    cluster<- cutree(hc(), k = input$clusters)
    cbind.data.frame(mtcars, cluster)
  })
  
  pc_df<- reactive({
    pca<- PCA(data_with_cluster()[, -ncol(data_with_cluster())], scale.unit = input$standardize, ncp = input$nPC)
    pc_all<- pca$ind$coord
    pc_df <- data.frame(PC1 = pc_all[ ,input$xpc], PC2 = pc_all[ ,input$ypc], Cluster = as.factor(data_with_cluster()$cluster))
    pc_df
  })
  
  output$IndPlot <- renderPlot({
    ggplot(data = pc_df(), aes(x = PC1, y = PC2, label =rownames(pc_df())))+
      geom_text(aes(color = Cluster))+
      geom_vline(xintercept = 0, color = "gray70")+
      geom_hline(yintercept = 0, color = "gray70")+
      ggtitle("PC plot on PC1 and PC2")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



