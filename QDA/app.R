#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

library(shiny)
library(mvtnorm)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App for Quadratic discriminant analysis with 2 classes"),
  
  fluidRow(
    column(4,
           textInput("mean1",'Enter a vector (comma delimmited) indicating the Mean for class 1', value = '0,0'),
           numericInput("size1", "Size of class 1", min = 1, value = 100),
           textInput("mean2",'Enter a vector (comma delimmited) indicating the Mean for class 2', value = '2,2'),
           numericInput("size2", "Size of class 2", min = 1, value = 100)
    ),
    
    column(4,
           numericInput("varx1",'Enter the variance for x coordinates in class 1', value = 2),
           numericInput("vary1",'Enter the variance for y coordinates in class 1', value = 2),
           numericInput("covxy1",'Enter the covariance for x & y coordinates in class 1', value = 1)
    ),
    column(4,
           numericInput("varx2",'Enter the variance for x coordinates in class 2', value = 1),
           numericInput("vary2",'Enter the variance for y coordinates in class 2', value = 1),
           numericInput("covxy2",'Enter the covariance for x & y coordinates in class 2', value = 0)
    )),
  
  hr(),
  
  fluidRow(
    column(width = 2
    ),
    
    column(width = 8,
           plotOutput("QdaPlot")
    ))
)


server <- function(input, output) {
  
  x<- reactive({
    mean1<- c(as.numeric(unlist(strsplit(input$mean1,','))))
    mean2<- c(as.numeric(unlist(strsplit(input$mean2,','))))
    sigma1<- matrix(c(input$varx1, input$covxy1, input$covxy1, input$vary1), ncol=2)
    sigma2<- matrix(c(input$varx2, input$covxy2, input$covxy2, input$vary2), ncol=2)
    
    X1 = rmvnorm(input$size1, mean = mean1, sigma = sigma1)
    X2 = rmvnorm(input$size2, mean = mean2,  sigma = sigma2)
    X = rbind(X1, X2)
  })
  
  
  labels<- reactive({
    labels = c(rep(1, input$size1), rep(2, input$size2))
  })
  
  table<- reactive({
    table<- as.data.frame(cbind(X = x()[,1], Y = x()[,2], Class = labels()))
  })
  
  boundary<- reactive({
    qda.fit <- qda(Class~. , data = table())
    
    # draw decision boundary
    x <- seq(min(x()[,1])-1,max(x()[,1])+1,0.02)
    y <- seq(min(x()[,2])-1,max(x()[,2])+1,0.02)
    #x <- seq(-6,6,0.02)
    #y <- seq(-6,6,0.02)
    m <- length(x)
    n <- length(y)
    z <- as.matrix(expand.grid(x,y),0)
    colnames(z) = c('X', 'Y')
    
    qda.Ghat <- as.numeric(predict(qda.fit, newdata= as.data.frame(z))$class)
    contour(x, y, matrix(qda.Ghat, m,n), drawlabels=FALSE, add=TRUE, col='green')
  })
  
  
  output$QdaPlot<- renderPlot({
    plot(x()[labels() == 1,], col = 'red', xlab="X-axis", ylab="Y-axix", xlim=c(min(x()[,1])-1, max(x()[,1])+1),  ylim=c(min(x()[,2])-1, max(x()[,2])+1))
    points(x()[labels() == 2,], col = 'blue')
    par(new=TRUE)
    boundary()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

